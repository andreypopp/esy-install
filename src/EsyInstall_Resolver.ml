LoudRejection.install ()

module P = PromiseSupport
module StringSet = Set.Make(String)
module StringMap = Map.Make(String)
module IntMap = Map.Make(struct type t = int let compare = compare end)

module RequestMap = Map.Make(
  struct
    let compare = Npm.PackageArg.compare
    type t = NpmTypes.Request.t
  end)

module Universe = struct
  open NpmTypes

  let empty = StringMap.empty

  let lookup_package pkg univ =
    let res = try
        Some (StringMap.find pkg univ)
      with Not_found ->
        None
    in Option.or_default StringMap.empty res

  (** A mapping from a package name to a list of versions with manifests *)
  type t = Manifest.t StringMap.t StringMap.t
end

let build_universe req =
  let module Let_syntax = P.Let_syntax in

  let resolving = Js.Dict.empty () in
  let univ = ref Universe.empty in

  let resolving_key req =
    let open NpmTypes.Request in
    match req with
    | Registry (_,name,_) -> name
    | Local (_,_,Directory path) | Local (_,_,File path) | Remote (_,_,path) ->
      path
    | Git (_,_,GitHub { github_username; github_reponame; github_committish }) ->
      github_username ^ "/" ^ github_reponame ^ "#" ^ github_committish
    | Git (_,_,GitRepo { git_url; git_committish }) ->
      git_url ^ "#" ^ git_committish
  in

  let rec resolve_task req =
    let%bind packument = Pacote.packument (NpmPackageArg.to_string req) in
    match packument with
    | None ->
      Js.log "OOPS";
      Js.log (NpmPackageArg.to_string req);
      P.resolve ()
    | Some { name; versions } ->
      let pkg_univ = ref (Universe.lookup_package name !univ) in
      let tasks = List.map
          (fun ({ NpmTypes.Manifest. dependencies } as manifest) ->
             let version = NpmVersion.to_string manifest.version in
             pkg_univ := StringMap.add version manifest !pkg_univ;
             List.map resolve dependencies)
          versions
      in
      univ := StringMap.add name !pkg_univ !univ;
      let tasks = List.flatten tasks in
      let tasks = Array.of_list tasks in
      let%bind _ = Js.Promise.all tasks in
      P.resolve ()

  and resolve req =
    let key = resolving_key req in
    match Js.Dict.get resolving key with
    | None ->
      let task = resolve_task req in
      Js.Dict.set resolving key task;
      task
    | Some task ->
      task
  in

  let%bind () = resolve req in
  P.resolve !univ

module CudfEncoding = struct

  module VersionMap = struct
    module Set = Set.Make(NpmVersion)
    module Map = Map.Make(NpmVersion)

    type t = {
      versions : (int Map.t * Set.t) StringMap.t;
      rev_versions : NpmVersion.t IntMap.t StringMap.t;
    }

    let empty = {
      versions = StringMap.empty;
      rev_versions = StringMap.empty;
    }

    let for_package name v =
      let lookup_default k m d =
        try StringMap.find k m with Not_found -> d
      in (
        lookup_default name v.versions (Map.empty, Set.empty),
        lookup_default name v.rev_versions IntMap.empty
      )

    let update_for_package name v (versions, rev_versions) = {
      versions = StringMap.add name versions v.versions;
      rev_versions = StringMap.add name rev_versions v.rev_versions;
    }

  end

  type context = {
    packages : (Cudf.package * NpmTypes.Manifest.t) list;
    versions : VersionMap.t;
  }

  let encode_package (manifest : NpmTypes.Manifest.t) (context : context) =
    let name = manifest.name in
    let ((versions_map, versions_set), rev_versions) = VersionMap.for_package name context.versions in
    let max_version =
      try let max_key, _ = IntMap.max_binding rev_versions in max_key
      with Not_found -> 0
    in
    let version = max_version + 1 in
    let versions_map = VersionMap.Map.add manifest.version version versions_map in
    let versions_set = VersionMap.Set.add manifest.version versions_set in
    let rev_versions = IntMap.add version manifest.version rev_versions in
    let cudf_package = {
      Cudf.
      default_package with
      package = name;
      version = version;
      installed = false;
      pkg_extra = [("npm-version", `String (NpmVersion.to_string manifest.version))]
    } in
    let versions = VersionMap.update_for_package
        name context.versions
        ((versions_map, versions_set), rev_versions)
    in
    {
      packages = (cudf_package, manifest)::context.packages;
      versions;
    }

  let encode_depends context (pkg, manifest) =
    let encode_dep (dep : NpmTypes.Request.t) =
      let open NpmTypes in
      match dep with
      | Request.Registry (_,name,NpmVersionConstraint.Exact npm_version) ->
        let ((map, _), _) = VersionMap.for_package name context.versions in
        let version = VersionMap.Map.find npm_version map in
        [[name, Some (`Eq, version)]]
      | Request.Registry (_,_,NpmVersionConstraint.Tag _) ->
        []
      | Request.Registry (_,name,NpmVersionConstraint.Range items) ->
        let items = NpmVersionConstraint.to_cnf items in
        let ((map, set), _) = VersionMap.for_package name context.versions in
        let min_version gset = 
          if VersionMap.Set.is_empty gset then
            None
          else
            let v = VersionMap.Set.min_elt gset in
            Some (VersionMap.Map.find v map)
        in
        let max_version gset =
          if VersionMap.Set.is_empty gset then
            None
          else
            let v = VersionMap.Set.max_elt gset in
            Some (VersionMap.Map.find v map)
        in
        let encode_rel (rel : NpmVersionConstraint.Relation.t) =
          match rel with
          | NpmVersionConstraint.Relation.LT v ->
            let (_, exists, gset) = VersionMap.Set.split v set in
            if not exists then (
              let v = min_version gset in
              (name, Option.map v (fun v -> (`Lt, v)))
            ) else
              let v = VersionMap.Map.find v map in
              (name, Some (`Lt, v))
          | NpmVersionConstraint.Relation.LTE v ->
            let (_, exists, gset) = VersionMap.Set.split v set in
            if not exists then
              let v = min_version gset in
              (name, Option.map v (fun v -> (`Lt, v)))
            else
              let v = VersionMap.Map.find v map in
              (name, Some (`Leq, v))
          | NpmVersionConstraint.Relation.GT v ->
            let (lset, exists, _) = VersionMap.Set.split v set in
            if not exists then
              let v = max_version lset in
              (name, Option.map v (fun v -> (`Gt, v)))
            else
              let v = VersionMap.Map.find v map in
              (name, Some (`Gt, v))
          | NpmVersionConstraint.Relation.GTE v ->
            let (lset, exists, _) = VersionMap.Set.split v set in
            if not exists then
              let v = max_version lset in
              (name, Option.map v (fun v -> (`Gt, v)))
            else
              let v = VersionMap.Map.find v map in
              (name, Some (`Geq, v))
          | NpmVersionConstraint.Relation.EQ v ->
            let (_, exists, _) = VersionMap.Set.split v set in
            let version = if exists then VersionMap.Map.find v map else 10000 in
            (name, Some (`Eq, version))
          | NpmVersionConstraint.Relation.NEQ v ->
            let (_, exists, _) = VersionMap.Set.split v set in
            let version = if exists then VersionMap.Map.find v map else 10000 in
            (name, Some (`Neq, version))
        in
        List.map (List.map encode_rel) items
      | Request.Local _ -> []
      | Request.Remote _ -> []
      | Request.Git _ -> []
    in

    let depends =
      manifest.NpmTypes.Manifest.dependencies
      |> List.map encode_dep 
      |> List.flatten
      |> List.filter (fun item -> item <> [])
    in

    let npm_dependencies =
      let open NpmTypes in
      manifest.NpmTypes.Manifest.dependencies
      |> List.map
        (function
          | Request.Registry (_,name,c) ->
            name ^ "@{" ^ NpmVersionConstraint.to_string c ^ "}"
          | Request.Local (raw,_,_) -> raw
          | Request.Remote (raw,_,_) -> raw
          | Request.Git (raw,_,_) -> raw)
      |> String.concat ", "
    in

    let pkg = Cudf.(
        {
          pkg with
          depends;
          pkg_extra = ("npm-dependencies", `String npm_dependencies)::pkg.pkg_extra;
        }
      )
    in (pkg, manifest)

  let encode_universe (univ : Universe.t) =

    let context = {
      packages = [];
      versions = VersionMap.empty;
    } in

    let context =
      StringMap.fold
        (fun _name versions context ->
           StringMap.fold
             (fun _version -> encode_package)
             versions context)
        univ context
    in

    let context =
      {
        context with
        packages = List.map (encode_depends context) context.packages;
      }
    in

    let univ = Cudf.empty_universe () in
    List.iter (fun (pkg, _) -> Cudf.add_package univ pkg) context.packages;
    univ

end

let () =

  let main =
    let module Let_syntax = P.Let_syntax in
    let req = NpmPackageArg.of_string_exn "./pkg" in
    let%bind univ = build_universe req in
    let cudf_univ = CudfEncoding.encode_universe univ in
    Cudf_printer.pp_universe stdout cudf_univ;
    P.return ()
  in

  let v = NpmVersion.of_string_exn "1.1.1" in
  Js.log (NpmVersion.major v);

  ignore (N.Main.run main)
