module StringSet = Set.Make(String)
module StringMap = Map.Make(String)
module IntMap = Map.Make(struct type t = int let compare = compare end)

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
  packages : (Cudf.package * NpmManifest.t) list;
  versions : VersionMap.t;
}

let encode_package (manifest : NpmManifest.t) (context : context) =
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
  let encode_dep (dep : NpmRequest.t) =
    match dep with
    | NpmRequest.Registry (_,name,NpmRequest.Constraint.Exact npm_version) ->
      let ((map, _), _) = VersionMap.for_package name context.versions in
      let version = VersionMap.Map.find npm_version map in
      [[name, Some (`Eq, version)]]
    | NpmRequest.Registry (_,_,NpmRequest.Constraint.Tag _) ->
      []
    | NpmRequest.Registry (_,name,NpmRequest.Constraint.Range items) ->
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
    | NpmRequest.Local _ -> []
    | NpmRequest.Remote _ -> []
    | NpmRequest.Git _ -> []
  in

  let depends =
    manifest.NpmManifest.dependencies
    |> List.map encode_dep
    |> List.flatten
    |> List.filter (fun item -> item <> [])
  in

  let npm_dependencies =
    manifest.NpmManifest.dependencies
    |> List.map
      (function
        | NpmRequest.Registry (_,name,c) ->
          name ^ "@{" ^ NpmRequest.Constraint.to_string c ^ "}"
        | NpmRequest.Local (raw,_,_) -> raw
        | NpmRequest.Remote (raw,_,_) -> raw
        | NpmRequest.Git (raw,_,_) -> raw)
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

let encode_universe (univ : EsyCore.Universe.t) =

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
