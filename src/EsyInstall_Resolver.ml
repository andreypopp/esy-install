module StringMap = Map.Make(String)

module RequestMap = Map.Make(
  struct
    let compare = Npm.PackageArg.compare
    type t = NpmTypes.Request.t
  end)

module Universe = struct
  open NpmTypes

  (** A mapping from a package name to a list of versions with manifests *)
  type t = (string * Manifest.t) list StringMap.t
end

exception ResolveError of string

let resolve (req : NpmTypes.Request.t) =
  let module Let_syntax = Promise.Let_syntax in

  let resolve_registry name =
    let client = Npm.RegistryClient.create () in
    let%bind packument = Npm.RegistryClient.get_packument ~fullMetadata:true client name in
    match packument with
    | None ->
      Promise.return_none
    | Some packument ->
      let versions =
        List.fold_left
          (fun results manifest -> (manifest.NpmTypes.Manifest.version, manifest)::results)
          []
          packument.NpmTypes.Packument.versions
      in
      Promise.return_some (name, versions)

  and resolve_git _name info =
    match info with
    | NpmTypes.Request.GitHub { github_username; github_reponame; github_committish } ->
      let url = Printf.sprintf
          "https://api.github.com/repos/%s/%s/contents/package.json?ref=%s"
          github_username
          github_reponame
          github_committish
      in
      let%bind resp = Fetch.fetch url in
      let%bind json = Fetch.Response.json resp in
      let manifest =
        json
        |> Json.Decode.field "content" Json.Decode.string
        |> Base64.decode
        |> Js.Json.parseExn
        |> Npm.Decode.manifest
      in
      Promise.return_some (manifest.name, [(manifest.version, manifest)])
    | NpmTypes.Request.GitRepo _ ->
      Promise.return_none

  in

  match req with
  | NpmTypes.Request.Registry (_, name, _) -> resolve_registry name
  | NpmTypes.Request.Local _ -> Promise.return None
  | NpmTypes.Request.Remote _ -> Promise.return None
  | NpmTypes.Request.Git (_,name,info) -> resolve_git name info

let resolve_universe (req : NpmTypes.Request.t) =
  let module Let_syntax = Promise.Let_syntax in

  let resolving = ref StringMap.empty in

  let find_resolving key =
    try Some (StringMap.find key !resolving)
    with Not_found -> None
  in

  let resolving_key req = match req with
    | NpmTypes.Request.Registry (_,name,_) ->
      name
    | NpmTypes.Request.Local (_,_,NpmTypes.Request.Directory path) ->
      path
    | NpmTypes.Request.Local (_,_,NpmTypes.Request.File path) ->
      path
    | NpmTypes.Request.Remote (_,_,url) ->
      url
    | NpmTypes.Request.Git (_,_,NpmTypes.Request.GitHub { github_username; github_reponame; _ }) ->
      (github_username ^ "/" ^ github_reponame)
    | NpmTypes.Request.Git (_,_,NpmTypes.Request.GitRepo { git_url; _ }) ->
      git_url
  in

  let merge_results into from =
    StringMap.fold (
      fun k v into ->
        let next_v = try StringMap.find k into with Not_found -> [] in
        let next_v = v @ next_v in
        StringMap.add k next_v into
    ) from into
  in

  let rec resolve_universe_impl req =
    let key = resolving_key req in
    match find_resolving key with
    | Some task ->
      task
    | None ->
      let task =
        let%bind resolved = resolve req in
        match resolved with
        | None ->
          let req = NpmPackageArg.to_string req in
          raise (ResolveError ("Unable to resolve: " ^ req))
        | Some (name, versions) ->
          let result = StringMap.empty |> StringMap.add name versions in
          let%bind results =
            versions
            |> List.map (fun (_version, manifest) -> manifest.NpmTypes.Manifest.dependencies)
            |> List.flatten
            |> List.map resolve_universe_impl
            |> Promise.all
          in
          let result = List.fold_left merge_results result results in
          Promise.return result
      in
      resolving := StringMap.add key task !resolving;
      task
  in

  resolve_universe_impl req

let () =

  let main =
    let module Let_syntax = Promise.Let_syntax in
    let req = Npm.PackageArg.of_string_exn "ocaml@esy-ocaml/ocaml" in
    let req2 = Npm.PackageArg.of_string_exn "lodash@15.0.0" in
    let%bind (res : Universe.t) = resolve_universe req
    and (res2 : Universe.t) = resolve_universe req2 in
    Js.log res;
    Js.log res2;
    Js.Promise.resolve ()
  in

  N.Main.run main
