LoudRejection.install ()

module P = PromiseSupport
module StringSet = Set.Make(String)
module StringMap = Map.Make(String)
module IntMap = Map.Make(struct type t = int let compare = compare end)

let build_universe req =
  let module Let_syntax = P.Let_syntax in

  let resolving = Js.Dict.empty () in
  let univ = ref EsyCore.Universe.empty in

  let resolving_key req =
    let open NpmRequest in
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
      P.resolve ()
    | Some { name; versions } ->
      let pkg_univ = ref (EsyCore.Universe.lookup_package name !univ) in
      let tasks = List.map
          (fun ({ NpmManifest. dependencies } as manifest) ->
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

let () =

  let main =
    let module Let_syntax = P.Let_syntax in
    let req = NpmPackageArg.of_string_exn "./pkg" in
    let%bind univ = build_universe req in
    let cudf_univ = EsyCudf.encode_universe univ in
    Cudf_printer.pp_universe stdout cudf_univ;
    P.return ()
  in

  ignore (N.Main.run main)
