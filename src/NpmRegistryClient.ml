module StringMap = Map.Make(String)

type client = <
  get : string -> unit -> unit Js.Promise.t [@bs.meth]
> Js.t

type packument_internal = <
  name : string;
  versions : manifest_internal Js.Dict.t;
> Js.t

and manifest_internal = <
  name : string;
  version : string;
  dependencies : dependencies_internal Js.undefined;
  devDependencies : dependencies_internal Js.undefined;
  peerDependencies : dependencies_internal Js.undefined;
  buildDependencies : dependencies_internal Js.undefined;
> Js.t

and dependencies_internal = string Js.Dict.t

let parse_dict parse_value dict = 
  let result = ref StringMap.empty in
  let entries = Js.Dict.entries dict in
  Array.iter
    (fun (k, v) -> result := StringMap.add k (parse_value v) !result)
    entries;
  !result

let parse_dependencies data =
  let data = Js.Undefined.to_opt data in
  let result = Option.map data
      (fun data ->
         let entries = Js.Dict.entries data in
         let reqs = Array.map
             (fun (name, constr) ->
                let req = Printf.sprintf "%s@%s" name constr in
                NpmPackageArg.of_string_exn req)
             entries
         in
         Array.to_list reqs
      ) in
  Option.or_default [] result

let parse_manifest data = {
  NpmTypes.Manifest.
  name = data##name;
  version = data##version;
  dependencies = parse_dependencies data##dependencies;
  dev_dependencies = parse_dependencies data##devDependencies;
  peer_dependencies = parse_dependencies data##peerDependencies;
  build_dependencies = parse_dependencies data##buildDependencies;
}

let parse_packument data =
  let versions = parse_dict parse_manifest data##versions in
  let versions = StringMap.bindings versions in
  let versions = List.map (fun (_, manifest) -> manifest) versions in
  {
    NpmTypes.Packument.
    name = data##name;
    versions;
  }

external create_internal : unit -> client =
  "../../../src_js/npm-registry-client" [@@bs.new] [@@bs.module]

let create () = create_internal ()

let registry_url = "https://registry.npmjs.org"

let get_packument
    ?(timeout : int option)
    ?(follow : bool option)
    ?(staleOk : bool option)
    ?(auth : unit option)
    ?(fullMetadata : bool option)
    client
    package_name =
  let module Let_syntax = Ppx_let_promise.Let_syntax in
  let params = Obj.magic ([%bs.obj {
      timeout = Js.Undefined.from_opt timeout;
      follow = Js.Undefined.from_opt follow;
      staleOk = Js.Undefined.from_opt staleOk;
      auth = Js.Undefined.from_opt auth;
      fullMetadata = Js.Undefined.from_opt fullMetadata;
    }])
  in
  let url = registry_url ^ "/" ^ package_name in
  match%bind N.Error.handle_error (client##get url params) with
  | Js.Result.Ok data ->
    let (data : packument_internal) = Obj.magic data in
    Js.Promise.resolve @@ Some (parse_packument data)
  | Js.Result.Error _err ->
    Js.Promise.resolve None
