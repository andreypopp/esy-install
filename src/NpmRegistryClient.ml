module StringMap = Map.Make(String)

type client = <
  get : string -> unit -> unit Js.Promise.t [@bs.meth]
> Js.t

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
    let (data : Js.Json.t) = Obj.magic data in
    Js.Promise.resolve @@ Some (NpmDecode.packument data)
  | Js.Result.Error _err ->
    Js.Promise.resolve None
