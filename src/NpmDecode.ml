let dependencies json =
  let open Json.Decode in
  let dict_to_req_list data =
    let entries = Js.Dict.entries data in
    let reqs = Array.map
        (fun (name, constr) ->
           let req = name ^ "@" ^ constr in
           NpmPackageArg.of_string_exn req)
        entries
    in
    Array.to_list reqs
  in
  map dict_to_req_list (dict string) json

let version json =
  let open Json.Decode in
  json |> map NpmVersion.of_string_exn string

let manifest json =
  let open Json.Decode in
  let optional_dependencies_field name = 
    let field = field name dependencies in
    map (Option.or_default []) (optional field)
  in {
    NpmManifest.
    name = json |> field "name" string;
    version = json |> field "version" version;
    dependencies = json |> optional_dependencies_field "dependencies";
    dev_dependencies = json |> optional_dependencies_field "devDependencies";
    build_dependencies = json |> optional_dependencies_field "buildDependencies";
    peer_dependencies = json |> optional_dependencies_field "peerDependencies";
  }

let packument_exn json =
  let open Json.Decode in
  let versions =
    let parse_versions versions =
      let versions = Js.Dict.entries versions in
      let versions = Array.map (fun (_, manifest) -> manifest) versions in
      Array.to_list versions
    in
    map parse_versions (dict manifest)
  in {
    NpmPackument.
    name = json |> field "name" string;
    versions = json |> field "versions" versions;
  }

let handle_decode_error decoder json =
  try Some (decoder json) with Json.Decode.DecodeError _ -> None

let packument =
  handle_decode_error packument_exn
