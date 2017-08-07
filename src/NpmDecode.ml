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

let manifest json =
  let open Json.Decode in
  let optional_dependencies_field name = 
    let field = field name dependencies in
    map (Option.or_default []) (optional field)
  in {
    NpmTypes.Manifest.
    name = json |> field "name" string;
    version = json |> field "version" string;
    dependencies = json |> optional_dependencies_field "dependencies";
    dev_dependencies = json |> optional_dependencies_field "devDependencies";
    build_dependencies = json |> optional_dependencies_field "buildDependencies";
    peer_dependencies = json |> optional_dependencies_field "peerDependencies";
  }

let packument json =
  let open Json.Decode in
  let versions =
    let parse_versions versions =
      let versions = Js.Dict.entries versions in
      let versions = Array.map (fun (_, manifest) -> manifest) versions in
      Array.to_list versions
    in
    map parse_versions (dict manifest)
  in {
    NpmTypes.Packument.
    name = json |> field "name" string;
    versions = json |> field "versions" versions;
  }
