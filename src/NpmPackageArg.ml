type spec_internal = <
  _type : string;
  name : string Js.null_undefined;
  fetchSpec : string;
  hosted : hosted_internal Js.null_undefined;
  gitCommittish : string Js.null_undefined;
> Js.t

and hosted_internal = <
  _type : string;
  user : string;
  project : string;
> Js.t

and range_internal = <
  set : <
    operator : string;
    semver : NpmVersion.t;
  > Js.t Js.Array.t Js.Array.t;
> Js.t

external parse_internal :
  string ->
  spec_internal =
  "npm-package-arg" [@@bs.module]

external parse_range_internal :
  string ->
  range_internal =
  "Range" [@@bs.module "semver"]

let parse_range range =
  let parse_relop relop =
    let open NpmVersionConstraint.Relation in
    let version = relop##semver in
    match relop##operator with
    | "<" -> [LT version]
    | "<=" -> [LTE version]
    | ">" -> [GT version]
    | ">=" -> [GTE version]
    | "=" -> [EQ version]
    | "" -> [EQ version]
    | _ -> []
  in
  let range =
    parse_range_internal range
  in
  range##set
  |> Array.map (fun items ->
      items
      |> Array.map parse_relop
      |> Array.to_list
      |> List.flatten)
  |> Array.to_list

let of_string value =
  let module Let_syntax = Option.Let_syntax in
  let obj = parse_internal value in
  let name = Js.Null_undefined.to_opt obj##name in
  match obj##_type with

  | "version" ->
    let%bind name = name in
    let version = NpmVersion.of_string_exn obj##fetchSpec in
    let version = NpmRequest.Constraint.Exact version in
    Some (NpmRequest.Registry (value, name, version))

  | "range" ->
    let%bind name = name in
    let range = parse_range obj##fetchSpec in
    let version = NpmRequest.Constraint.Range range in
    Some (NpmRequest.Registry (value, name,  version))

  | "tag" ->
    let%bind name = name in
    let version = NpmRequest.Constraint.Tag obj##fetchSpec in
    Some (NpmRequest.Registry (value, name,  version))

  | "directory" ->
    let path = NpmRequest.Directory obj##fetchSpec in
    Some (NpmRequest.Local (value, name, path))

  | "file" ->
    let path = NpmRequest.File obj##fetchSpec in
    Some (NpmRequest.Local (value, name, path))

  | "remote" ->
    let url = obj##fetchSpec in
    Some (NpmRequest.Remote (value, name, url))

  | "git" ->
    let hosted = Js.Null_undefined.to_opt obj##hosted in
    let%bind git_info = match hosted with
      | Some hosted ->
        begin match hosted##_type with
          | "github" ->
            let%bind github_committish = Js.Null_undefined.to_opt obj##gitCommittish in
            Some (
              NpmRequest.GitHub {
                github_username = hosted##user;
                github_reponame = hosted##project;
                github_committish;
              }
            )
          | _ ->
            None
        end
      | None ->
        let%bind git_committish = Js.Null_undefined.to_opt obj##gitCommittish in
        Some (
          NpmRequest.GitRepo {
            git_url = obj##fetchSpec;
            git_committish;
          }
        )
    in
    Some (NpmRequest.Git (value, name, git_info))

  | _ ->
    None

exception ParseError of string

let of_string_exn value =
  match of_string value with
  | Some v -> v
  | None -> raise (ParseError value)

let to_string v = match v with
  | NpmRequest.Registry (v, _, _) -> v
  | NpmRequest.Local (v, _, _) -> v
  | NpmRequest.Remote (v, _, _) -> v
  | NpmRequest.Git (v, _, _) -> v

let compare x y =
  String.compare (to_string x) (to_string y)
