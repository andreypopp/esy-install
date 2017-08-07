open NpmTypes

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

external parse_internal :
  string ->
  spec_internal =
  "npm-package-arg" [@@bs.module]

let of_string value =
  let module Let_syntax = Option.Let_syntax in
  let obj = parse_internal value in
  let name = Js.Null_undefined.to_opt obj##name in
  match obj##_type with

  | "version" ->
    let%bind name = name in
    let version = VersionConstraint.Exact obj##fetchSpec in
    Some (Request.Registry (value, name,  version))

  | "range" ->
    let%bind name = name in
    let version = VersionConstraint.Range obj##fetchSpec in
    Some (Request.Registry (value, name,  version))

  | "tag" ->
    let%bind name = name in
    let version = VersionConstraint.Tag obj##fetchSpec in
    Some (Request.Registry (value, name,  version))

  | "directory" ->
    let path = Request.Directory obj##fetchSpec in
    Some (Request.Local (value, name, path))

  | "file" ->
    let path = Request.File obj##fetchSpec in
    Some (Request.Local (value, name, path))

  | "remote" ->
    let url = obj##fetchSpec in
    Some (Request.Remote (value, name, url))

  | "git" ->
    let hosted = Js.Null_undefined.to_opt obj##hosted in
    let%bind git_info = match hosted with
      | Some hosted ->
        begin match hosted##_type with
          | "github" ->
            let%bind github_committish = Js.Null_undefined.to_opt obj##gitCommittish in
            Some (
              Request.GitHub {
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
          Request.GitRepo {
            git_url = obj##fetchSpec;
            git_committish;
          }
        )
    in
    Some (Request.Git (value, name, git_info))

  | _ ->
    None

exception ParseError of string

let of_string_exn value =
  match of_string value with
  | Some v -> v
  | None -> raise (ParseError value)

let to_string v = match v with
  | Request.Registry (v, _, _) -> v
  | Request.Local (v, _, _) -> v
  | Request.Remote (v, _, _) -> v
  | Request.Git (v, _, _) -> v

let compare x y =
  String.compare (to_string x) (to_string y)
