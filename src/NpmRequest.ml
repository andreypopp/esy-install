module Constraint = struct
  type t = 
    | Exact of NpmVersion.t
    | Tag of string
    | Range of NpmVersionConstraint.formula

  let to_string (c : t) =
    match c with
    | Exact v -> NpmVersion.to_string v
    | Tag v -> v
    | Range disj ->
      let wrap_cnj cnj = match disj with
        | [] | _::[] -> cnj
        | _ -> "{" ^ cnj ^ "}"
      in
      disj
      |> List.map (fun cnj -> wrap_cnj (cnj |> List.map NpmVersionConstraint.Relation.to_string |> String.concat ", "))
      |> String.concat " || "
end

type t =
  (** Request to a package stored in a registry *)
  | Registry of (string * string * Constraint.t)
  (** Request to a package stored locally on a filesystem *)
  | Local of (string * string option * local_info)
  (** Request to a package stored remotely and addressed by an URL *)
  | Remote of (string * string option * string)
  (** Request to a package stored remotely in a git repo *)
  | Git of (string * string option * git_info)

and local_info =
  | Directory of string
  | File of string

and git_info =
  | GitHub of github_info
  | GitRepo of gitrepo_info

and github_info = {
  github_username : string;
  github_reponame : string;
  github_committish : string;
}

and gitrepo_info = {
  git_url : string;
  git_committish : string;
}
