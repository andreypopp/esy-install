module VersionConstraint : sig
  type t = 
    | Exact of string
    | Tag of string
    | Range of string
end

(** Represents a dependency request *)
module Request : sig
  type t =
    (** Request to a package stored in a registry *)
    | Registry of (string * string * VersionConstraint.t)
    (** Request to a package stored locally on a filesystem *)
    | Local of (string * string option * local_info)
    (** Request to a package stored remotely and addressed by an URL *)
    | Remote of (string * string option * string)
    (** Request to a package stored remotely in a git repo *)
    | Git of (string * string option * git_info)

  and registry_info =
    | Exact of string
    | Tag of string
    | Range of string

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
end

(** Package manifest *)
module Manifest : sig
  type t = {
    name : string;
    version : string;
    (** Regular dependencies needed during package runtime. *)
    dependencies : Request.t list;
    (** Dependencies needed during package development (e.g. merlin, ocp-indent). *)
    dev_dependencies : Request.t list;
    (** Dependencies needed during package build process. (e.g. jbuilder, reason) *)
    build_dependencies : Request.t list;
    (** TODO: decide what to do with peer_dependencies. We probably need to
        continue supporting them for b/c reasons but they are not needed anymore
        as the default dep solving strategy is going to be the same as for
        regular dependencies. *)
    peer_dependencies : Request.t list;
  }
end

module Packument : sig
  type t = {
    name : string;
    versions : Manifest.t list;
  }
end

