type t = {
  name : string;
  version : NpmVersion.t;
  (** Regular dependencies needed during package runtime. *)
  dependencies : NpmRequest.t list;
  (** Dependencies needed during package development (e.g. merlin, ocp-indent). *)
  dev_dependencies : NpmRequest.t list;
  (** Dependencies needed during package build process. (e.g. jbuilder, reason) *)
  build_dependencies : NpmRequest.t list;
  (** TODO: decide what to do with peer_dependencies. We probably need to
      continue supporting them for b/c reasons but they are not needed anymore
      as the default dep solving strategy is going to be the same as for
      regular dependencies. *)
  peer_dependencies : NpmRequest.t list;
}
