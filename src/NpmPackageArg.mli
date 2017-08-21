val of_string : string -> NpmRequest.t option
val of_string_exn : string -> NpmRequest.t
val to_string : NpmRequest.t -> string
val compare : NpmRequest.t -> NpmRequest.t -> int
