open NpmTypes

val of_string : string -> Request.t option
val of_string_exn : string -> Request.t
val to_string : Request.t -> string
val compare : Request.t -> Request.t -> int
