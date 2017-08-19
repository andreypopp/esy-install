type t

external major : t -> int = "" [@@bs.get]
external minor : t -> int = "" [@@bs.get]
external patch : t -> int = "" [@@bs.get]
external of_string_exn : string -> t = "SemVer" [@@bs.module "semver"]
external compare : t -> t -> int = "compare" [@@bs.module "semver"]
external to_string : t -> string = "toString" [@@bs.send]

val of_string : string -> t option
