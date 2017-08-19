type t = <
  major : int;
  minor : int;
  patch : int;
  prerelease : int array;
> Js.t

external major : t -> int = "" [@@bs.get]
external minor : t -> int = "" [@@bs.get]
external patch : t -> int = "" [@@bs.get]
external of_string_exn : string -> t = "SemVer" [@@bs.module "semver"]
external compare : t -> t -> int = "compare" [@@bs.module "semver"]
external to_string : t -> string = "toString" [@@bs.send]

let of_string s =
  try Some (of_string_exn s)
  with Js.Exn.Error _ -> None
