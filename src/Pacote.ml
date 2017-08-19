(** Bindings to pacote library *)

module Bindings = struct
  external packument : string -> Js.Json.t Js.Promise.t = "" [@@bs.module "pacote"]
  external manifest : string -> Js.Json.t Js.Promise.t = "" [@@bs.module "pacote"]
end

module P = PromiseSupport
module Let_syntax = P.Let_syntax

let packument spec =
  match%bind N.Error.handle_error (Bindings.packument spec) with
  | Js.Result.Ok result -> P.resolve (NpmDecode.packument result)
  | Js.Result.Error _ -> P.resolve_none

let manifest spec =
  P.map (Bindings.manifest spec) NpmDecode.manifest
