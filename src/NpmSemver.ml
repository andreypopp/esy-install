external satisfies_internal : string -> string -> bool = "satisfies" [@@bs.module "semver"]

let satisfies version constr =
  match constr with
  | NpmTypes.VersionConstraint.Exact constr -> satisfies_internal version constr
  | NpmTypes.VersionConstraint.Range constr -> satisfies_internal version constr
  | NpmTypes.VersionConstraint.Tag _ -> false
