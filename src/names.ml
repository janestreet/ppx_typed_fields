open! Base

let derived_on_name = "derived_on"

let localize name ~local =
  match local with
  | false -> name
  | true -> name ^ "__local"
;;
