
(* Input reading *)
let read_int () = Scanf.scanf " %i" (fun x -> x)

(* Module Matrix : initialisation {{{1 *)
module ComplexMatrix = MatrixFactory.Make (
  struct
    type t = Complex.t
    let zero = Complex.zero
    let one = Complex.one
    let prod = Complex.mul
    let div = Complex.div
    let sqrt = Complex.sqrt
    let add = Complex.add
    let to_string = fun (c:Complex.t) ->
      Printf.sprintf "%.2f + %.2fI " c.Complex.re c.Complex.im
  end
);;
(* }}} *)
(* Quelques fonctions sur les complexes {{{1 *)
let complex_of_float f = {
  Complex.re = f;
  Complex.im = 0.;
}
let ( *: )  = Complex.mul
let ( /: )  = Complex.div
let ( +: )  = Complex.add
(* }}} *)
(*
(* Replace function {{{1 *)
let str_replace search mixed = 
  Str.global_replace (Str.regexp_string search) mixed
in 
(* Translator function *)
let exec str l = 
  let rec _exec str l i = 
    match l with
    | [] -> str
    | t :: q -> 
      let search = "%" ^ (string_of_int i) in
      _exec (str_replace search t str) q (i+1)
  in _exec str l 0
and str = " %0 %1 %2 "
in

log (exec str ["test"; string_of_int 5; string_of_float 5.]);;
(* }}} *)
*)

