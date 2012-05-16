

module ComplexMatrix = MatrixFactory.Make( 
  struct
    type t = Complex.t
    let zero = Complex.zero
    let one = Complex.one
    let prod = Complex.mul
    let div = Complex.div
    let sqrt = Complex.sqrt
    let add = Complex.add
    let norm2 = Comlex.norm2
    let to_string = fun (c:Complex.t) ->
      Printf.sprintf "%.2f + %.2fI " c.Complex.re c.Complex.im
  end
);;
open ComplexMatrix


(* Register Class {{{1 *)
exception State_Does_Not_Exist of string;;
class register n = object
  val size = n
  val state = new vector (1 lsl n) (* Equivalent à 2^n *)
  (* Non destructif *)
  method getStateProbability s = 
    if s >= state#rows () then raise State_Does_Not_Exist "getStateProbability"
    else state#row s
  method size () = size
  (* Destructif *)
  method measureState () =
    if s >= state#rows () then raise State_Does_Not_Exist "measureState"
    else begin
      (* FIXME trop naif ? *)
      let measured      = ref false
      and stateMeasured = ref -1
      and alea          = float_of_int (Random.int ()) /. float_of_int max_int 
      and bottom        = ref 0. 
      and top           = ref 0. in  
      for i = 1 to state#rows do
        if not measured then begin
          let norm = (Complex.norm2 (state#row i)) in
          top := !top +. norm;
          if !bottom < alea && alea < !top then begin
            measured := true;
            stateMeasured := i
            state#rowset i Complex.one
          end else state#rowset i Complex.zero
          bottom := !bottom +. norm 
        end else state#rowset i Complex.zero
      done;
      !stateMeasured
    end
end
(* }}} *)
(*
(* Circuit Class {{{1 *)
class circuit n = object
  (* Nombre de qubits du circuit *)
  val nb_qubits = n
  (* Portes du circuit, les circuits étant linéaires *)
  val mutable gates = []
  (* Ajout d'une porte *)
  method add_gate (gate:gate) = gates = gate :: gates
end
(* }}} *)
 *)
