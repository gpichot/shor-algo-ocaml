open Log;;
open Printf;;
(* Quelques fonctions sur les complexes {{{1 *)
let complex_of_float f = {
  Complex.re = f;
  Complex.im = 0.;
}
and complex_of_int i = { 
  Complex.re = float_of_int i;
  Complex.im = 0.;
}
let ( *! ) = Complex.mul
let ( +! ) = Complex.add
let ( -! ) = Complex.sub
(* }}} *)
let pi = acos (-1.);;
let c_to_string = fun (c:Complex.t) ->
      Printf.sprintf "%.9F + %.9FI " c.Complex.re c.Complex.im
let w n = Complex.exp (
  (complex_of_float(2. *. pi /. (float_of_int n)  ) ) *! Complex.i
)

module ComplexMatrix = MatrixFactory.Make( 
  struct
    type t = Complex.t
    let zero = Complex.zero
    let one = Complex.one
    let prod = Complex.mul
    let mul a b = Complex.mul a (complex_of_float b) 
    let add = Complex.add
    let norm2 = Complex.norm2
    let to_string = c_to_string
  end
);;
open ComplexMatrix;;


(* Register Class {{{1 *)
exception Quant_Bad_Access of string;;
class register n = object(self)
  val size = n
  val state = new vector ~rows:(1 lsl n) () (* Equivalent à 2^n *)
  method size () = size
  method state () = state
  method nbStates () = 1 lsl n
  method norm () = state#norm ()
  method normalize () = state#normalize ()
  method setState s = 
    if Array.length s > state#rows () then raise (Quant_Bad_Access "setState")
    else begin
      for i = 0 to Array.length s - 1 do
        state#rowset (i + 1) s.(i)
      done;
    end
  method setStateProbability s v = state#rowset (s+1) v
  method getStateProbability s = 
    if s > state#rows () then raise (Quant_Bad_Access "getStateProbability")
    else state#row (s+1)
  method dump () =
    printf "Le registre est dans l'état (norme %f):\n" (self#norm () );
    for i = 1 to state#rows () do
      printf "État %i : %s.\n" (i-1) (c_to_string (state#row i));
    done
  (* Met les n premiers états dans un état de superposition uniforme *)
  method setUniformSuperposition n =
    if n > state#rows () then raise (Quant_Bad_Access "setUniformSuperposition")
    else begin
      let prob = complex_of_float (sqrt(1. /. (float_of_int n))) in
      for i = 1 to n do
        state#rowset i prob
      done
    end
  (* Transformation de Fourier discrète sur les q premiers états {{{2 *)
  method dft q = 
    let dftvals = Array.make q Complex.zero in 
    for a = 0 to q - 1 do
      for c = 0 to q - 1 do
        dftvals.(c) <- dftvals.(c) +! (
          Complex.exp (
            (complex_of_float(2. *. pi /. (float_of_int q) *. float_of_int(a * c)  ) ) *! Complex.i
          ) *! (state#row (a + 1))
        )
      done
    done;
    self#setState dftvals
  (* }}} *)
  (* Transformée de Fourier rapide (car q est une puissance de 2 !!!) {{{2 *)
  method fft () =
    let rec fft_aux ?(step=1) ?(start=0) () =
      let n = state#rows () / step in
      if n = 1 then [| state#row (start + 1) |]
      else begin
        let even = fft_aux ~step:(step * 2) ~start ()
        and odd  = fft_aux ~step:(step * 2) ~start:(start + step) () in
        let c  = ref Complex.one
        and w  = w n
        and u' = Array.make n Complex.zero in
        for k = 0 to n / 2 - 1 do 
          u'.(k) <- even.(k) +! !c *! odd.(k);
          u'.(k + n / 2) <- even.(k) -! !c *! odd.(k);
          c := !c *! w
        done;
        u'
      end
    in self#setState (fft_aux ())
    (* }}} *)
  (* measureState {{{2 *)
  method measureState () =
    (* On procède ainsi, imaginons l'état suivant :
     *             00              01     10   11
     * |          0.5          |  0.25  | 0 | O.25 |
     * |  ------------ alea ------->|0.63  
     * La probabilité de chaque état est bien respecté   *)
    let measured      = ref false
    and stateMeasured = ref (-1)
    and alea          = float_of_int (Random.int max_int) /. (float_of_int max_int)
    and bottom        = ref 0. 
    and top           = ref 0. in  
    for i = 1 to state#rows () do
      if not !measured then begin
        let norm = (Complex.norm2 (state#row i)) in
        top := !top +. norm;
        if !bottom < alea && alea < !top then begin
          measured := true;
          stateMeasured := i - 1;
          state#rowset i Complex.one
        end else begin state#rowset i Complex.zero end;
        bottom := !bottom +. norm 
      end else state#rowset i Complex.zero
    done;
    !stateMeasured
  (* }}} *)
end
(* }}} *)
