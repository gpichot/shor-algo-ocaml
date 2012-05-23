
open Log;;
open Printf;;

(* Pi *)
let pi = acos (-1.);;
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

let c_to_string = fun (c:Complex.t) ->
      Printf.sprintf "%.9F + %.9FI " c.Complex.re c.Complex.im
let w n = Complex.exp (
  (complex_of_float(2. *. pi /. (float_of_int n)  ) ) *! Complex.i
)
(* }}} *)
(* Module Matrix pour les complexes {{{1 *)
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
(* }}} *)


(* Register Class {{{1 *)
exception Quant_Bad_Access of string;;
class register n = object(self)
  inherit vector ~rows:(1 lsl n) () as super
  val size = n
  method size () = size
  method nbStates = self#rows
  method setState s = 
    if Array.length s > self#nbStates then raise (Quant_Bad_Access "setState")
    else begin
      for i = 0 to Array.length s - 1 do
        self#rowset (i + 1) s.(i)
      done;
    end
  method setStateProbability s v = self#rowset (s+1) v
  method getStateProbability s = 
    if s > self#nbStates then raise (Quant_Bad_Access "getStateProbability")
    else self#row (s+1)
  method dump () =
    printf "Le registre est dans l'état (norme %f):\n" (self#norm () );
    for i = 1 to self#rows do
      printf "État %i : %s.\n" (i-1) (c_to_string (self#row i));
    done
  (* Met les n premiers états dans un état de superposition uniforme *)
  method setUniformSuperposition n =
    if n > self#nbStates then raise (Quant_Bad_Access "setUniformSuperposition")
    else begin
      let prob = complex_of_float (sqrt(1. /. (float_of_int n))) in
      for i = 1 to n do
        self#rowset i prob
      done
    end
  (* Transformation de Fourier discrète sur les q premiers états {{{2 *)
  method dft q = 
    let dftvals = Array.make q Complex.zero in 
    for a = 0 to q - 1 do
      for c = 0 to q - 1 do
        dftvals.(c) <- dftvals.(c) +! (
          Complex.exp ( (
              complex_of_float(
                2. *. pi /. (float_of_int q) *. float_of_int(a * c)  
              ) 
            ) *! Complex.i
          ) *! (self#row (a + 1))
        )
      done
    done;
    self#setState dftvals
  (* }}} *)
  (* Transformée de Fourier rapide (q est une puissance de 2) {{{2 *)
  method fft () =
    let rec fft_aux ?(step=1) ?(start=0) () =
      let n = self#rows / step in
      if n = 1 then [| self#row (start + 1) |]
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
    for i = 1 to self#rows do
      if not !measured then begin
        let norm = (Complex.norm2 (self#row i)) in
        top := !top +. norm;
        if !bottom < alea && alea < !top then begin
          measured := true;
          stateMeasured := i - 1;
          self#rowset i Complex.one
        end else begin self#rowset i Complex.zero end;
        bottom := !bottom +. norm 
      end else self#rowset i Complex.zero
    done;
    !stateMeasured
  (* }}} *)
end
(* }}} *)
