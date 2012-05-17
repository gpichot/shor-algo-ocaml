(*
 * Ce fichier contient sans doute la partie la plus intéressante :
 * recherche de l'ordre
 * puis diverses fonctions arithmétiques : pgcd, test si premier...
 *)

open Log;;
open Quantum;;

(* primeTest {{{1
 * Détermine si un nombre est premier ou non (exact).
 * Renvoit true s'il l'est *)
let primeTest n = 
  let racine      = int_of_float (sqrt (float_of_int n)) in
  let isNotPrime  = ref (n mod 2 = 0 && n <> 2) in
  let j           = ref 1 in
  while not !isNotPrime && !j <= racine / 2 do
    if n mod (2 * !j + 1) = 0 then 
      isNotPrime := true;
    incr j
  done;
  not !isNotPrime
(* }}} *)
(* primePowTest {{{1 @TODO
 * Teste si l'entier n passé en argument n'est pas la puissance d'un nombre
 * premier *)
let primePowTest n = false
(* }}} *)
(* Pgcd {{{1 *)
let rec pgcd a b = match b with 
  | 0 -> a
  | _ -> pgcd b (a mod b)
(* }}} *)
(* isANonTrivialDivisor {{{1 *)
let isANonTrivialDivisor p n = p <> 1 && p <> n && p mod n = 0
(* }}} *)
(* pow {{{1 *)
let rec pow x n = match n with
  | 0 -> 1
  | _ -> let r = pow x (n / 2) in
    if n mod 2 = 0 then r * r else r * r * x
(* }}} *)
(* Exponentiation modulaire {{{1 *)
(* Connaissant x a n on calcule x ^ a mod n *)
let expModulaire x a n =
  let rec expMod_aux xi ai r = match ai with
    | 0 -> r
    | _ -> let r = if ai land 1 = 1 then r * xi mod n else r
      in expMod_aux (xi * xi mod n) (ai lsr 1) r
  in expMod_aux x a 1
(* }}} *)

(* Recherche de l'ordre de p dans Z/nZ {{{1 *)
(* On créé plusieurs petites fonctions auxiliaires *)
(* getQ : récupère l'entier tel que n^2 <= q < 2n^2 *)
let getQ n = 
  let n2 = n * n and q = ref 1 in
  let p = ref 2 in
  while !p < n2 do
    incr q;
    p := !p * 2
  done;
  !q

let order p n = 
  log "order %i in %i\n" p n; 
  let l = getQ n in
  let q = pow 2 l in
  log "q = %i = 2 ^ %i\n" q l;
  let reg1 = new register l
  and reg2 = new register l in
  (* On met le premier registre dans un état de superposition uniforme *)
  reg1#setUniformSuperposition q;
  (* On calcule x ^ i mod n dans le second... *)
  for i = 0 to reg2#nbStates () - 1 do
    reg2#setStateProbability i (complex_of_int(expModulaire p i n))
  done;
  (* ... mais on veut des probabilités ! *)
  reg2#normalize ();

  (* Maintenant on applique la transformée de Fourier *)
  reg1#dft q;
  let r = reg1#measureState () in
  r
  

(* }}} *)
