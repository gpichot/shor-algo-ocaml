(*
 * Ce fichier contient sans doute la partie la plus intéressante :
 * recherche de l'ordre
 * puis diverses fonctions arithmétiques : pgcd, test si premier...
 *)

open Log;;
open Printf;;
open Quantum;;
open Print;;

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
(* primePowerTest {{{1 
 * Teste si l'entier n passé en argument n'est pas la puissance d'un nombre *)
let primePowerTest n = 
  let nb = [|3;5;7;11;13;17;19;23;29;31|] in
  let isPrimePower = ref false in
  let i = ref 0 in
  while not !isPrimePower && !i < Array.length nb - 1 do
    let m = ref nb.(!i) in
    while not !isPrimePower && !m <= n do
      if !m = n then isPrimePower := true;
      m := !m * nb.(!i)
    done;
    incr i;
  done;
  !isPrimePower
(* }}} *)
(* Pgcd {{{1 *)
let rec pgcd a b = match b with 
  | 0 -> a
  | _ -> pgcd b (a mod b)
(* }}} *)
(* isANonTrivialDivisor {{{1 *)
let isANonTrivialDivisor p n = p <> 1 && p <> n && n mod p = 0
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
(* Approximation du réél a par une fraction {{{ *) 
(* Les fractions continues fournissent une bonne approximation *)
let approx x q = 
  let rec reduite (p0,p1) (q0,q1) r precis =
    if abs_float (p1 /. q1 -. x) < precis then (p1, q1)
    else begin
      let a = floor r in
      let d = r -. a in
      reduite (p1, a *. p1 +. p0) (q1, a *. q1 +. q0) (1. /. d) precis
    end
  in let (p,q) = reduite (1., floor x) (0., 1.) (1. /. (x -. floor x)) (0.5 /. q)
  in (int_of_float p, int_of_float q)
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
(* Donne le nombre de bits nécessaire pour représenter l'entier *)
let rec nb_bits = function 
  | 0 -> 0
  | n -> 1 + (nb_bits (n lsr 1))

let order ~texPrint p n = 
  debug "\n---- Recherche de l'ordre de %i dans %i ----\n" p n; 
  let l = getQ n in
  let q = pow 2 l in
  debug "On trouve q = %i = 2 ^ %i.\n" q l;
  let reg1 = new register l
  and reg2 = new register (nb_bits n) in
  debug "Création de deux registres de tailles respectives %i et %i.\n" 
    (reg1#size()) (reg2#size());
  (* On met le premier registre dans un état de superposition uniforme *)
  reg1#setUniformSuperposition q;
  (* On calcule p ^ a mod n pour tous les a, on doit cependant
   * penser à les conserver (ce que ne peut pas faire un calculateur 
   * quantique) pour les "superposer" avec le premier registre ensuite. *)
  let expModTemp = Array.make n Complex.zero in
  let sauvExpMod = Array.make q 0 in
  for a = 0 to q - 1 do
    let state = expModulaire p a n in
    sauvExpMod.(a) <- state;
    expModTemp.(state) <- expModTemp.(state) +! Complex.one
  done;
  reg2#setState expModTemp;
  (* ... mais on veut des probabilités donc on normalise *)
  reg2#normalize ();
  (* On mesure le second registre *)
  let value = reg2#measureState () in
  (* On doit répercuter cette mesure sur le premier registre *)
  for a = 0 to q - 1 do
    let new_val = (if sauvExpMod.(a) = value then Complex.one else Complex.zero) in
    reg1#setStateProbability a new_val
  done;
  (* Et on oublie pas de normaliser *)
  reg1#normalize ();
  (* Maintenant on applique la transformée de Fourier *)
  debug "Transformée de Fourier.\n"; 
  reg1#fft (); 
  debug "Fin de la transformée de Fourier.\n";
  reg1#normalize ();
  (* Si on doit afficher le résultat de la transformée de Fourier *)
  if texPrint then printAsTex reg1 p n;
  (* On mesure c sur le premier registre *)
  let c = reg1#measureState () in
  debug "On trouve pour c : %i.\n" c;
  (* On approche le réel grâce aux fractions continues *)
  let s =  (float_of_int c) /. (float_of_int q) in
  let (d,r) = approx s (float_of_int q) in
  debug "---- Un ordre possible est donc %i. ----\n" r;
  r


(* }}} *)
