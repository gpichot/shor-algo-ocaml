(*
 * Ce fichier contient le squelette de l'application, vérifications etc.
 * @TODO :
 *  - faire la fonction primePowTest
 *  -? ne pas obliger l'utilisateur à rentrer l'entier p. 
 *)
open Log;;        (* Log, debug *)
open Sys;;        (* Arguments *)
open Printf;;     (* Printf *)
open Arithmetik;; (* primeTest, order ...*)

(* Lire un entier depuis l'entrée *)
let read_int () = Scanf.scanf " %i" (fun x -> x);;

(* Initialisation (debug, args, random...) {{{1 *)
print "Simulation de l'algorithme de Shor\n";
Random.self_init ();
set_debug true;
if !mod_debug then print "Mode DEBUG : on.\n";


(* Le nombre que l'on souhaite factoriser *)
let nb_args = Array.length argv in
let n = 
  if nb_args >= 2 then int_of_string argv.(1)
  else begin
    log "Veuillez entrer un nombre à factoriser : ";
    read_int ();
  end
in
log "Le nombre à factoriser est %i.\n" n;
(* Un nombre qui ne divise pas n *)
let p = 
  if nb_args >= 3 then int_of_string argv.(2)
  else begin
    log "Veuillez entrer un nombre ne divisant pas n : "; 
    read_int ();
  end
in
log "Le nombre qui va nous servir (et qui ne divise pas n) est : %i.\n" p;
(* On peut aussi afficher les graphes d'après de transformée de Fourier avec
 * l'option --print mise en dernière *)
let printState = nb_args >= 4 && argv.(3) = "--print" in
let texPrint   = nb_args >= 4 && argv.(3) = "--tex" in
(* }}} *)

(* Vérifications {{{1 *) 
(* 1) N n'est pas pair *)
if n mod 2 = 0 then begin
  log "Erreur : Le nombre à factoriser doit être impair !\n";
  exit 0
end;
(* 2) Si le nombre est premier *)
if primeTest n then begin
  log "Erreur : Le nombre ne doit pas être premier !\n";
  exit 0
end;
(* @TODO
 * 3) Teste si le nombre est la puissance d'un nombre premier (ne fait rien 
 * pour le moment en vérité) *)
if primePowTest n then begin
  log "Erreur : le nombre ne doit être la puisance d'un nombre premier !\n";
  exit 0
end;
(* Teste si 1 <= p <= n - 1 *)
if 1 > p || p >= n then begin
  log "Erreur : l'entier p doit être compris entre 1 et le nombre à factoriser strictement.\n";
  exit 0
end;
(* Teste si n et p sont bien premiers entre eux *)
if pgcd n p <> 1 then begin
  log "Pseudo-Succès :  %i | %i (sans passer l'algorithme de Shor).\n" (pgcd p n) n;
  exit 0
end;
(* }}} *)

(* Tests sur l'ordre {{{1 *)
let orderFail r p = 
  (* Si r est impair *)
  if r mod 2 = 1 then begin
    log "Erreur : l'ordre est impair (%i), réessayez avec un autre nombre !\n" r;
    true
  end
  (* Si r est congru a -1 modulo n *)
  else if expModulaire p (r / 2) n = n - 1 then begin
    log "Erreur : x^(r/2) est congru à -1 modulo n !, un autre entier ?\n";
    true
  end else false
and findFactor r p =
  let ppow = (expModulaire p (r/2) n) in (*pow p (r / 2) in *)
  let r1 = pgcd (ppow - 1) n
  and r2 = pgcd (ppow + 1) n in
  if isANonTrivialDivisor r1 n then r1
  else if isANonTrivialDivisor r2 n then r2
  else 0
in
(* }}} *)

(* Ce qui suit peut paraître dérisoire mais c'est pourtant le coeur de notre
 * algorithme et tout notre sujet :) *)

(* Et ... {{{1 *)
let success = ref false 
and attempts = ref 5 in
while not !success && !attempts > 0 do
  let r = order ~print:printState ~texPrint p n in
  log "Ordre possible trouvé : %i.\n" r; 
  if not (orderFail r p) then begin
    let factor = findFactor r p in
    if factor <> 0 then begin
      log "\n ---- Succès : %i = %i x %i. ----\n" n factor (n / factor);
      success := true
    end
  end;
  decr attempts
done;
if not !success then
  log "\nToutes les tentatives ont échouées à factoriser ce nombre.\n";
(* }}} *)


