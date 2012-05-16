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
Random.init 42;
set_debug true;
if !mod_debug then print "Mode DEBUG : on.\n";


(* Le nombre que l'on souhaite factoriser *)
let nb_args = Array.length argv in
let n = 
  if nb_args >= 2 then int_of_string argv.(1)
  else begin
    print "Veuillez entrer un nombre à factoriser : ";
    read_int ();
  end
in
printf "Le nombre à factoriser est %i.\n" n;
(* Un nombre qui ne divise pas n *)
let p = 
  if nb_args >= 3 then int_of_string argv.(2)
  else begin
    print "Veuillez entrer un nombre ne divisant pas n : "; 
    read_int ();
  end
in
printf "Le nombre qui va nous servir (et qui ne divise pas n) est : %i.\n" p;
(* }}} *)

(* Vérifications {{{1 *) 
(* 1) N n'est pas pair *)
if n mod 2 = 0 then begin
  print "Erreur : Le nombre à factoriser doit être impair !";
  exit 0
end;
(* 2) Si le nombre est premier *)
if primeTest n then begin
  print "Erreur : Le nombre ne doit pas être premier !";
  exit 0
end;
(* @TODO
 * 3) Teste si le nombre est la puissance d'un nombre premier (ne fait rien 
 * pour le moment en vérité) *)
if primePowTest n then begin
  print "Erreur : le nombre ne doit être la puisance d'un nombre premier !";
  exit 0
end;
(* Teste si 1 <= p <= n - 1 *)
if 1 > p || p >= n then begin
  printf "Erreur : l'entier p doit être compris entre 1 et le nombre à factoriser strictement";
  exit 0
end;
(* Teste si n et p sont bien premiers entre eux *)
if pgcd n p <> 1 then begin
  printf "Pseudo-Succès :  %i | %i (sans passer l'algorithme de Shor).\n" p n;
  exit 0
end;
(* }}} *)

(* Ce qui suit peut paraître dérisoire mais c'est pourtant le coeur de notre
 * algorithme et tout notre sujet :) *)
let r     = order p n in
let ppow  = pow p (r / 2) in

(* Tests sur l'ordre {{{1 *)
(* Si r est impair *)
if r mod 2 = 1 then begin
  print "Erreur : l'ordre est impair, réessayez avec un autre nombre !";
  exit 0
end;
(* Si r est congru a -1 modulo n *)
if ppow mod n = n - 1 then begin
  print "Erreur : x^(r/2) est congru à -1 modulo n !, un autre entier ?";
  exit 0
end;
(* }}} *)

(* It's done !! {{{1 *)
let r1 = pgcd (ppow - 1) n
and r2 = pgcd (ppow + 1) n in
if isANonTrivialDivisor r1 n then
  printf "\nSuccès : %i divise %i." r1 n
else if isANonTrivialDivisor r2 n then
  printf "\nSuccès : %i divise %i." r2 n
else
  print "Impossible de factoriser ce nombre, désolé.";
(* }}} *)


