open Printf;;



(** Vars **)
(* Open the logfile *)
let logfile =  open_out "shor.log" (*stdout * open_out "shor.log" *)
(* Enabled debug mod *)
let mod_debug = ref false

(** Methods **)
(* Active, unactive debug mode *)
let set_debug b = mod_debug := b
(* Log the fmt into the file *)
(* let log fmt = fprintf logfile fmt *)
let log fmt = printf fmt
(* Log the debug fmt if necessary *)
let debug fmt = if !mod_debug then fprintf logfile fmt else ifprintf logfile fmt
(* Show in the console (do not forget to use the ! to flush the data) *)
let print = printf "%s%!"
