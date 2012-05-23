open Log;;
open Graphics;;
open Quantum;;

let showProb u = 
  let n = u#rows () in
  let base = if 800 / n = 0 then 1 else 800 / n in
  let swidth = base * n
  and sheight = 300 in
  open_graph (Printf.sprintf " %ix%i" swidth sheight);
  for i = 0 to n - 1 do
    let height = int_of_float (Complex.norm2 (u#row (i + 1)) *. (float_of_int sheight)) in
    fill_rect (i * base) 0 base height
  done;
  let _ = read_key () in
  close_graph ()
;;

let printAsTex u n p =
  (* On récupère la liste des données qui peuvent être vues *)
  let rec obtainList = function
    | 0 -> []
    | k -> let y = Complex.norm2 (u#row k) in
      if y > 0.00001 then (k,y) :: (obtainList (k - 1))
      else obtainList (k - 1)
  and max = List.fold_left (fun a b -> if snd a > snd b then a else b) (0,0.) in
  let data = obtainList (u#rows ()) in
  let max = snd (max data) in
  let m = u#rows () in
  let s = List.fold_left begin fun s (i,y) -> 
    let x = float_of_int i *. 16. /. (float_of_int m) +. 0.01
    and y = y /. max *. 2. in
    s ^ (Printf.sprintf "(%f,%f)\n" x y) 
  end "" data in
  let file = open_out (Printf.sprintf "output/shor_%i_%i.tex" n p) in
  let out = ref (Printf.sprintf "\\begin{tikzpicture}
    \\draw (0,0) -- (0,3);
    \\draw (0,0) -- (16,0);\n" ) in
  for i = 0 to 8 do
    out := !out ^ (Printf.sprintf "  \\draw(%i,0)node[below]{%i};\n" (i * 2) (i * m / 8))
  done;
  out := !out ^ "  \\foreach \\x in {0,2,...,16} \\draw (\\x,0) -- (\\x,0.2);
    \\draw[very thick] plot[ycomb] coordinates {\n";
  output_string file (!out ^ s ^ "};\n\\end{tikzpicture}")
;;
   

