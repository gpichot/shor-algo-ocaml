let rec iterator ?(i=0) n f acc = match i with
  | _ when i > n || i = n -> acc
  | _ -> iterator ~i:(i+1) n f (f i acc)
let rec viterator ?(i=0) n f = match i with
  | _ when i > n || i = n -> ()
  | _ -> viterator ~i:(i+1) n f

(* Ev Module {{{1 *)
module type K = sig
  type t
  val zero : t
  val one : t
  val prod : t -> t -> t
  val div : t -> t -> t
  val sqrt : t -> t 
  val add : t -> t -> t
  val norm2 : t -> float
  val to_string : t -> string
end
(* }}} *)

(* Matrix Module {{{1 *)
module Make = functor (Ev : K) ->  struct
  (* Exceptions {{{2 *)
  exception Dims_Not_Matching of string
  (* }}} *)
  (* Matrix Class {{{2 *)
  class matrix ?data ?(dims=(0,0)) () = object
    val rows = match data with | Some x -> Array.length x     | None -> fst dims
    val cols = match data with | Some x -> Array.length x.(0) | None -> snd dims
    val data = match data with 
      | Some x -> x 
      | None -> Array.make_matrix (fst dims) (snd dims) Ev.zero
    method rows () = rows
    method cols () = cols
    method get row col = data.(row-1).(col-1)
    method set row col value = data.(row-1).(col-1) <- value
    method to_string () =
      Array.fold_left begin fun s row ->
        s ^ (Array.fold_left (fun t coef -> t ^ " " ^ (Ev.to_string coef)) "(" row) ^ " )\n"
      end "" data
  end
  (* }}} *)
  (* Square Matrix Class {{{2 *)
  class square_matrix ?data n = object(self)
    inherit matrix ~dims:(n,n) ?data () as super
    method trace () = iterator n (fun i sum -> Ev.add (self#get i i) sum) Ev.zero
  end
  (* }}} *)
  (* Vector Class {{{2 *)
  class vector ?data n = object(self)
    inherit matrix ~dims:(n,1) ?data () as super
    method row n = super#get n 0
    method rowset n = super#set n 0
    method norm () = sqrt (iterator n begin fun i sum -> 
     (Ev.norm2 (self#row i)) +. sum
    end 0.)
    method normalize () = let norm = self#norm () in
      viterator n (fun i -> self#rowset i (Ev.div (self#row i) norm))
  end
  (* }}} *)
  (* Operators Module {{{2 *)
  module Op = struct
    let build_matrix_array f rows cols = 
      Array.init rows begin fun i ->
        Array.init cols (fun j -> f i j)
      end

    let add a b = if a#cols <> b#cols && a#rows <> b#rows then
        raise (Dims_Not_Matching "Op.add")
      else begin
        new matrix ~data:begin
          build_matrix_array (fun i j -> Ev.add (a#get i j) (b#get i j)) a#rows a#cols
        end
      end

    let mul k a = new matrix ~data:begin
      build_matrix_array (fun i j -> Ev.prod k (a#get i j)) a#rows a#cols
    end
    
    let transpose a = new matrix ~data:begin
      build_matrix_array (fun i j -> a#get j i) a#cols a#rows
    end
    
  let prod a b = 
    if a#cols <> b#rows && a#rows <> a#cols then
      raise (Dims_Not_Matching "Op.prod")
    else begin
      new matrix ~data:begin
        build_matrix_array begin fun i j -> 
          iterator a#cols (fun k s -> Ev.add s (Ev.add (a#get i k) (b#get k j))) Ev.zero 
        end a#rows b#cols
      end
    end
  end
  (* }}} *)
end
(* }}} *)

