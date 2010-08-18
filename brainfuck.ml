(*type zipper = {prec : int list; e : int; next:int list};;
let init = {prec = []; e = 0; next = []};;
let apply f zipper = {zipper with e = f zipper.e}
let insert zipper = {prec = zipper.e::zipper.prec; e = 0; next = zipper.next};;
let e = insert e;;
let print_zipper zipper = 
  let print_list = List.iter (Printf.printf "%d ") in
    print_list (List.rev zipper.prec); print_list ([zipper.e]); print_list zipper.next
;;
print_zipper (apply ((+) 3)e );;
*)

let pointeur = ref 0;;
let tbl = Array.make 30000 0;;
let incremente n () = pointeur := !pointeur +n;;
let decremente n () = pointeur := !pointeur -n;;

let get() = tbl.(!pointeur);;

let incremente_octet n () = tbl.(!pointeur) <- get()+n;;
let decremente_octet n () = tbl.(!pointeur) <- get()-n;;

let view () = print_char (char_of_int (get()));;
let read () = tbl.(!pointeur) <- int_of_char (Scanf.scanf "%c" (fun c -> c));;

let init() = 
  pointeur := 0;
  for i = 0 to 49 do
    tbl.(i) <- 0
  done

type expr = 
  | Pp of int
  | Pm of int
  | Pop of int 
  | Pom of int
  | Print
  | Read
  | Razpo
  | Block of bf
and bf = expr list

let rec exec_expr = function
  | Pp n -> incremente n()
  | Pm n -> decremente n ()
  | Pop n -> incremente_octet n()
  | Pom n -> decremente_octet n()
  | Razpo -> tbl.(!pointeur) <- 0
  | Print -> view()
  | Read -> read ()
  | Block code ->
      if get() <> 0 then
	begin
	  exec code; 
	  if get() <> 0 then exec_expr (Block code)
	end
and exec code = List.iter exec_expr code

exception UnclosedP of int
exception UnopenedP of int

let parse = 
  let associations = [('>', Pp 1); ('<', Pm 1); ('+', Pop 1); ('-', Pom 1); ('.', Print); (',', Read)] in
  let assoc_op char = List.assoc char  associations in
    (* Lis le code tant qu'il y a des opérateurs autres que [ et ] *)
    (* utilisation : parse_op s i limit 
       => s code brainfuck, 
       i la position à partir de laquelle on lit le code, 
       limit : position ou l'on s'arrete de lire 
    *)
  let parse_op = 
    let rec parse_op' acc s i limit = 
      if i >= limit then (List.rev acc, limit)
      else
	match s.[i] with
	  | char when List.mem char ['>'; '<'; '+'; '-'; '.'; ','] -> 
	      parse_op' (assoc_op char::acc) s (i+1) limit 
	  | char when not (List.mem char ['['; ']']) ->
	      parse_op' acc s (i+1) limit
	  | _ -> (List.rev acc, i)
    in parse_op' []
  in
    (* parse un block de code *)
  let rec parse_block s i limit = 
    if s.[i] = '[' then
      let r, ni = parse [] s (i+1) limit in
	if ni < limit && s.[ni] = ']' then
	  ([Block r], ni+1)
	else
	  raise (UnclosedP i)
    else
      parse_op s i limit
	(* Apelle parse_block tant qu'il y a des blocks à consommer *)
  and parse acc s i limit = 
    if i >= limit then (acc, limit)
    else 
      let c, ni = parse_block s i limit in
	if ni = i then (acc , i)
	else
	  parse (acc@c) s ni limit
  in
  let parse_code s = 
    let size = String.length s in
    let c, s = parse [] s 0 size in
      if s < size then raise (UnopenedP s)
      else c
  in
    parse_code

let optimise code = 
  let add = function
    | Pp n -> Pp (n+1)
    | Pm n -> Pm (n+1)
    | Pop n ->  Pop (n+1)
    | Pom n ->  Pom (n+1)
    | _ -> failwith "bad code"
  in
  let compare_type = function
    | Pp _, Pp _ 
    | Pm _, Pm _ 
    | Pop _, Pop _ 
    | Pom _, Pom _ -> true
    | _ -> false
  in
  let rle code = 
    let rec rle' e = function
      | [] -> if e <> Pp 0 then [e] else []
      | elem::l when compare_type (e, elem) && List.mem elem [Pp 1; Pm 1; Pop 1; Pom 1] -> rle' (add e) l
      | (Block code)::l -> 
	  let r =  Block (rle' (Pp 0) code) :: (rle' (Pp 0) l) in
	    if e <> (Pp 0) then e::r else r
      | elem::l -> 
	  let r = rle' elem l in
	    if e <> (Pp 0) then e::r else r
    in
      rle' (Pp 0) code
  in
  let rec razpo = function
    | [] -> []
    | Block [Pom 1]::l -> Razpo::(razpo l)
    | Block code::l -> Block (razpo code)::(razpo l)
    | e::l -> e::razpo l
  in
    razpo (rle code)

let read_file file = 
  let c = open_in file in
  let rec aux acc = 
    try
      aux (acc^(input_line c))
    with End_of_file -> acc
  in
    aux ""
;;

let _ = 
  init();
  let code = read_file "test.bf" in
    (*print_endline code;*)
  let bf = optimise (parse code) in
    exec bf
