(**************************************************************)
(* Language Tyrme: expr, lexing and parsing                   *)
(**************************************************************)

(* La definition de l'arbre syntactique des expression Tyrme se trouve
   dans ast.mli *)
open Ast


(* fonction de parsing: prends une expression de Tyrme et retourne
   l'arbre syntactique *)
let parse (s : string) : expr = Parser.main Lexer.token (Lexing.from_string s)


(**************************************************************)
(* Instructions of the MV                                     *)
(**************************************************************)

type instr = 
    Halt
  | Push
  | Apply
  | Printi
  | Acc of int
  | Consti of int
  | Return of int
  | Pop of int
  | BranchIf of int
  | Branch of int
  | Binopi of int
  | Getblock of int
  | Makeblock of int * int
  | Closure of int * int
  | Str of string
  | ArrayPush of int * int


let string_of_instr : instr -> string = fun instr -> match instr with
	| Halt -> "Halt"
	| Push -> "Push"
	| Apply -> "Apply"
	| Printi -> "Print"
	| Acc n -> "Acc "^(string_of_int n)
	| Consti n -> "Const "^(string_of_int n)
	| Return n -> "Return "^(string_of_int n)
	| Pop n -> "Pop "^(string_of_int n)
	| BranchIf n -> "BranchIf "^(string_of_int n)
	| Branch n-> "Branch "^(string_of_int n)
	| Binopi n -> "Binop "^(string_of_int n)
	| Getblock n -> "Getblock "^(string_of_int n)
	| Makeblock (t,n) -> "Makeblock ("^(string_of_int t)^", "^(string_of_int n)^")"
	| Closure(n,o)-> "Closure ("^(string_of_int n)^", "^(string_of_int o)^")"
	| Str s-> "Str "^s
	| ArrayPush(n,r)->"ArrayPush at "^(string_of_int n)^" case "^(string_of_int r)
    
    
    
	
	
	


(**************************************************************)
(* Asm                                                        *)
(**************************************************************)


(* Fonctions de lecture et d'ecriture d'entier 8bits et 32bits *)
let out_i8  (buf : out_channel) (i : int) : unit = output_char buf (char_of_int i)
let out_i32 (buf : out_channel) (i : int) : unit = output_binary_int buf i 

let in_i8   (buf : in_channel) : int = int_of_char (input_char buf)
let in_i32  (buf : in_channel) : int = input_binary_int buf


(* Fonction d'assemblage d'instruction *)
let assemble_instr (buf : out_channel) : instr -> unit = fun instr ->
  let rec write_str s= 
  	if((String.length s)=1) then (out_i8 buf (Char.code(s.[0]))) else (out_i8 buf (Char.code(s.[0]))); write_str (String.sub s 1 (String.length s))
  in match instr with(**failwith "assemblage d'une instruction"**)
  | Halt -> out_i8 buf 0;
  | Push -> out_i8 buf 1;
  | Printi -> out_i8 buf 2;
  | Apply -> out_i8 buf 3;
  | Acc n -> out_i8 buf 4; out_i32 buf n;
  | Consti n -> out_i8 buf 5; out_i32 buf n;
  | Return n -> out_i8 buf 6; out_i32 buf n;
  | Pop n -> out_i8 buf 7; out_i32 buf n;
  | BranchIf n -> out_i8 buf 8; out_i32 buf n;
  | Branch n-> out_i8 buf 9; out_i32 buf n;
  | Getblock n -> out_i8 buf 10; out_i32 buf n;
  | Makeblock(t,n) -> out_i8 buf 11; out_i8 buf t; out_i32 buf n;
  | Closure(n,o)-> out_i8 buf 12; out_i32 buf n; out_i32 buf o;
  | Binopi n -> out_i8 buf 13; out_i8 buf n;
  | ArrayPush(n,o)-> out_i8 buf 15; out_i32 buf n; out_i32 buf o;
  | Str s-> out_i8 buf 14; out_i32 buf (String.length s); write_str s
  	



(* Fonction d'assemblage d'une liste d'instructions *)
let rec assemble (buf : out_channel) : instr list -> unit = fun istrlist ->(**failwith "assemblage d'une liste d'instruction"**)
  match istrlist with
    | []-> ()
    | n::l-> assemble_instr buf n; assemble buf l


(* Ecrite pour vous: une fonction d'assemblage qui ecrit dans un fichier *)
let assemble_filename (name : string) (is : instr list) : unit = 
  let buf = open_out_bin name in
  begin
    assemble buf is;
    close_out buf;
  end

(* fonction de desassemblage: stub *)
let rec disassemble (buf : in_channel) : instr list =
  (* Get the next char, and make sure to capture the end of the file *)
  let inc = (try Some (in_i8 buf) with | End_of_file -> None) in
  (* Test if there were a char *)
  match inc with
  | None   -> []  (* Nope: end of the file *)
  | Some c ->     (* Yep ! Carry on *)
     (*
 
     ici, vous avez recupere un entier 8 bits (la valeur c) qui encode
     une instruction. Derriere, d'autre octets donnent potentiellement
     les arguments. A vous de les recuperer et d'en faire une
     instruction.
      
      *)
	match c with
	  | 0->Halt::disassemble(buf)
	  | 1->Push::disassemble(buf)
	  | 2->Printi::disassemble(buf)
	  | 3->Apply::disassemble(buf)
	  | 4->let r=(in_i32 buf) in Acc(r)::disassemble(buf)
	  | 5->let r=(in_i32 buf) in Consti(r)::disassemble(buf)
	  | 6->let r=(in_i32 buf) in Return(r)::disassemble(buf)
	  | 7->let r=(in_i32 buf) in Pop(r)::disassemble(buf)
	  | 8->let r=(in_i32 buf) in BranchIf(r)::disassemble(buf)
	  | 9->let r=(in_i32 buf) in Branch(r)::disassemble(buf)
	  | 10->let r=(in_i32 buf) in Getblock(r)::disassemble(buf)
	  | 11->let r=(in_i8 buf) and v=(in_i32 buf) in Makeblock(r,v)::disassemble(buf)
	  | 12->let r=(in_i32 buf) and v=(in_i32 buf) in Closure(r,v)::disassemble(buf)
	  | 13->let r=(in_i8 buf) in Binopi(r)::disassemble(buf)
	  | 14->let str = ref "" and n = in_i32 buf and c= ref 0 in 
	    for i=1 to n do
	    	c:=in_i8 buf;
	    	str:=!str^Char.escaped(Char.chr (!c))
	    done; Str(!str)::disassemble(buf)
    | _ ->failwith "Code octet non compatible"


(* Ecrite pour vous: une fonction de desassemblage qui lit d'un fichier *)
let disassemble_filename (name : string) : instr list = 
  let buf = open_in_bin name; in
  let insts = disassemble buf; in
  let _ = close_in buf; in
  insts

(**************************************************************)
(* Machine virtuelle                                          *)
(**************************************************************)

type tag = int

type mot = 
  | MotInt of int
  | PointString of string
  | PointBloc of (tag * (mot list))


let rec string_of_mot : mot -> string = fun mot ->
	let rec strlist : (mot list) -> string = fun liste-> match liste with
	  | [] -> ""
	  | s::n-> (string_of_mot s)^" "^(strlist n)
	in
	match mot with(**failwith "peut-etre un pretty-printer?"**)
    | MotInt n->"MotInt "^(string_of_int n)
    | PointString s ->"PointString "^s
    | PointBloc(t,l)-> "PointBloc "^(string_of_int t)^" ["^(strlist l)^"]"

type mv_state = {
  mutable acc: mot;
  code: instr array;
  mutable pc: int; (* indice de l’instruction courante dans code *)
  stack: mot array;
  mutable sp: int; (* indice du sommet de la pile dans stack *)
}


(* retourne l'accumulateur de l'etat donne en argument *)
let get_acc (s : mv_state) : mot = s.acc


(* Pour construire l'etat de la machine au debut de l'execution *)
let init (c : instr list) : mv_state = {
  code = Array.of_list c;
  stack = Array.make 1000 (MotInt(42));
  pc = 0;
  sp = -1;
  acc=MotInt(52)  	
}


(* Peut-etre une fonction d'impression ? *)
let print_state (s : mv_state) : unit = 
  for i=s.pc to Array.length s.code - 1 do
    print_int i;
    print_string (" - "^(string_of_instr (s.code.(i))^" "));
	print_newline();
  done;
  print_newline();;
            
let valint v= match v with 
                  | MotInt(i)-> i 
                  | _ -> failwith "Unexpected type (Need MotInt)";;

let valstring v= match v with 
                  | PointString(s)-> s
                  | _ -> failwith "Unexpected type (Need PointString)";;

let valpointb v= match v with 
                  | PointBloc(t,l)-> l
                  | _ -> failwith "Unexpected type (Need PointBloc)";;  


(* Affiche les éléments de pile *)
let print_stack (s : mv_state) : unit = 
  for j = 0 to s.sp do
    print_string ("\n   "^string_of_mot(s.stack.(j))^"");
  done;
;;


(* Print array *)
let print_array a : unit = 
  for j = 0 to Array.length a do
    print_string("["^string_of_mot(Array.get a j)^"]");
    done;
;;



(* La fonction d'execution de la machine *)
let machine (s : mv_state) (debug : bool) : mv_state =
  let idx = ref 0 in
  while s.pc < Array.length s.code do
      idx := !idx + 1;
      if debug then print_string ("\n\n=== Step " ^ (string_of_int !idx) ^ " ===\n");
      if debug then print_string ("With PC = " ^ (string_of_int s.pc) ^ " (" ^ (string_of_instr (s.code.(s.pc))) ^ ")\n");
      begin match s.code.(s.pc) with
        | Halt -> 
            s.pc <- Array.length s.stack;
            s.pc <- s.pc +1;

        | Push ->
            s.sp <- s.sp +1;
            s.stack.(s.sp) <- s.acc;
            s.pc <- s.pc +1;

        | Apply -> 
            let l = Array.of_list (valpointb(s.acc)) in
            let old_pc = s.pc +1 in
            s.pc <- valint l.(0);
            let sommet_tmp = s.stack.(s.sp) in
            s.stack.(s.sp) <- MotInt old_pc;
            s.sp <- s.sp + 1;
            for j = ((Array.length l)-1) downto 1
            do
              s.stack.(s.sp) <- l.(j);
              s.sp <- s.sp +1;
            done;
            s.stack.(s.sp) <- sommet_tmp;

        | Printi ->
            print_string (valstring s.acc);
            print_newline ();
            s.pc <- s.pc +1;

        | Acc n ->
            s.acc <- s.stack.(s.sp-n);
            s.pc <- s.pc +1;

        | Consti n ->
            s.acc <- MotInt(n);
            s.pc <- s.pc +1;

        | Return n ->
            s.sp <- s.sp - n;
            s.pc <- valint s.stack.(s.sp);
            s.sp <- s.sp -1;

        | Pop n ->
            if((s.sp-n)<(-1))
              then s.sp<-(-1)
            else
              s.sp <- s.sp - n;
            s.pc <- s.pc +1;

        | BranchIf n ->
          if valint (s.acc) = 0
          then
            s.pc <- s.pc + 1
          else
            s.pc <- s.pc + n

        | Branch n ->
            s.pc <- s.pc + n

        | Binopi n -> begin match n with
                        (* Add *)
                        | 15 ->
                            s.acc <- MotInt(valint(s.stack.(s.sp))+valint(s.acc));
                            s.sp<-s.sp-1;
                            s.pc <- s.pc +1;

                        (* Sub *)
                        | 16 ->
                            s.acc <- MotInt(valint(s.stack.(s.sp))-valint(s.acc));
                            s.sp<-s.sp-1;
                            s.pc <- s.pc +1;

                        (* Mul *)
                        | 17 ->
                            s.acc <- MotInt(valint(s.stack.(s.sp))*valint(s.acc));
                            s.sp<-s.sp-1;
                            s.pc <- s.pc +1;

                        (* Div *)
                        | 18 ->
                            s.acc <- MotInt(valint(s.stack.(s.sp))/valint(s.acc));
                            s.sp<-s.sp-1;
                            s.pc <- s.pc +1;

                        (* Leq *)
                        | 4 ->
                            s.acc <- if s.stack.(s.sp)<=s.acc then MotInt(1) else MotInt(0);
                            s.sp<-s.sp-1;
                            s.pc <- s.pc +1;

                        (* Eq *)
                        | 19 ->
                            s.acc <- if s.acc=s.stack.(s.sp) then MotInt(1) else MotInt(0);
                            s.sp<-s.sp-1;
                            s.pc <- s.pc +1;

                        (* And *)
                        | 6 ->
                            s.acc <- MotInt(valint(s.stack.(s.sp))*valint(s.acc));
                            s.sp<-s.sp-1;
                            s.pc <- s.pc +1;

                        (* Cat *)
                        | 20 ->
                            s.acc <- PointString(valstring(s.stack.(s.sp)) ^ valstring(s.acc));
                            s.sp<-s.sp-1;
                            s.pc <- s.pc +1;

                        | _ -> failwith "Unexpected int";
                      end;

        | Getblock n ->
            let rec elem l n = match l with
              | p::r->if(n=0) then p else elem r (n-1)
              | []->failwith "Array index out of bounds"
            in s.acc<-(elem (valpointb s.acc) n);
            s.pc <- s.pc +1;

        | Makeblock(t, n) ->
            let l = ref [] in 
              for j = 0 to n-1 do
              	 if(t!=3) then
 	             	(l := !l@[s.stack.(s.sp)];
	             	s.sp<-s.sp-1;)
	             else
	               l:=!l@[MotInt 0];	
	           done;
              s.acc<-PointBloc(t,!l);
              s.pc <- s.pc +1;

        | Closure(n, o) ->
            let l = ref [MotInt(s.pc+o)] in 
              for j = 0 to n-1 do
                l := !l@[s.stack.(s.sp-j)];
              done;
              s.acc<-PointBloc(88,!l);
              s.pc <- s.pc +1;

        | Str n ->
            s.acc <- PointString(n);
            s.pc <- s.pc +1;

        | ArrayPush(n,r)->
        	let tab = Array.of_list(valpointb s.stack.(n)) in
        	  tab.(r)<-s.acc;
        	  s.stack.(n)<-PointBloc(3,Array.to_list tab);
        	  s.pc <- s.pc +1;


     end;
    if debug then
    begin
      print_string ("Acc = " ^ string_of_mot s.acc ^"\n");
      print_string ("Stack :");
      print_stack(s);
      print_string ("\nNew state is\n");
      print_state s;
    end;
    done;
    if debug then
    begin
      print_string ("Acc = " ^ string_of_mot s.acc ^"\n");
      print_string ("Stack :");
      print_stack(s);
      print_string ("\n" ^ (string_of_int !idx) ^ " steps in total\n\n");
    end;
    s;;


(* La fonction d'evaluation: retourne l'accumulateur a la fin de l'evaluation *)
let eval (c : instr list) (debug : bool) : mot =
  let s = machine (init c) debug in get_acc s




(**************************************************************)
(* Compilation                                                *)
(**************************************************************)

type env = (var * int) list

let empty_env = []

let repr = function
  | env,ValVar v -> Acc (List.assoc v env)
  | env,Int i -> Consti i
  | env,Bool b -> if b then Consti 1 else Consti 0
  | env,String s -> Str s
  | env,Unit -> Acc 0


let op = function
 | Add -> 15
 | Sub -> 16
 | Mult -> 17
 | Div -> 18
 | Leq -> 4
 | Eq -> 19
 | And -> 6
 | Cat -> 20
 | App -> 8

let print_env env = 
  let rec aux e = match e with
    | [] -> print_newline ()
    | (k, v)::l -> print_string ("["^k^" = "^(string_of_int v)^"]");
                    aux l
  in aux env;;


(* La fonction de compilation *)
let rec compil : env*expr -> instr list = function
  | env, Var s -> 
      [Acc (List.assoc s env)]

  | env, Const n ->
      [repr (env,n)]

  | env, Binop(o,e1,e2)->
      begin match o with 
      | App ->
          let succ (v,i) = (v,i+1) in
          compil (env,e2) @
          [Push] @
          compil ((List.map succ env),e1) @
          [Apply]
      | _ ->
          let succ (v,i) = (v,i+1) in
					compil (env,e2) @
				  [Push] @
					compil (List.map succ env,e1) @
					[Binopi (op o)]
      end;

  | env, If(e1,e2,e3)->
      let i2 = compil (env,e2) in
      let i3 = compil (env,e3) in 
		  compil (env,e1) @
		  [BranchIf (2+List.length i3)] @
		  i3 @
		  [Branch (1+List.length i2)] @
		  i2

  | env, Let(v,e1,e2)->
      let succ (v,i) = (v,i+1) in
		  let new_env = (v,0) :: (List.map succ env) in 
		  compil (env,e1) @
		  [Push] @
		  compil(new_env,e2) @
		  [Pop 1]

  | env, Letf(v1,v2,e1,e2) ->
      let succ (v,i) = (v, i+1) in
      let env_v2 = (v2,0)::(List.map succ env) in
      let env_v1 = (v1,0)::(List.map succ env) in
      let compil_1 = compil (env_v2, e1) in
      let compil_2 = compil (env_v1, e2) in
      [Closure ((List.length env_v1)-1, (List.length compil_2) +4); Push] @
      compil_2 @
      [Pop 1; Branch ((List.length compil_1) +2)] @
      compil_1 @
      [Return (List.length env_v2)]

  |env, Print(e1,e2)->
      compil (env,e1) @
      [Printi] @
      compil (env,e2) @
      [Pop 1]

  |env,Pair(e1,e2) ->
      compil (env,e1) @
      [Push;Push] @
      compil (env,e2) @
      [Push;Makeblock(2,2);Pop 1]

  |env,Fst(e)->
  	compil(env,e) @
      [Getblock 1;Push]
  		

  |env,Snd(e)->
  compil(env,e)	@
      [Getblock 0;Push]
  			

  |env,Array(n)->
  	[Makeblock(3,n)]


  |env,Proj(n,e)->
  compil(env,e)	@
  	[Getblock n;Push]
  	
  




(* Pour lire le codex *)
let lire_codex () = 
  print_string (string_of_mot (eval (disassemble_filename "codex.tm") false))

let codex () =
  let l = disassemble_filename "codex.tm" in
    let rec r u = match u with
      a::h -> print_string ((string_of_instr a)^"\n");
              r h;
      | [] -> ();
    in r l;;
               
               
(* Exemple de compilation qui doit marcher et rendre la valeur 3 *)
let ex_compil () =
  print_string (string_of_mot (eval (compil (empty_env, parse "let x = 1 in x + 2")) false))


(* let addition = compil([], parse "2 + 6");; *)
(* let ifelse = compil([], parse "if 0 == 1 then 10 else 20");; *)
(* let f1 = compil([], parse "let f x = x + 2 in f 3");; *)
(* let f2 = compil([], parse "let f x = x + 5 in 3 * (f (f 2))");; *)
(* let f3 = compil([], parse "let f x = x + 2 in let g y = f y in g 6");; *)
(* let f4 = compil([], parse "let f x = x + 2 in let g x = 3 + f x in let k x = g x + 4 in k 0");; *)
(* let r= Pair(parse "let f x = x + 5 in 3 * (f 2)",parse "let x = 1 in x + 2");; *)
(* let mac = compil([],r);; *)
(* let paire = compil([], parse "let x = (1, 2) in x");; *)
(* let array=[Makeblock (3, 10); Push; Consti 42; Push; ArrayPush(0,2); Pop 1; Acc 0;Pop 1];;*)
(* let proj=[Makeblock (3, 10); Push; Consti 42; Push; ArrayPush(0,2); Pop 1; Acc 0; Pop 1;Getblock 2;Push;Acc 0;Pop 1];; *)