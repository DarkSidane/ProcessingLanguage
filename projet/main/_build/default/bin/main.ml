(*
# Authors : Sidane ALP, Vlad TANASOV
# Date : 08/10/2023
# Subject : Functionnal programming
 *)

(* Hello guys, today we are going to do the homework of functionnal programming. *)
(* Holà, como estas. todo recto esta mi  *)
(* Please give me bonus point for speaking spanish *)

(*-- PART 2 --*)
type 's action = Action of 's |Coaction of 's | Silent;; 
(*'s is the generic token for symbols
  SILENT is the token for SILENT action
  *)
type 's preProcess = ('s action * 's process) and 
    's process = 
    Skip 
    | SubProcess of 's preProcess 
    | Plus of ('s preProcess * 's preProcess) 
    | Parallele of ('s process * 's process);; 
type 's tree = Node of ('s preProcess * 's tree) list;; 
(* Example TEST*)

(*-- PART 3 -- *)
let rec processToTree proc = match proc with 
	| Skip | Parallele (Skip, Skip) -> Node []
	| Parallele (Skip, SubProcess(a, _)) | Parallele (SubProcess(a, _), Skip) -> Node [(a, Skip), Node []]
	| SubProcess (a, p) -> Node [(a, p), (processToTree p)]
	| Plus ((a, p), (b, q)) -> Node [(a, p), (processToTree p); (b, q), (processToTree q)]
	| Parallele (p1, p2) ->
		let rec aux1 arbre1 proRef = match arbre1 with 
			| Node [] -> []
			| Node (((a,p), _)::t) -> ((a, Parallele (p, proRef)), processToTree(Parallele (p, proRef)))::(aux1 (Node t) proRef)
		in let aux2 pro1 pro2 = match pro1, pro2 with 
			| SubProcess (Action s1, p), SubProcess (Coaction s2, q) when s1 = s2-> 
                                        [(Silent, Parallele (p, q)), processToTree (Parallele (p, q))]
			| SubProcess (Coaction s1, p), SubProcess (Action s2, q) when s1 = s2-> 
                                        [(Silent, Parallele (p, q)), processToTree (Parallele (p, q))]
			| _ -> []
		in Node ((aux1 (processToTree(p2)) p1)@(aux1 (processToTree(p1)) p2)@(aux2 p1 p2));;

let u = SubProcess(Action "tea",SubProcess(Action "coin", Skip));;
let m = SubProcess(Coaction "tea",SubProcess(Coaction "coin", Skip));;
let string_of_action a = match a with
        | Action s -> "Action " ^ s
        | Coaction s -> "Coaction " ^ s
        | Silent -> "Silent";;
let rec string_of_process p = match p with
        | Skip -> "Skip"
        | SubProcess (a, p) -> "SubProcess (" ^ (string_of_action a) ^ ", " ^ (string_of_process p) ^ ")"
        | Plus ((a, p), (b, q)) -> "Plus ((" ^ (string_of_action a) ^ ", " ^ (string_of_process p) ^ "), (" ^ (string_of_action b) ^ ", " ^ (string_of_process q) ^ "))"
        | Parallele (p1, p2) -> "Parallele (" ^ (string_of_process p1) ^ ", " ^ (string_of_process p2) ^ ")";;
(* We will print the tree in the following pattern :
        +--- X
        | +-action- process1 
        | | +-action- process1.1
        | | +-action- process1.2
        | +-action- process2
        | | +-action- process2.1
        | | | +-action- process2.1.1
        | | | | +-action- process2.1.1.1
        | | | | +-action- process2.1.1.2 
        | | | | +-action- process2.1.1.3
        | | | | +-action- process2.1.1.4
        | | | | | +-action- process2.1.1.4.1
        | | | | | +-action- process2.1.1.4.2
        | | | +-action- process2.1.2
        | | | | +-action- process2.1.2.1
        | | | | +-action- process2.1.2.2
        | | | +-action- process2.1.3
        | | +-action- process2.2
        | | | +-action- process2.2.1
        | | | | +-action- process2.2.1.1
        | | | | +-action- process2.2.1.2
        | +-action- process3
        | | +-action- process3.1
        | +-action- process4
        *)
let print_tree tree =
        let rec aux depth tr = match tr with
    | Node [] -> ()
    | Node ((prePro,subTree)::rest) ->
                    begin
                            print_depth depth;
            Printf.printf "+- %s\n" (string_of_preProcess prePro);
            aux (depth + 1) subTree;
            aux depth (Node rest)
                    end
                    and print_depth d =
                            if d > 0 then begin
                                    print_char '|';
        print_string " ";
        print_depth (d - 1)
                    end
                            and string_of_preProcess (ac, proc) =
                                    (string_of_action ac) ^ " -> " ^ (string_of_process proc) in
        aux 0 tree;;
let t = Parallele (u, m);;
print_string "Here is the tree of the process U|M : \n \n";;
print_tree (processToTree t);;
print_string "\n";;


(*-- PART 4 --*)
let m1 = SubProcess(Action "coin", Plus((Action "tea", Skip), (Action "coffee", Skip)));;
let m2 = Plus((Action "coin", SubProcess(Action "tea", Skip)), (Action "coin", SubProcess(Action "coffee", Skip)));;
(* Example PART 5*)
let m3 = Plus((Action "coin", Skip), (Action "coin", SubProcess(Action "tea", Skip)));; 
let m4 = SubProcess(Action "coin", SubProcess(Action "tea", Skip));;
(* Q < P if for every action a in process P leading to P', the action a is also in process Q leading to Q' and Q' < P' *)
print_string "Here is the tree of the process m1 : \n \n";;
print_tree (processToTree m1);;
print_string "\n";;
print_string "Here is the tree of the process m2 : \n \n";;
print_tree (processToTree m2);;
print_string "\n";;
print_string "Here is the tree of the process m3 : \n \n";;
print_tree (processToTree m3);;
print_string "\n";;
print_string "Here is the tree of the process m4 : \n \n";;
print_tree (processToTree m4);;
print_string "\n";;

let is_simulated q p = 
        let rec aux1 tree_q tree_p = match tree_q, tree_p with
        (* Reccursion on tree_q *)
                | Node [], _ -> true
                | _, Node [] -> false
                | Node (((a1, _), tree1)::t1), Node (((a2, _), tree2)::t2) ->
                                ((if a1 = a2 then 
                                        (aux1 tree1 tree2)
                                else 
                                        false) || (aux1 tree_q (Node t2)))
                                && (aux1 (Node t1) tree_p)

in aux1 (processToTree q) (processToTree p);;
print_string "m1 is simulated by m2 : ";;
print_endline (string_of_bool (is_simulated m1 m2));;
print_string "m2 is simulated by m1 : ";;
print_endline (string_of_bool (is_simulated m2 m1));;
print_string "m3 is simulated by m4 : ";;
print_endline (string_of_bool (is_simulated m3 m4));;
print_string "m4 is simulated by m3 : ";;
print_endline (string_of_bool (is_simulated m4 m3));;
(*-- PART 5 --*)
(* Now, we want to know if Q equivalent to P*)
let equivalent q p = 
        let rec aux1 tree_q tree_p = match tree_q, tree_p with
        (* Reccursion on tree_q *)
                | Node [], Node [] -> true
                | Node [], _ -> false 
                | _, Node [] -> false
                | Node (((a1, _), tree1)::t1), Node (((a2, _), tree2)::t2) ->
                                ((if a1 = a2 then 
                                        (aux1 tree1 tree2)
                                else 
                                        false)) || (aux1 tree_q (Node t2))
                                        && (aux1 (Node t1) tree_p)

        in aux1 (processToTree q) (processToTree p);;
print_string "m3 is equivalent to m4 : ";;
print_endline (string_of_bool (equivalent m3 m4));;
(* Thank you for your attention, we hope you enjoyed our homework. *)
(* We are waiting for your feedbacks. *)
(* Have a nice day. *)
(* Adios amigos. *)
