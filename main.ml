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
	| Parallele (Skip, SubProcess(a, p)) | Parallele (SubProcess(a, p), Skip) -> Node [(a, Skip), Node []]
	| SubProcess (a, p) -> Node [(a, p), (processToTree p)]
	| Plus ((a, p), (b, q)) -> Node [(a, p), (processToTree p); (b, q), (processToTree q)]
	| Parallele (p1, p2) ->
		let rec aux1 arbre1 proRef = match arbre1 with 
			| Node [] -> []
			| Node (((a,p), tree)::t) -> ((a, Parallele (p, proRef)), processToTree(Parallele (p, proRef)))::(aux1 (Node t) proRef)
		in let rec aux2 pro1 pro2 = match pro1, pro2 with 
			| SubProcess (Action s1, p), SubProcess (Coaction s2, q) when s1 = s2-> 
                                        [(Silent, Parallele (p, q)), processToTree (Parallele (p, q))]
			| SubProcess (Coaction s1, p), SubProcess (Action s2, q) when s1 = s2-> 
                                        [(Silent, Parallele (p, q)), processToTree (Parallele (p, q))]
			| _ -> []
		in Node ((aux1 (processToTree(p2)) p1)@(aux1 (processToTree(p1)) p2)@(aux2 p1 p2));;

let u = SubProcess(Action "tea",SubProcess(Action "coin", Skip));;
let m = SubProcess(Coaction "tea",SubProcess(Coaction "coin", Skip));;
let t = Parallele (u, m);;

(*-- PART 4 --*)
let m1 = SubProcess(Action "coin", Plus((Action "tea", Skip), (Action "coffee", Skip)));;
let m2 = Plus((Action "coin", SubProcess(Action "tea", Skip)), (Action "coin", SubProcess(Action "coffee", Skip)));;
(* Example PART 5*)
let m3 = Plus((Action "coin", Skip), (Action "coin", SubProcess(Action "tea", Skip)));; 
let m4 = SubProcess(Action "coin", SubProcess(Action "tea", Skip));;
(* Q < P if for every action a in process P leading to P', the action a is also in process Q leading to Q' and Q' < P' *)
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

(* Thank you for your attention, we hope you enjoyed our homework. *)
(* We are waiting for your feedbacks. *)
(* Have a nice day. *)
(* Adios amigos. *)
