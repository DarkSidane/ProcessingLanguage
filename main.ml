(*
# Authors : Sidane ALP, Vlad TANASOV
# Date : 23/09/2023
# Subject : Functionnal programming
 *)

(* Hello guys, today we are going to do the homework of functionnal programming. *)
(* Holà, todo recto, *)
(* Please give me bonus point for speaking spanish *)

(*-- PART 1 --*)
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

(*-- PART 2 -- *)
let rec processToTree proc = match proc with 
	| Skip -> Node []
	| SubProcess (a, p) -> Node [(a, p), (processToTree p)]
	| Plus ((a, p), (b, q)) -> Node [(a, p), (processToTree p); (b, q), (processToTree q)]
	| Parallele (Skip, Skip) -> Node [] 
	| Parallele (Skip, SubProcess(a, p)) -> Node [(a, Skip), Node []]
	| Parallele (SubProcess(a, p), Skip) -> Node [(a, Skip), Node []]
	| Parallele (p1, p2) ->
		let rec aux1 arbre1 proRef = match arbre1 with 
			| Node [] -> []
			(*| Node (((a,p), tree)::t) -> ((a, Parallele (p, proRef)), tree)::(aux1 (Node t) proRef)*)
			| Node (((a,p), tree)::t) -> ((a, Parallele (p, proRef)), processToTree(Parallele (p, proRef)))::(aux1 (Node t) proRef)
			(*| Node (((a,p), Node [])::t) -> ((a, Parallele (p, proRef)), Node [])::(aux1 (Node t) proRef)
			| Node (((a,p), (Node (afils, pfils)::tfils))::t) -> ((a, Parallele (p, proRef)), )::(aux1 (Node t) proRef)*)
		in let rec aux2 pro1 pro2 = match pro1, pro2 with 
			| SubProcess (a, p), SubProcess (b, q) -> [(Silent, Parallele (p, q)), processToTree (Parallele (p, q))]
			| _ -> []
		in Node ((aux1 (processToTree(p2)) p1)@(aux1 (processToTree(p1)) p2)@(aux2 p1 p2));;

let u = SubProcess(Action "tea",SubProcess(Action "coin", Skip));;
let m = SubProcess(Coaction "tea",SubProcess(Coaction "coin", Skip));;
let t = Parallele (u, m);;

(*-- PART 3 --*)
(* Verify if Q < P, I think we must use tree because Parallele process will be too long to code.  *)

(*-- PART 3 --*)
(* Verify if Q < P, I think we must use tree because Parallele process will be too long to code.  *)
let simulate q p = 
	let rec aux arbre1 arbre2 = match arbre1, arbre2 with 
		| Node [], Node [] -> true
		| Node [], _ -> false 
		| _, Node [] -> false
		| Node (((action1, process1), tree)::t), Node (((action2, process2), tree2)::t2) -> 
			(*Same action for the brother of a same node*)
			((action1 = action2) || (aux (Node (((action1, process1), tree)::t)) (Node t2)))
			(*And we must verify for the action of the sons*)
			&& (aux tree tree2)
	in aux (processToTree p) (processToTree q);;
let m1 = SubProcess(Action "coin", Plus((Action "tea", Skip), (Action "coffee", Skip)));;
let m2 = Plus((Action "coin", SubProcess(Action "tea", Skip)), (Action "coin", SubProcess(Action "coffee", Skip)));;
let m3 = Plus((Action "coin", Skip), (Action "coin", SubProcess(Action "tea", Skip)));; 
let m4 = SubProcess(Action "coin", SubProcess(Action "tea", Skip));;
