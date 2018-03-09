(* this is a test SML document*)

(*
fun f [] = [] (*a*)
| f (x::xs) = (x + 1) :: (f xs) (*b*);
*)

(* Datatype Set *)
datatype 'a set = Empty | Set of 'a * 'a set

(* isMember *)
fun isMember(e, Set(element, otherSet)) =
  if e = element then true
  else if otherSet = Empty then false
  else isMember(e, otherSet);

fun list2Set [] = Empty
  | list2Set (x::xs) = Set(x, list2Set(xs));
