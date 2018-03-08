(* this is a test SML document*)

(* Datatype Set *)


datatype 'a set = Empty | Set of 'a * 'a set

fun isMember(e, Set(element, otherSet)) =
  if e = element then true
  else if otherSet = Empty then false
  else isMember(e, otherSet);

isMember "one" ["1","2","3","4"];

(*
fun f [] = [] (*a*)
  | f (x::xs) = (x + 1) :: (f xs) (*b*);
*)
