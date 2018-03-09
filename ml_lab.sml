(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Logan Davis
* logand222@gmail.com
*
***************************************************************)

(* Define your data type and functions here *)

(* This function takes every value of a list that is passed in
and adds one to every value. *)
 fun f [] = []
   | f (x::xs) = (x + 1) :: (f xs)

(*Datatype that defines either Empty, or a set with one element
  and another set*)
datatype 'a set = Empty | Set of 'a * 'a set;

(*isMember takes in an element to check and a set, compares the
  passed in element with the first element of the set. If it does
  not match, it calls isMember again on the set within the set until
  it reaches empty.*)
fun isMember (e, Set(element, otherSet)) =
  if e = element then true
  else if otherSet = Empty then false
  else isMember(e, otherSet);

(*list2Set splits a list into the head and the rest of the list,
  puts the head in a set as the first element, then recursively
  calls list2Set again with the rest of the list.*)
fun list2Set [] = Empty
  | list2Set (x::xs) = Set(x,(list2Set(xs)));

(*Returns the union of two sets*)
fun union (Set(element, otherSet), set2) =
  if isMember(element, set2) then union(otherSet, set2) (*Checks if element is in set2. If it is skip it and move on.*)
  else if otherSet = Empty then Set(element, set2) (*If element is not in set2, check if otherSet is empty. If it is, you have reached the end, return the unionized set.*)
  else Set(element,union(otherSet, set2)); (*If otherset isn't empty i.e. not the end, add element to the set, and recursively call union with otherset and set2*)

(*Returns the intersection of two sets*)
fun intersect (Set(element, otherSet), set2) =
  if otherSet = Empty andalso isMember(element, set2) then Set(element, Empty) (*Checks if otherSet is empty (the end) and if element is apart of set2. If it is, finish the set off and add element to it.*)
  else if otherSet = Empty then Empty (*If the program is at the end, but element is not in set2, just return empty and finish the set.*)
  else if isMember(element, set2) then Set(element, intersect(otherSet, set2))(*If element is also in set2, add it to the set and call intersect recursively with otherSet and set2.*)
  else intersect(otherSet, set2); (*Element isn't in set2, and the program isn't at the end of the set, so call intersect with otherSet and set2.*)

(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"];

(* Question 1 *)
f [3, 1, 4, 1, 5, 9]

(* Question 5 *)
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
