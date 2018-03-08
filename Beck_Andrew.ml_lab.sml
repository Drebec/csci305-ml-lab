(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Andrew Beck
* drew.beck33@gmail.com
*
***************************************************************)
(* Changing the default print depth *)
(* Possibly unnecessary because of the stringify methods *)
(* It also causes the Atom terminal to not display the output properly so I would leave it out *)
(* Control.Print.printDepth := 100; *)

(* Define your data type and functions here *)
(* Datatype definition *)
datatype 'element set = Empty | Set of 'element * 'element set;

(* Warmup function *)
fun f [] = []
  | f (x::xs) = (x + 1) :: (f xs);

(* Function which checks a given set for a given value and returns a boolean of the result *)
fun isMember e Empty = false  (* If is member is ever called on an empty list, return false. This is the base case *)
  | isMember e (Set(x, xs)) = (* The (Set(x, xs)) matches any non-empty set and binds x to the first element and xs to next set, similar to the x::xs notation for lists *)
    if e = x then true  (* Compare e to x, if they match, e exists in the set *)
    else isMember e xs; (* If these don't match, recurse on the "tail" of the set *)

(* Function to convert a list to a set *)
fun list2Set L = foldr (fn (x, xs) => (* Using foldr, iterate through every item in the list and apply f *)
    if (isMember x xs) then xs        (* f uses the isMember function to check if the element 'x' being inserted already exists in the growing set 'xs', and if it does, it is ignored and not added *)
    else Set(x, xs)) Empty L;         (* If the element does not exist append 'x' on to the front of the existing set 'xs' *)
                                      (* foldr begins with an Empty set and iterates through the given list L *)

(* This function iterates through the first given set and compares every element in it to every element in the second given set. *)
(* When a no match is found it adds the element to the second set and continues with the rest of the first set. *)
(* This turns the second set in to the mathematical union of the two sets. *)
fun union Empty Empty = Empty         (* If either input to union is ever Empty, return the othe input (which could also be empty). This is the base case. *)
  | union set1 Empty = set1
  | union Empty set2 = set2
  | union (Set(s1, s1s)) (Set(s2, s2s)) = (* This definition binds to two unempty sets exactly as done in the above isMember function *)
    if (isMember s1 (Set(s2, s2s))) then union s1s (Set(s2, s2s)) (* If s1 exists in the second set it can be ignored by definition of a union and*)
                                                                  (* union can be recursively on the "tail" of the first set and all of the second set *)
    else union s1s (Set(s1, Set(s2, s2s)));                       (* Otherwise the first element of the first set can be added to the second set and then *)
                                                                  (* Union can be called on the "tail" of the first set and this new set which has s1 added to it *)

(* This function iterates through the first given set and compares every element in it to every element in the second given set. *)
(* When a match is detected the element is added to a set which represents the mathematical intersection of the two sets. *)
fun intersect Empty Empty = Empty     (* If either input to intersect is Empty, return Empty. This is the base case *)
  | intersect set1 Empty = Empty
  | intersect Empty set2 = Empty
  | intersect (Set(s1, s1s)) (Set(s2, s2s)) = (* This definition binds to two unempty sets exactly as done above *)
    if (isMember s1 (Set(s2, s2s))) then (Set(s1, (intersect s1s (Set(s2, s2s)))))  (* If s1 exists in the second set, it is part of the intersection *)
                                                                                    (* And s1 can be added to the intersection set. intersect can now be called on the remaining items of the first set*)
    else intersect s1s (Set(s2, s2s));                                              (* Otherwise intersect is called on the "tail" of the first set and all of the second set *)

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
(* list2Set []; *)
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"];

(* Question 1 *)
f [3, 1, 4, 1, 5, 9];

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
