(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Andrew Beck
* drew.beck33@gmail.com
*
***************************************************************)
(* Changing the default print depth*)
Control.Print.printDepth := 100;

(* Define your data type and functions here *)
(* Datatype definition *)
datatype 'element set = Empty | Set of 'element * 'element set;

(* Warmup function *)
fun f [] = []
  | f (x::xs) = (x + 1) :: (f xs);

(* Function which checks a given set for a given value and returns a boolean of the result *)
fun isMember e Empty = false
  | isMember e (Set(x, xs)) =
    if e = x then true
    else isMember e xs;

(* Function to convert a list to a set *)
fun list2Set L = foldr (fn (x, xs) => if (isMember x xs) then xs else Set(x, xs)) Empty L;

(* Function which returns the union of two sets *)
fun union Empty Empty = Empty
  | union set1 Empty = set1
  | union Empty set2 = set2
  | union (Set(s1, s1s)) (Set(s2, s2s)) =
    if (isMember s1 (Set(s2, s2s))) then union s1s (Set(s2, s2s))
    else union s1s (Set(s1, Set(s2, s2s)));

fun intersect Empty Empty = Empty
  | intersect set1 Empty = Empty
  | intersect Empty set2 = Empty
  | intersect (Set(s1, s1s)) (Set(s2, s2s)) =
    if (isMember s1 (Set(s2, s2s))) then (Set(s1, (intersect s1s (Set(s2, s2s)))))
    else intersect s1s (Set(s2, s2s));

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

val test = Set ("1", Set("2", Set("3", Empty)));

isMember "one" test;

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
