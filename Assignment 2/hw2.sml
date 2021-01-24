(* CSE 341, HW2 Provided Code *)

(* main datatype definition we will use throughout the assignment *)
datatype json =
         Num of real (* real is what SML calls floating point numbers *)
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list

(* some examples of values of type json *)
val json_pi    = Num 3.14159
val json_hello = String "hello"
val json_false = False
val json_array = Array [Num 1.0, String "world", Null]
val json_obj   = Object [("foo", json_pi), ("bar", json_array), ("ok", True)]

(* some provided one-liners that use the standard library and/or some features
   we have not learned yet. (Only) the challenge problem will need more
   standard-library functions. *)

(* dedup : string list -> string list -- it removes duplicates *)
fun dedup xs = ListMergeSort.uniqueSort String.compare xs

(* strcmp : string * string -> order compares strings alphabetically
   where datatype order = LESS | EQUAL | GREATER *)
val strcmp = String.compare                                        
                        
(* convert an int to a real *)
val int_to_real = Real.fromInt

(* absolute value of a real *)
val real_abs = Real.abs

(* convert a real to a string *)
val real_to_string = Real.toString

(* return true if a real is negative : real -> bool *)
val real_is_negative = Real.signBit

(* We now load 3 files with police data represented as values of type json.
   Each file binds one variable: small_incident_reports (10 reports), 
   medium_incident_reports (100 reports), and large_incident_reports 
   (1000 reports) respectively.

   However, the large file is commented out for now because it will take 
   about 15 seconds to load, which is too long while you are debugging
   earlier problems.  In string format, we have ~10000 records -- if you
   do the challenge problem, you will be able to read in all 10000 quickly --
   it's the "trick" of giving you large SML values that is slow.
*)

(* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important -- it ends the previous binding *)
Control.Print.printDepth := 3;
Control.Print.printLength := 3;

use "parsed_small_police.sml";
use "parsed_medium_police.sml";

(* (* uncomment when you are ready to do the problems needing the large report*)
use "parsed_large_police.sml"; 

val large_incident_reports_list =
    case large_incident_reports of
        Array js => js
      | _ => raise (Fail "expected large_incident_reports to be an array")
*)

(* Now make SML print more again so that we can see what we're working with. *)
; Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 1-8 HERE ****)

(* 1 *)
fun make_silly_json (i: int) = 
  if i = 0 then Array [Null]
  else
    let
      val myJson = Object [
        ("n", Num (int_to_real i)),
        ("b", True)
      ];
      fun appendArray(item, arr) =
        case arr of
            Array [Null] => Array [item]
          | Array a => Array (item :: a)
          | _ => False
        ;
      ;
    in
      appendArray(myJson, make_silly_json(i - 1))
    end
;

(* 2 *)
fun assoc (k, xs) =
  case xs of 
      ((k1, v1) :: xs') => (
        if k1 = k 
        then SOME (v1)
        else assoc (k, xs')
      )
    | _ => NONE
  ;
;

(* 3 *)
fun dot (j, f) =
  case j of
      Object obj => assoc(f, obj)
    | _ => NONE
  ;
;

(* 4 *)
fun one_fields (j) =
  let
    fun aux (j, acc) =
      case j of
        Object ((k1, v1) :: tail) => aux(Object tail, k1 :: acc)
        | _ => acc
    ;
  in
    aux (j, [])
  end
;

(* 5 *)
fun no_repeats (strLst: string list) = if length(strLst) = length(dedup(strLst)) then true else false;

(* 6 *)
fun recursive_no_field_repeats (j) =
  let
    fun arr_aux (a) =
      case a of
          [] => true
        | (head :: tail) => (recursive_no_field_repeats (head) andalso arr_aux (tail))
      ;
    ;
    fun obj_aux (j) = 
      case j of
          [] => true
        | ((_, v) :: tail) => (recursive_no_field_repeats (v) andalso obj_aux (tail))
    ;
  in
    case j of
        Num _ => true
      | String _ => true
      | False => true
      | True => true
      | Null => true
      | Array arr => arr_aux (arr)
      | Object obj => (no_repeats (one_fields (Object obj)) andalso obj_aux(obj))
  end
;

(* 7 *)
fun count_occurrences (lst, expn) = 
  let
    fun in_order (lst) = 
      case lst of
          (first :: second :: tail) => ((strcmp (first, second) <> GREATER)
            andalso in_order (second :: tail))
        | _ => true
      ;
    ;
    fun aux_out (item, outLst) = 
      case outLst of
          [] => (item, 1) :: outLst
        | ((str, cnt) :: tail) => (
          if item = str 
          then (str, cnt + 1) :: tail
          else aux_out (item, tail) @ outLst)
      ;
    ;
    fun aux_in (inLst, outLst) =
      case inLst of
          [] => outLst
        | (head :: tail) => aux_in (tail, aux_out(head, outLst))
      ;
    ;
  in
    if in_order (lst)
    then aux_in (lst, [])
    else raise expn
  end
;

(* histogram and historgram_for_field are provided, but they use your 
   count_occurrences and string_values_for_field, so uncomment them 
   after doing earlier problems *)

(* histogram_for_field takes a field name f and a list of objects js and 
   returns counts for how often a string is the contents of f in js. *)
(*
exception SortIsBroken

fun histogram (xs : string list) : (string * int) list =
  let
    fun compare_strings (s1 : string, s2 : string) : bool = s1 > s2

    val sorted_xs = ListMergeSort.sort compare_strings xs
    val counts = count_occurrences (sorted_xs,SortIsBroken)

    fun compare_counts ((s1 : string, n1 : int), (s2 : string, n2 : int)) : bool =
      n1 < n2 orelse (n1 = n2 andalso s1 < s2)
  in
    ListMergeSort.sort compare_counts counts
  end

fun histogram_for_field (f,js) =
  histogram (string_values_for_field (f, js))
*)

(**** PUT PROBLEMS 9-11 HERE ****)

;Control.Print.printDepth := 3;
Control.Print.printLength := 3;

(**** PUT PROBLEMS 12-15 HERE ****)

;Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 16-19 HERE ****)

(* For CHALLENGE PROBLEMS, see hw2challenge.sml *)

