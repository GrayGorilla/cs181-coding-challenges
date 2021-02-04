(* CSE341, HW3 Provided Code *)

datatype pattern = WildcardP
                 | VariableP of string
                 | UnitP
                 | ConstantP of int
                 | ConstructorP of string * pattern
                 | TupleP of pattern list

datatype valu = Constant of int
              | Unit
              | Constructor of string * valu
              | Tuple of valu list

fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            WildcardP         => f1 ()
          | VariableP x       => f2 x
          | ConstructorP(_,p) => r p
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = AnythingT
             | UnitT
             | IntT
             | TupleT of typ list
             | DatatypeT of string

(**** you can put all your code here ****)

(* 1 *)
fun only_lowercase strList = List.filter (fn str => Char.isLower (String.sub (str, 0))) strList;

(* 2 *)
fun longest_string1 strList = 
    List.foldl (fn (item, longest) => 
        if String.size item > String.size longest then item else longest
    ) "" strList
;

(* 3 *)
fun longest_string2 strList = 
    List.foldl (fn (item, longest) => 
        if String.size item >= String.size longest then item else longest
    ) "" strList
;

(* 4 *)
fun longest_string_helper isLonger strList = 
    List.foldl (fn (item, longest) => if isLonger (item, longest) then item else longest) "" strList
;

val longest_string3 = fn strList => 
    longest_string_helper (fn (a, b) => String.size a > String.size b) strList
;

val longest_string4 = fn strList => 
    longest_string_helper (fn (a, b) => String.size a >= String.size b) strList
;
