(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	val r = g f1 f2
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals xs = List.filter(fn x => Char.isUpper(String.sub(x,0))) xs

fun longest_string1 xs = List.foldl (fn(x,y) => if (String.size(x) <= String.size(y)) then y else x) "" xs;

fun longest_string2 xs = List.foldl (fn(x,y) => if (String.size(x) < String.size(y)) then y else x) "" xs;

fun longest_string_helper f = List.foldr (fn(x,y) => if f( String.size(x), String.size (y) ) then y else x) ""
val longest_string3 = longest_string_helper ( fn(x,y) => if ( x < y) then true else false)
val longest_string4 = longest_string_helper ( fn(x,y) => if ( x <=  y) then true else false)

val longest_capitalized  = longest_string1 o only_capitals

val rev_string =  String.implode o List.rev o  String.explode

fun first_answer f ls=
    case ls of
    [] => raise NoAnswer
    | x::xs => case f(x) of
                NONE=> first_answer f xs
               |SOME v => v

fun all_answers f lst=
    let
      val has_none = List.exists (fn x=> f(x) = NONE)
      val final_result = List.foldl (fn(x,acc)  => (case f(x) of SOME v => v@acc ))
    in
    case lst of
      [] => SOME []
      |_=>if has_none lst
          then NONE
          else SOME (final_result [] lst)
    end

val count_wildcards = g (fn _ => 1)(fn _ =>0)

fun count_some_var (x,p) = g  (fn _ => 0)(fn str =>if x = str then 1 else 0) p ;

fun check_pat p =
  let
      fun get_str_list p =
         case p of
            Variable x => [x]
           |TupleP ps  => List.foldl (fn (r,i) => get_str_list(r)@i) [] ps
           | _ => []

      fun do_same_exists x = List.exists(fn y => x = y )

      fun check_uniqueness lst =
       case lst of
        [] => true
        | x::xs =>   if (do_same_exists x xs)
                     then false
                     else check_uniqueness xs
  in
    check_uniqueness ( get_str_list p)
  end

fun match (v,p)   =
     case (v,p) of
      (_,Wildcard) => SOME []
     |(Const v1,ConstP p1) =>if v1 = p1 then SOME [] else NONE
     |(Unit,UnitP) =>SOME []
     |(Constructor (s ,v1),ConstructorP (s1, p1) ) => if s = s1 then match(v1,p1) else NONE
     |(Tuple vs,TupleP ps) => if List.length vs = List.length ps
                              then case all_answers match (ListPair.zip(vs,ps))  of
                                    SOME v2=>SOME v2
                                   |_ => NONE
                              else NONE
     |(_, Variable s ) => SOME [(s,v)]
     |(_,_) => NONE

fun first_match v p =
    SOME (first_answer (fn x => match(v,x)) p)
    handle NoAnswer =>NONE

