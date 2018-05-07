(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(x ,[]) = NONE
  | all_except_option(x, hd::tl) =
    case same_string(x, hd) of
          true => SOME tl
       |  false => case all_except_option(x, tl) of
                    NONE => NONE
                 |  SOME y => SOME (hd::y)


fun get_substitutions1([], x) = []
  | get_substitutions1(hd::tl, x) =
    case all_except_option(x,hd) of
         NONE => get_substitutions1(tl,x)
      |  SOME y => y @ get_substitutions1(tl,x)


fun get_substitutions2(xs', x) =
  let fun aux([], acc) = acc
        | aux(hd::tl,acc) =
          case all_except_option(x,hd) of
               NONE => get_substitutions2(tl,x)
            |  SOME y => y @ aux(tl,acc)
  in
    aux(xs', [])
  end


fun similar_names (lst, full_name) =
  let fun aux(lst, acc) =
        case lst of
          []    => acc
        | x::xs' => case full_name of
                     {first, middle, last} => aux(xs', acc @ [{first=x, last=last, middle=middle}])
  in
    case full_name of
      {first, middle, last} => aux(first::get_substitutions2(lst, first), [])
  end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

fun card_color card =
  case card  of
       (Clubs, _) => Black
     | (Spades, _ )=> Black
     | (_, _) => Red

fun card_value card =
  case card of
       (_, Num x) => x
     | (_, Ace) => 11
     | (_, _) => 10

fun remove_card(cs, c, e) =
  let
      fun is_same(c1: card,c2: card) =
        c1 = c2

      fun aux([], c) = raise e
        | aux(hd::tl,c) =
          case is_same(hd, c) of
              true => tl
            | false => case aux(tl, c) of
                            [] => []
                          | y => hd::y
  in
    aux(cs,c)
  end

fun all_same_color cs =
  case cs of
       [] => true
     | (c::[]) => true
     | (c::c'::cs') => case card_color c = card_color c' of
                            true => all_same_color(c'::cs')
                          | false => false

fun sum_cards cs =
  let
      fun aux([],acc) = acc
        | aux(cs,acc) =
      case cs of
           [] => acc
         |  c::[] => card_value c + acc
         | c::cs' => aux(cs', acc + card_value c)
  in
    aux(cs,0)
  end

fun score(cs, goal) =
  let
    val sum = sum_cards cs
    val preliminary_score = case sum > goal of
                                 true => 3 * (sum - goal)
                               | false => goal - sum
  in
    case all_same_color cs of
        true => preliminary_score div 2
      | false => preliminary_score
  end

fun officiate(cs, moves, goal) =
  let
    fun play_move(cs, hcs, moves) =
      case moves of
           [] => hcs
         | mv::mv' => case mv of
                          Discard c => play_move(cs, remove_card(hcs, c, IllegalMove), mv')
                        | Draw => case cs of
                                        [] => hcs
                                      | c::cs' => case sum_cards(c::hcs) > goal of
                                                    true => c::hcs
                                                  | false => play_move(cs',(c::hcs),mv')

  in
    score(play_move(cs, [], moves), goal)
  end
