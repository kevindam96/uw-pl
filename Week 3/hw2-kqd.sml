(* Kevin Dam, Coursera PL, HW2 Solution Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* 1.  This problem involves using first-name substitutions to come up with alternate names. For example,
       Fredrick William Smith could also be Fred William Smith or Freddie William Smith. Only part (d) is
       specifically about this, but the other problems are helpful. *)

(* 1.(a) Write a function all_except_option, which takes a string and a string list. Return NONE if the
         string is not in the list, else return SOME lst where lst is identical to the argument list except the string
         is not in it. You may assume the string is in the list at most once. Use same_string, provided to you,
         to compare strings. Sample solution is around 8 lines. *)
fun all_except_option(s : string, los : string list) = (* string list option *)
    let 
      fun all_except_option_nonempty(s : string, los : string list) = (* string
        list *)
        case los of
             [] => [] (* should never happen *)
           | x :: [] => if same_string(x, s)
                        then []
                        else x :: []
           | x :: rest => if same_string(x, s)
                          then rest
                          else x :: all_except_option_nonempty(s, rest)
    in
      case los of
           [] => NONE
         | _ => SOME(all_except_option_nonempty(s, los))
    end

(* 1.(b) Write a function get_substitutions1, which takes a string list list (a list of list of strings, the
         substitutions) and a string s and returns a string list. The result has all the strings that are in
         some list in substitutions that also has s, but s itself should not be in the
         result. *)
fun contains(str:string, strs:string list) = (* bool *)
  case strs of
       [] => false
     | x :: rest => if same_string(x, str)
                    then true
                    else contains(str, rest)

fun get_substitutions1(subs : string list list, s: string) = (* string list *)
  case subs of
       [] => []
     | x :: [] => if contains(s, x)
                  then case all_except_option(s, x) of
                            NONE => []
                          | SOME subs_for_x => subs_for_x
                  else []
     | x :: rest => if contains(s, x)
                    then case all_except_option(s, x) of
                              NONE => get_substitutions1(rest, s)
                            | SOME subs_for_x => subs_for_x @
                            get_substitutions1(rest, s)
                    else get_substitutions1(rest, s)

(* 1.(c) Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive
         local helper function. *)
fun get_substitutions2(subs: string list list, s: string) = (* string list *)
    let 
      fun get_substitutions2_helper(subs: string list list, s: string, acc: string
            list) = (* string list *)
        case subs of
             [] => acc
           | x :: [] => if contains(s, x)
                        then case all_except_option(s, x) of
                                  NONE => acc
                                | SOME subs_for_x => acc @ subs_for_x
                        else acc
           | x :: rest => if contains(s, x)
                          then case all_except_option(s, x) of
                                    NONE => get_substitutions2_helper(rest, s,
                                    acc)
                                  | SOME subs_for_x =>
                                    get_substitutions2_helper(rest, s,
                                    acc @ subs_for_x)
                          else get_substitutions2_helper(rest, s, acc)
    in
      get_substitutions2_helper(subs, s, [])
    end

(* 1.(d) Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and
         (c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full
         names (type {first:string,middle:string,last:string} list). The result is all the full names you
         can produce by substituting for the first name (and only the first name) using substitutions and parts (b)
         or (c). The answer should begin with the original name (then have 0 or
         more other names). *)
fun similar_names(subs : string list list, full : {first:string, middle:string,
  last:string}) = (* {first:string, middle:string, last:string} list *)
  let 
    fun build_list(first_names : string list, middle : string, last : string) =
      (* {first:string, middle:string, last:string} list *)
      case first_names of
           [] => []
         | x :: rest => {first=x, middle=middle, last=last} :: build_list(rest,
         middle, last)
  in
    let val {first=f, middle=m, last=l} = full
    in full :: build_list(get_substitutions2(subs, f), m, l)
    end
  end





(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* 2.(a) Write a function card_color, which takes a card and returns its color (spades and clubs are black,
         diamonds and hearts are red). Note: One case-expression is enough. *)
fun card_color c = (* color *)
  case c of
       (Clubs, _) => Black
     | (Diamonds, _) => Red
     | (Hearts, _) => Red
     | (Spades, _) => Black

(* 2.(b)  Write a function card_value, which takes a card and returns its value (numbered cards have their
          number as the value, aces are 11, everything else is 10). Note: One
          case-expression is enough. *)
fun card_value c = (* rank *)
  case c of
       (_, Jack) => 10
     | (_, Queen) => 10
     | (_, King) => 10
     | (_, Ace) => 11
     | (_, Num x) => x

(* 2.(c) Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a
         list that has all the elements of cs except c. If c is in the list more than once, remove only the first one.
         If c is not in the list, raise the exception e. You can compare cards with =. *)
fun remove_card (cs, c, e) = (* card list *)
  case cs of
       [] => raise e       
     | x :: rest => if (x = c)
                    then rest
                    else x :: remove_card(rest, c, e)

(* 2.(d) Write a function all_same_color, which takes a list of cards and returns true if all the cards in the
         list are the same color. Hint: An elegant solution is very similar to one of the functions using nested
         pattern-matching in the lectures. *)
fun all_same_color cards = (* bool *)
  case cards of
       [] => true
     | _ :: [] => true
     | c1 :: c2 :: rest => (card_color c1) = (card_color c2) andalso
     all_same_color (c2 :: rest)

(* 2.(e) Write a function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally
         defined helper function that is tail recursive. (Take “calls use a constant amount of stack space” as a
         requirement for this problem.) *)
fun sum_cards cards = (* int *)
    let 
      fun sum_cards_helper (cards, acc) = (* int *)
        case cards of
             [] => acc
           | x :: rest => sum_cards_helper(rest, acc + (card_value x))
    in
      sum_cards_helper (cards, 0)
    end

(* 2.(f) Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes
         the score as described above. *)
fun score (cards, goal) = (* int *)
    let 
      fun prelim_score (cards, goal) = (* int *)
        if (sum_cards (cards)) > goal
        then 3 * ((sum_cards (cards)) - goal)
        else goal - (sum_cards (cards))
    in
      if all_same_color (cards)
      then prelim_score (cards, goal) div 2
      else prelim_score (cards, goal)
    end

(* 2.(g) Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list
         (what the player “does” at each point), and an int (the goal) and returns the score at the end of the
         game after processing (some or all of) the moves in the move list in order. Use a locally defined recursive
         helper function that takes several arguments that together represent the current state of the game. As
         described above:
         • The game starts with the held-cards being the empty list.
         • The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
         • If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
           not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
           exception.
         • If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes
           the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues
           with a larger held-cards and a smaller card-list. *)
fun officiate (cards, moves, goal) = (* int *)
    let
      fun officiate_helper (hand, cards, moves, goal) = (* int *)
        case (hand, cards, moves, goal) of
             (h, _, [], g) => score (h, g)
           | (h, c :: c_rest, Draw :: m_rest, g) => officiate_helper (c :: h,
           remove_card ((c :: c_rest), c, IllegalMove), m_rest, g)
           | (h, [], Draw :: m_rest, g) => score (h, g)
           | (h, c, Discard m :: m_rest, g) => officiate_helper (remove_card(h,
           m, IllegalMove), c, m_rest, g)
    in
      officiate_helper ([], cards, moves, goal)
    end

(* 3.(a) Write score_challenge and officiate_challenge to be like their non-challenge counterparts except
         each ace can have a value of 1 or 11 and score_challenge should always return the least (i.e., best)
         possible score. (Note the game-ends-if-sum-exceeds-goal rule should apply only if there is no sum that
         is less than or equal to the goal.) Hint: This is easier than you might think. *)
fun score_challenge (cards, goal) = (* int *)
    let 
      fun card_value_no_ace c = (* int *)
        case c of
             (_, Jack) => 10
           | (_, Queen) => 10
           | (_, King) => 10
           | (_, Num x) => x
      fun best_prelim_score (cards, goal) = (* int *)
        let
          fun count_aces (cards) = (* int *)
            case cards of
                 [] => 0
               | x :: rest => case x of
                                   (_, Ace) => 1 + count_aces(rest)
                                 | _ => count_aces(rest)
          fun sum_non_aces (cards) = (* int *)
            case cards of
                 [] => 0
               | x :: rest => case x of
                                   (_, Ace) => sum_non_aces(rest)
                                 | _ => card_value_no_ace (x) + sum_non_aces
                                 (rest)
          fun ace_summer (goal, num_ace, acc) = (* int *) 
            if num_ace = 0
            then acc
            else if acc + (num_ace * 11) <= goal
            then ace_summer (goal, num_ace - 1, acc + 11)
            else ace_summer (goal, num_ace - 1, acc + 1)
          fun best_prelim_score_helper (goal, num_ace, acc) = (* int *)
            if (ace_summer (goal, num_ace, acc)) > goal
            then 3 * ((ace_summer (goal, num_ace, acc)) - goal)
            else goal - (ace_summer (goal, num_ace, acc))
        in
          best_prelim_score_helper (goal, count_aces
          (cards), sum_non_aces (cards))
        end
    in
      if all_same_color (cards)
      then best_prelim_score (cards, goal) div 2
      else best_prelim_score (cards, goal)
    end


















