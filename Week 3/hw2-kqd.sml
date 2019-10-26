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




















