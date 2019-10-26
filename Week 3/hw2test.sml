(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1a_1 = all_except_option ("string", ["string"]) = SOME []
val test1a_2 = all_except_option ("hello", ["hi", "hey", "hello", "sup"]) =
  SOME ["hi", "hey", "sup"]
val test1a_3 = all_except_option ("hello", []) = NONE
val test1a_4 = all_except_option ("hello", ["hi"]) = SOME ["hi"]

val contains_test1 = contains ("hello", ["hey", "hi", "sup"]) = false
val contains_test2 = contains ("hello", []) = false
val contains_test3 = contains ("hello", ["hey", "hi", "hello", "sup"]) = true
val contains_test4 = contains ("hello", ["hello"]) = true

val test1b_1 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test1b_2 = get_substitutions1 ([["Fred", "Fredrick"], ["Elizabeth", "Betty"],
                                                         ["Freddie", "Fred",
                                                         "F"]], "Fred") =
                                                         ["Fredrick", "Freddie",
                                  "F"]
val test1b_3 = get_substitutions1([["Fred", "Fredrick"], ["Jeff", "Jeffrey"],
["Geoff", "Jeff", "Jeffrey"]], "Jeff") = ["Jeffrey", "Geoff", "Jeffrey"]
val test1b_4 = get_substitutions1 ([["Fred", "Fredrick"], ["Elizabeth", "Betty"], [],
                                                         ["Freddie", "Fred",
                                                         "F"]], "Fred") =
                                                         ["Fredrick", "Freddie",
                                  "F"]

val test1c_1 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test1c_2 = get_substitutions2 ([["Fred", "Fredrick"], ["Elizabeth", "Betty"],
                                                         ["Freddie", "Fred",
                                                         "F"]], "Fred") =
                                                         ["Fredrick", "Freddie",
                                  "F"]
val test1c_3 = get_substitutions2([["Fred", "Fredrick"], ["Jeff", "Jeffrey"],
["Geoff", "Jeff", "Jeffrey"]], "Jeff") = ["Jeffrey", "Geoff", "Jeffrey"]
val test1c_4 = get_substitutions2 ([["Fred", "Fredrick"], ["Elizabeth", "Betty"], [],
                                                         ["Freddie", "Fred",
                                                         "F"]], "Fred") =
                                                         ["Fredrick", "Freddie",
                                  "F"]

val test1d_1 = similar_names ([], {first="Aaron", middle="Apple", last="Aardvark"}) = [{first="Aaron", middle="Apple", last="Aardvark"}]
val test1d_2 = similar_names ([["Elizabeth", "Betty"], ["Fred", "Fredrick", "Freddie"], ["Airy", "Double-A", "Aaron", "Big-A"]], {first="Aaron", middle="Apple", last="Aardvark"}) = [{first="Aaron", middle="Apple", last="Aardvark"}, {first="Airy", middle="Apple", last="Aardvark"}, {first="Double-A", middle="Apple", last="Aardvark"}, {first="Big-A", middle="Apple", last="Aardvark"}]
val test1d_3 = similar_names ([["Elizabeth", "Betty"], ["Airy", "Double-A",
"Big-A"], ["Fred", "Fredrick", "Freddie"]], {first="Aaron", middle="Apple",
last="Aardvark"}) = [{first="Aaron", middle="Apple", last="Aardvark"}]
val test1d_4 = similar_names ([["Aaron", "Big-A"]], {first="Aaron",
middle="Apple", last="Aardvark"}) = [{first="Aaron", middle="Apple",
last="Aardvark"}, {first="Big-A", middle="Apple", last="Aardvark"}]
val test1d_5 = similar_names
([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
{first="Fred", middle="W", last="Smith"}) =[{first="Fred", last="Smith",
middle="W"}, {first="Fredrick", last="Smith", middle="W"},
            {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test2a_1 = card_color (Clubs, Num 2) = Black
val test2a_2 = card_color (Diamonds, Num 8) = Red
val test2a_3 = card_color (Hearts, Queen) = Red
val test2a_4 = card_color (Spades, King) = Black

val test2b_1 = card_value (Clubs, Num 2) = 2
val test2b_2 = card_value (Diamonds, Num 8) = 8
val test2b_3 = card_value (Hearts, Queen) = 10
val test2b_4 = card_value (Spades, Ace) = 11
val test2b_5 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             
             
