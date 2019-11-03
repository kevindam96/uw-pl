(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw3-kqd.sml";
(* Problem 1. Tests *)
val test1_1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_2 = only_capitals ["b"] = []
val test1_3 = only_capitals ["A", "b", "C", "d"] = ["A", "C"]
val test1_4 = only_capitals ["a", "B", "c", "D"] = ["B", "D"]

(*Problem 2. Tests *)
val test2_1 = longest_string1 ["A","bc","C"] = "bc"
val test2_2 = longest_string1 [""] = ""
val test2_3 = longest_string1 ["1", "2_", "3__", "4___", "7______", "5____"] =
  "7______"
val test2_4 = longest_string1 ["2_", "3__", "_3_", "__3"] = "3__"

(* Problem 3. Tests *)
val test3_1 = longest_string2 ["A","bc","C"] = "bc"
val test3_2 = longest_string2 [""] = ""
val test3_3 = longest_string2 ["1", "2_", "3__", "4___", "7______", "5____"] =
  "7______"
val test3_4 = longest_string2 ["2_", "3__", "_3_", "__3"] = "__3"

(* Problem 4. Tests *)
val test4_1 = longest_string3 ["A","bc","C"] = "bc"
val test4_2 = longest_string3 [""] = ""
val test4_3 = longest_string3 ["1", "2_", "3__", "4___", "7______", "5____"] =
  "7______"
val test4_4 = longest_string3 ["2_", "3__", "_3_", "__3"] = "3__"

val test4_5 = longest_string4 ["A","bc","C"] = "bc"
val test4_6 = longest_string4 [""] = ""
val test4_7 = longest_string4 ["1", "2_", "3__", "4___", "7______", "5____"] =
  "7______"
val test4_8 = longest_string4 ["2_", "3__", "_3_", "__3"] = "__3"

(* Problem 5. Tests *)
val test5_1 = longest_capitalized ["A","bc","C"] = "A"
val test5_2 = longest_capitalized ["A", "bcde", "fGehafhg", "Abcde" ] = "Abcde"
val test5_3 = longest_capitalized ["a", "bcde", "efghijk"] = ""
val test5_4 = longest_capitalized ["wash", "washing", "washington",
"Washington", "WAshington", "WASHINGTON"] = "Washington"

(* Problem 6. Tests *)
val test6_1 = rev_string "abc" = "cba"
val test6_2 = rev_string "" = ""
val test6_3 = rev_string "Hi there" = "ereht iH"

(* Problem 7. Tests *)
val test7_1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7_2 = ((first_answer (fn x => if Char.isUpper (String.sub (x, 0)) then SOME x
                                    else NONE) ["hi", "there", "world"]); false) handle NoAnswer => true
val test7_3 = first_answer (fn x => if Char.isUpper (String.sub (x, 0)) then SOME
x else NONE) ["hi", "there", "World"] = "World"
val test7_4 = first_answer (fn x => if Char.isUpper (String.sub (x, 0)) then
  SOME x else NONE) ["hi", "There", "World"] = "There"

(*
val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []

*)
