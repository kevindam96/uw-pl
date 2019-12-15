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

(* Problem 8. Tests *)
val test8_1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_2 = all_answers (fn x => if x = 1 then SOME [x] else NONE)
[2,3,4,5,6,1,7] = NONE
val test8_3 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1, 1, 1] =
 SOME [1, 1, 1]
val test8_4 = all_answers (fn x => if Char.isUpper (String.sub (x, 0)) then SOME
[x] else NONE) ["hello", "world", "how", "are", "you"] = NONE
val test8_5 = all_answers (fn x => if Char.isUpper (String.sub (x, 0)) then SOME
[x] else NONE) ["hello", "world", "How", "are", "you"] = NONE
val test8_6 = all_answers (fn x => if Char.isUpper (String.sub (x, 0)) then SOME
[x] else NONE) ["Hello", "World", "How", "Are", "You"] = SOME ["Hello",
                            "World", "How", "Are", "You"]

(* Problem 9. Tests *)
val test9a_1 = count_wildcards Wildcard = 1
val test9a_2 = count_wildcards (TupleP [Wildcard, Wildcard, Wildcard]) = 3
val test9a_3 = count_wildcards (TupleP [Variable "Nice", Variable "HW",
Wildcard]) = 1

val test9b_1 = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_2 = count_wild_and_variable_lengths (TupleP [Wildcard, Wildcard,
(Variable "abc")]) = 5

val test9c_1 = count_some_var ("x", Variable("x")) = 1
val test9c_2 = count_some_var ("x", Wildcard) = 0
val test9c_3 = count_some_var ("abc", (TupleP [Wildcard, Wildcard, (Variable
"abc"), (Variable "bca"), (Variable "abc")])) = 2

(* Problem 10. Tests *)
val test10_1 = check_pat (Variable("x")) = true
val test10_2 = check_pat (TupleP [Wildcard, Wildcard, Variable("x"),
Variable("y")]) = true
val test10_3 = check_pat (TupleP [Wildcard, Wildcard, Variable("x"),
Variable("x")]) = false

(* Problem 11. Tests*)
val test11_1 = match (Const(1), UnitP) = NONE
val test11_2 = match ((Tuple [Const(0), Const(0)]),
                      (TupleP [ConstP(0), ConstP(0)])) = SOME []
val test11_3 = match (Const(3), Wildcard) = SOME []
val test11_4 = match (Const(3), Variable("three")) = SOME [("three", Const(3))]

(* Problem 12. Tests *)
val test12_1 = first_match Unit [UnitP] = SOME []
val test12_2 = first_match Unit [ConstP(2), ConstP(3), ConstP(4), UnitP] = SOME
[]
val test12_3 = first_match Unit [ConstP(2), ConstP(3), ConstP(4)] = NONE
