(* 1. Write a function is_older that takes two dates and evaluates to true or false. 
*     It evaluates to true if the first argument is a date that comes before the
*     second argument. If the two dates are the same, the result is false.*)

fun is_older (date_1:int*int*int, date_2:int*int*int) = 
  if (#1 date_1) < (#1 date_2) 
  then true
  else 
    if (#2 date_1) < (#2 date_2)
    then true
    else (#3 date_1) < (#3 date_2)

