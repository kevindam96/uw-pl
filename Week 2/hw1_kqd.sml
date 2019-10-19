(* 1. Write a function is_older that takes two dates and evaluates to true or false. 
*     It evaluates to true if the first argument is a date that comes before the
*     second argument. If the two dates are the same, the result is false.*)
fun is_older (date_1:int*int*int, date_2:int*int*int) = 
    if (#1 date_1) < (#1 date_2) 
    then true
    else if (#2 date_1) < (#2 date_2)
         then true
         else (#3 date_1) < (#3 date_2)

(* 2. Write a function number_in_month that takes a list of dates and a month
*     (i.e., an int) and returns how many dates in the list are in the given month.
* *)
fun number_in_month (dates:int*int*int list, m:int) = 
    let val ans = 0
    in
        if (null dates)
        then ans
        else if (#2 (hd dates)) = m
             then ans + 1
             else number_in_month ((tl dates), m)
    end

(* 3. Write a function number_in_months that takes a list of dates and a list of
      (i.e., an int list) that returns the number of dates in the list of dates
      that are in any of the months in the list of months. Assume the list of
      months has no number repeated. Hint: Use your answer to the previous
      problem. *)
(*TODO*)
