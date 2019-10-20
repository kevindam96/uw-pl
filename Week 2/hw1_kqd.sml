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
fun number_in_month (dates:(int*int*int) list, m:int) = 
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
fun number_in_months (dates:(int*int*int) list, months:int list) =
    let val ans = 0
    in
        if (null months)
        then ans
        else ans + number_in_month (dates, (hd months)) 
                 + number_in_months (dates, (tl months))
    end
(* 4. Write a function dates_in_month that takes a list of dates and a month
*     (i.e., an int) and returns a list holding the dates from the argument list of
*     dates that are in the month. The returned list should contain dates in the
*     order they were originally given. *)
fun dates_in_month (dates:(int*int*int) list, m: int) = 
    let val ans = []
    in
        if (null dates)
        then ans
        else if (#2 (hd dates)) = m
             then (hd dates) :: ans
             else dates_in_month ((tl dates), m)
    end

(* 5. Write a function dates_in_months that takes a list of dates and a list of
      months (i.e., an int list) and returns a list holding the dates from the
      argument list of dates that are in any of the months in the list of
      Assume the list of months has no number repeated. Hint: Use your answer to
      the previous problem and SML's list-append operator (@). *)
fun dates_in_months (dates:(int*int*int) list, months:int list) = 
    let val ans = []
    in
        if (null months)
        then ans
        else dates_in_month (dates, (hd months))
                 @ dates_in_months (dates, (tl months))
    end

(* 6. Write a function get_nth that takes a list of strings and an int n and
*     returns the nth element of the list where the head of the list is 1st. Do
*     not worry about the case where the list has too few elements: your
*     function may apply hd or tl to the empty list in this case, which is okay.*)
fun get_nth (strings:string list, n:int) =
    if n = 1
    then (hd strings)
    else get_nth ((tl strings), n - 1)

(* 7. Write a function date_to_string that takes a date and returns a string of
      the form January 20, 2013 (for example). Use the operator ^ for
      concatenating strings and the library function Int.toString for converting
      an int to a string. For producting the month part, do not use a bunch of conditionals.
      Instead, use a list holding 12 strings and your answer to the previous
      problem. For consistency, put a comma following the day and use
      capitalized English month names: January, February, March, April, May,
      June, July, August, September, October, November, December. *)
fun date_to_string (date:(int*int*int)) = 
    let 
        fun year_to_string (y:int) = 
            Int.toString (y)
        fun month_to_string (m:int) = 
            get_nth (["January", "February", "March", "April", "May", "June",
                        "July", "August", "September", "October", "November", "December"],
                     m)
        fun day_to_string (d:int) = 
            Int.toString (d)
    in  
        month_to_string((#2 date)) ^ " " ^ day_to_string((#3 date)) ^ ", " ^
        year_to_string((#1 date))
    end

(* 8. Write a function number_before_reaching_sum that takes an int called sum,
*     which you can assume is positive, and an int list, which you can assume
*     contains all positive numbers, and returns an int. You should return an
*     int n such that the first n elements of the list add to less than sum, but
*     the first n + 1 elements of the list add to sum or more. Assume the entire
*     list sums to more than the passed in value; it is okay for an exception to
*     occur if this is not the case.*)
fun number_before_reaching_sum (sum:int, nums:int list) = 
    let 
        val index = 0
        val run = 0
        fun number_before_reaching_sum_helper (index:int, run:int, nums:int list) =
            if run + (hd nums) >= sum
            then index
            else number_before_reaching_sum_helper (index + 1, 
                                                    run + (hd nums),
                                                    (tl nums))
    in  
        if run + (hd nums) >= sum
        then index
        else number_before_reaching_sum_helper (index + 1, 
                                                run + (hd nums), 
                                                (tl nums))
    end









