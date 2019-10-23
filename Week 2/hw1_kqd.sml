(* 1. Write a function is_older that takes two dates and evaluates to true or false. 
*     It evaluates to true if the first argument is a date that comes before the
*     second argument. If the two dates are the same, the result is false.*)
fun is_older (date_1:int*int*int, date_2:int*int*int) = 
    if (#1 date_1) < (#1 date_2) 
    then true
    else if (#1 date_1) > (#1 date_2)
         then false
         else if (#2 date_1) < (#2 date_2)
              then true
              else if (#2 date_1) > (#2 date_2)
                   then false
                   else if (#3 date_1) < (#3 date_2)
                        then true
                        else false

(* 2. Write a function number_in_month that takes a list of dates and a month
*     (i.e., an int) and returns how many dates in the list are in the given month.
* *)
fun number_in_month (dates:(int*int*int) list, m:int) = 
    if (null dates)
    then 0
    else if (#2 (hd dates)) = m
         then 1 + number_in_month ((tl dates), m)
         else number_in_month ((tl dates), m)

(* 3. Write a function number_in_months that takes a list of dates and a list of
      (i.e., an int list) that returns the number of dates in the list of dates
      that are in any of the months in the list of months. Assume the list of
      months has no number repeated. Hint: Use your answer to the previous
      problem. *)
fun number_in_months (dates:(int*int*int) list, months:int list) =
  if (null months)
  then 0
  else number_in_month (dates, (hd months)) + number_in_months (dates, (tl months))

(* 4. Write a function dates_in_month that takes a list of dates and a month
*     (i.e., an int) and returns a list holding the dates from the argument list of
*     dates that are in the month. The returned list should contain dates in the
*     order they were originally given. *)
fun dates_in_month (dates:(int*int*int) list, m: int) = 
    if (null dates)
    then []
    else if (#2 (hd dates)) = m
         then (hd dates) :: []
         else dates_in_month ((tl dates), m)

(* 5. Write a function dates_in_months that takes a list of dates and a list of
      months (i.e., an int list) and returns a list holding the dates from the
      argument list of dates that are in any of the months in the list of
      Assume the list of months has no number repeated. Hint: Use your answer to
      the previous problem and SML's list-append operator (@). *)
fun dates_in_months (dates:(int*int*int) list, months:int list) = 
    if (null months)
    then []
    else dates_in_month (dates, (hd months)) @ dates_in_months (dates, (tl months))

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

(* 9. Write a function what_month that takes a day of year (i.e., an int between
*     1 and 365) and returns what month that day is in (1 for January, 2 for
*     February, etc.). Use a list holding 12 integers and your answer to the
*     previous problem. *)
fun what_month (day:int) = 
    number_before_reaching_sum (day, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1

(* 10. Write a function month_range that takes two days of the year day1 and
*      day2 and returns an int list [m1, m2, ..., mn] where m1 is the month of
       day 1, m2 is the month of day1+1, ..., and mn is the month of day day2.
       Note that the result will have length day2 0 day1 + 1 or length 0 if
       day1>day2.  *)
fun month_range (day1:int, day2: int) = 
    let 
        val index = 1
        val length = day2 - day1 + 1
        fun month_range_helper (index:int, day1:int, length:int) = 
            if index = length
            then []
            else what_month (day1 + index) :: month_range_helper (index + 1, day1, length)
    in
        if day1 > day2
        then [] 
        else
            what_month (day1) :: month_range_helper (index, day1, length)
    end

(* 11. Write a function oldest that takes a list of dates and evaluates to an
*      (int*int*int) option. It evaluates to NONE if the list has no dates and
*      SOME d if the date d is the oldest date in the list. *)
fun oldest (dates:(int*int*int) list) =
    if (null dates)
    then
        NONE
    else
        let
            fun oldest_nonempty (dates:(int*int*int) list) = 
                if null (tl dates)
                then (hd dates)
                else
                    let val tl_oldest_date = oldest_nonempty (tl dates)
                    in 
                        if is_older ((hd dates), tl_oldest_date)
                        then (hd dates)
                        else tl_oldest_date
                    end
        in
            SOME (oldest_nonempty (dates))
        end

(* COMMENT OUT 12. SINCE CANNOT TEST IT       
(* 12. Challenge Problem: Write functions number_in_months_challenge and
*      dates_in_months_challenge that are like your solutions to problem 3 and 5
*      except having a month in the second argument multiple times has no more effect
*      than having it once. (Hint: Remove duplicates, then use previous work.) *)
fun number_in_months_challenge (dates:(int*int*int) list, months:int list) = 
    let 
        fun remove_duplicates (months:int list) =
            let 
                val appeared = []
                val new_months = []
                fun contains (nums:int list, n:int) =
                    if (null nums)
                    then false
                    else if (hd nums) = n
                         then true
                         else contains ((tl nums), n)
                fun remove_duplicates_helper (months, appeared, new_months) = 
                    if (null months)
                    then new_months
                    else
                        if contains (appeared, (hd months))
                        then remove_duplicates_helper ((tl months), appeared, new_months)
                        else remove_duplicates_helper ((tl months), 
                                                       (hd months) :: appeared, 
                                                       (hd months) :: new_months)
            in
                if (null months)
                then []
                else remove_duplicates_helper (months, appeared, new_months)
            end
    in
        let
            val new_months = remove_duplicates (months)
        in
            if (null new_months)
            then 0
            else number_in_month (dates, (hd new_months)) + 
                 number_in_months (dates, (tl new_months))
        end
    end
fun dates_in_months_challenge (dates:(int*int*int) list, months:int list) = 
    let 
        fun remove_duplicates (months:int list) =
            let 
                val appeared = []
                val new_months = []
                fun contains (nums:int list, n:int) =
                    if (null nums)
                    then false
                    else if (hd nums) = n
                         then true
                         else contains ((tl nums), n)
                fun remove_duplicates_helper (months, appeared, new_months) = 
                    if (null months)
                    then new_months
                    else
                        if contains (appeared, (hd months))
                        then remove_duplicates_helper ((tl months), appeared, new_months)
                        else remove_duplicates_helper ((tl months), 
                                                       (hd months) :: appeared, 
                                                       (hd months) :: new_months)
            in
                if (null months)
                then []
                else remove_duplicates_helper (months, appeared, new_months)
            end
    in
        let val new_months = remove_duplicates (months)
        in
            if (null new_months)
            then []
            else dates_in_month (dates, (hd new_months))
                 @ dates_in_months (dates, (tl new_months))
        end
    end
COMMENT OUT 12. SINCE CANNOT TEST IT *)

(* 13. Challenge Problem: Write a function reasonable_date that takes a date and
*      determines if it describes a real date in the common era. A "real date"
*      has a positive year (year 0 did not exist), a month between 1 and 12, and
*      a day appropriate for the month. Solutions should properly handle leap
*      years. Leap years are years that are either divisible by 400 or divisible
*      by 4 but not divisible by 100. (Do not worry about days possibly lost in
*      the conversion to the Gregorian calendar in the late 1500s.) *)
fun reasonable_date (date:(int*int*int)) = 
    let
        fun is_leap_year (year:int) =
            (year mod 400 = 0) orelse (year mod 4 = 0 andalso (not (year mod 100 = 0)))
    in
        if (not ((#1 date) > 0))
            then false
            else if (not ((#2 date) > 0 andalso (#2 date) < 13))
                 then false
                 else if (not (is_leap_year (#1 date)))
                      then let
                               val days_in_month = [31, 28, 31, 30, 31, 30, 31,
                                                    31, 30, 31, 30, 31]
                               fun max_days_in_month (month:int) = 
                                   let 
                                       val index = 1
                                       fun max_days_in_month_helper (month:int, 
                                                                     index:int, 
                                                                     days_in_month:int list) =  
                                            if (#2 date) = index
                                            then (hd days_in_month)
                                            else max_days_in_month_helper (month, index + 1, (tl days_in_month))
                                   in  
                                       max_days_in_month_helper (month, index, days_in_month)
                                   end
                           in  
                               not ((#3 date) < 1 orelse (#3 date) > max_days_in_month (#3 date))
                           end
                      else let
                               val days_in_month = [31, 29, 31, 30, 31, 30, 31,
                                                    31, 30, 31, 30, 31]
                               fun max_days_in_month (month:int) = 
                                   let 
                                       val index = 1
                                       fun max_days_in_month_helper (month:int, 
                                                                     index:int, 
                                                                     days_in_month:int list) =  
                                            if (#2 date) = index
                                            then (hd days_in_month)
                                            else max_days_in_month_helper (month, index + 1, (tl days_in_month))
                                   in  
                                       max_days_in_month_helper (month, index, days_in_month)
                                   end
                           in  
                               not ((#3 date) < 1 orelse (#3 date) > max_days_in_month (#3 date))
                           end
    end
