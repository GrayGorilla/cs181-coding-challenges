(* Helper Function *)
fun get_nth_int(lst: int list, n: int) = 
  if n = 1
  then hd(lst)
  else get_nth_int(tl(lst), n - 1)
;

(* 1 *)
fun is_older(a: (int*int*int), b: (int*int*int)) =
  if (#3 a) < (#3 b) 
  then true 
  else if (#2 a) < (#2 b) andalso (#3 a) = (#3 b)
  then true
  else if (#1 a) < (#1 b) andalso (#2 a) = (#2 b) andalso (#3 a) = (#3 b)
  then true
  else false
;

(* 2 *)
fun number_in_month(dates: (int*int*int) list, month: int) =
  if null dates
  then 0
  else if #2 (hd(dates)) = month
  then 1 + number_in_month(tl(dates), month)
  else 0 + number_in_month(tl(dates), month)
;

(* 3 *)
fun number_in_months(dates: (int*int*int) list, months: int list) =
  if null months
  then 0
  else number_in_month(dates, hd(months)) + number_in_months(dates, tl(months))
;

(* 4 *)
fun dates_in_month(dates: (int*int*int) list, month: int) =
  if null dates
  then []
  else if #2 (hd(dates)) = month
  then hd(dates) :: dates_in_month(tl(dates), month)
  else dates_in_month(tl(dates), month)
;

(* 5 *)
fun dates_in_months(dates: (int*int*int) list, months: int list) =
  if null months
  then []
  else dates_in_month(dates, hd(months)) @ dates_in_months(dates, tl(months))
;

(* 6 *)
fun get_nth(lst: string list, n: int) = 
  if n = 1
  then hd(lst)
  else get_nth(tl(lst), n - 1)
;

(* 7 *)
fun date_to_string(date: (int*int*int)) =
  let
    val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
  in
    get_nth(months, (#2 date)) ^ "-" ^ Int.toString(#1 date) ^ "-" ^ Int.toString(#3 date)
  end
;

(* 8 *)
fun number_before_reaching_sum(sum: int, nums: int list) =
  if sum <= hd(nums)
  then 0
  else 1 + number_before_reaching_sum(sum - hd(nums), tl(nums))
;

(* 9 *)
fun what_month(day: int) =
  let
    val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  in
    number_before_reaching_sum(day, months) + 1
  end
;

(* 10 *)
fun month_range(day1: int, day2: int) =
  let
    val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  in
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + get_nth_int(month_days, what_month(day1)), day2)
  end
;

(* 11 *)
fun oldest(dates: (int*int*int) list) =
  if null(dates)
  then NONE
  else 
    let
      val oldest_answer = oldest(tl(dates));
    in
      if isSome(oldest_answer)
        andalso is_older(valOf(oldest_answer), hd(dates))
      then oldest_answer
      else SOME(hd(dates))
    end
;

(* 12 *)
fun cumulative_sum(nums: int list) =
  if null(nums)
  then []
  else if null(tl(nums))
  then nums
  else
      hd(nums) :: cumulative_sum(hd(nums) + hd(tl(nums)) :: tl(tl(nums)))
;
