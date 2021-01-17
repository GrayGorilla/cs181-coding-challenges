use "hw1.sml";

(* Testing Functions *)
fun val_tester(name, a, b) =
  if a = b
  then "Test " ^ name ^ " Passed!"
  else "Test " ^ name ^ " Failed."
  ;

fun list_tester(name, a, b) = 
  if null(a) andalso null(b)
  then "Test " ^ name ^ " Passed!"
  else if hd(a) = hd(b)
  then list_tester(name, tl(a), tl(b))
  else "Test " ^ name ^ " Failed."
  ;

(* ---------- Tests ---------- *)

val_tester("1.1", is_older((13, 1, 2021), (14, 1, 2021)), true);
val_tester("1.2", is_older((15, 1, 2021), (14, 1, 2021)), false);
val_tester("1.3", is_older((14, 1, 2021), (14, 1, 2021)), false);

val_tester("2.1", number_in_month([(30, 3, 2020), (9, 3, 2021), (12, 4, 2022)], 2), 0);
val_tester("2.2", number_in_month([(30, 3, 2020), (9, 3, 2021), (12, 4, 2022)], 3), 2);
val_tester("2.3", number_in_month([(30, 3, 2020), (9, 3, 2021), (12, 4, 2022)], 4), 1);

val_tester("3.1", number_in_months([(30, 3, 2020), (9, 3, 2021), (12, 4, 2022)], [7, 8, 9]), 0);
val_tester("3.2", number_in_months([(30, 3, 2020), (9, 3, 2021), (12, 4, 2022)], [2, 3, 4]), 3);
val_tester("3.3", number_in_months([(30, 3, 2020), (9, 3, 2021), (12, 4, 2022)], [4, 5, 6]), 1);

list_tester("4.1", dates_in_month([(30, 3, 2020), (9, 3, 2021), (12, 4, 2022)], 2), []);
list_tester("4.2", dates_in_month([(30, 3, 2020), (9, 3, 2021), (12, 4, 2022)], 3), [(30, 3, 2020), (9, 3, 2021)]);
list_tester("4.3", dates_in_month([(30, 3, 2020), (9, 3, 2021), (12, 4, 2022)], 4), [(12, 4, 2022)]);

list_tester("5.1", dates_in_months([(30, 3, 2020), (9, 3, 2021), (12, 4, 2022)], [7, 8, 9]), []);
list_tester("5.2", dates_in_months([(30, 3, 2020), (9, 3, 2021), (12, 4, 2022)], [2, 3, 4]), [(30, 3, 2020), (9, 3, 2021), (12, 4, 2022)]);
list_tester("5.3", dates_in_months([(30, 3, 2020), (9, 3, 2021), (12, 4, 2022)], [4, 5, 6]), [(12, 4, 2022)]);

val_tester("6.1", get_nth(["one", "two", "three"], 1), "one");
val_tester("6.2", get_nth(["one", "two", "three"], 2), "two");
val_tester("6.3", get_nth(["one", "two", "three"], 3), "three");

val_tester("7.1", date_to_string((30, 5, 1998)), "May-30-1998");
val_tester("7.2", date_to_string((2, 7, 2006)), "July-2-2006");
val_tester("7.3", date_to_string((10, 12, 2077)), "December-10-2077");

val_tester("8.1", number_before_reaching_sum(10, [1, 2, 3, 4, 5, 6, 7]), 3);
val_tester("8.2", number_before_reaching_sum(30, [2, 4, 6, 8, 10, 12, 14]), 4);

val_tester("9.1", what_month(33), 2);
val_tester("9.2", what_month(365), 12);

list_tester("10.1", month_range(33, 365), [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]);
list_tester("10.2", month_range(90, 91), [3]);

val_tester("11.1", oldest([(30, 3, 2020), (9, 3, 2021), (12, 4, 2022)]), SOME((30, 3, 2020)));
val_tester("11.2", oldest([(16, 8, 1997), (20, 6, 1984), (1, 10, 1984)]), SOME((20, 6, 1984)));
val_tester("11.2", oldest([(5, 24, 1776), (5, 25, 1776), (5, 23, 1776)]), SOME((5, 23, 1776)));

list_tester("12.1", cumulative_sum([1, 2, 3, 4, 5, 6]), [1, 3, 6, 10, 15, 21]);
list_tester("12.2", cumulative_sum([2, 4, 6, 8, 10]), [2, 6, 12, 20, 30]);
