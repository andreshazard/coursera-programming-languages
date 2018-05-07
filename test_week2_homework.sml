use "testing.sml";
open SmlTests;

use "peer_2.sml";

test("is_older: true if second year is older",
  assert_true(is_older((2000, 0, 0), (2010, 0, 0))));

test("is_older: true simple ",
  assert_true(is_older((1,2,3),(2,3,4))));


test("is_older: false if second year is not older",
  assert_false(is_older((2010, 0, 0), (2000, 0, 0))));

test("is_older: true if years are the same but month is older",
  assert_true(is_older((2010, 1, 0), (2010, 2, 0))));

test("is_older: false if years are the same but month is not older",
  assert_false(is_older((2010, 4, 0), (2010, 2, 0))));

test("is_older: true if first year and month are equal but day is older",
  assert_true(is_older((2010, 1, 1), (2010, 1, 3))));

test("is_older: false if first year and month are equal but day is not older",
  assert_false(is_older((2010, 1, 5), (2010, 1, 3))));

test("is_older: false if the dates are equal",
  assert_false(is_older((2010, 0, 0), (2010, 0, 0))));

test("is_older: problem",
  assert_false(is_older((2011,4,28),(2011,3,31))));

test("number_in_month: 1 with two dates and only one month requested",
  assert_equals_int(number_in_month ([(2012,2,28),(2013,12,1)],2), 1));

test("number_in_month: 3 with three dates and three month requested",
  assert_equals_int(number_in_month ([(2012,3,28),(2013,3,1), (2015,3,15)],3), 3));

test("number_in_month: 0 with three dates and no month requested",
  assert_equals_int(number_in_month ([(2012,3,28),(2013,3,1), (2015,3,15)],1), 0));

test("number_in_monts: 3 with four dates and 3 months on the list",
  assert_equals_int(number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]),3));

test("number_in_monts: 0 with four dates and 3 months on the list",
  assert_equals_int(number_in_months([(2012,8,28),(2013,7,1),(2011,7,31),(2011,8,28)],[2,3,4]),0));

test("number_in_monts: 4 with four dates and 1 months on the list",
  assert_equals_int(number_in_months([(2012,8,28),(2013,8,1),(2011,8,31),(2011,8,28)],[8]),4));

test("number_in_monts: 0 with four dates and 0 months on the list",
  assert_equals_int(number_in_months([(2012,8,28),(2013,8,1),(2011,8,31),(2011,8,28)],[]),0));

test("number_in_monts: 0 with no dates and 2 months on the list",
  assert_equals_int(number_in_months([],[2,3,4]),0));

test("dates_in_month one result",
  assert_equals_any(dates_in_month([(2012,2,28),(2013,12,1),(2013,12,1),(2013,12,1)],2),[(2012,2,28)]));

test("dates_in_month two results",
  assert_equals_any(dates_in_month([(2012,2,28),(2013,2,1)],2),[(2012,2,28),(2013,2,1)]));

test("dates_in_month three results",
  assert_equals_any(dates_in_month([(2012,2,28),(2013,2,1),(2018,2,1)],2),[(2012,2,28),(2013,2,1),(2018,2,1)]));

test("dates_in_month with 0 results",
  assert_equals_any(dates_in_month([(2012,2,28),(2013,2,1),(2018,2,1)],0),[]));

test("dates_in_months list with 3 months and 3 results",
  assert_equals_any(dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]),[(2012,2,28),(2011,3,31),(2011,4,28)]));

test("dates_in_months list with 3 months and 0 results",
  assert_equals_any(dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[11,5,6]),[]));

test("dates_in_months list with 1  month and 1 result",
  assert_equals_any(dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,6,28)],[6]),[(2011,6,28)]));

test("dates_in_months list with 1  month and null dates",
  assert_equals_any(dates_in_months([],[6]),[]));

test("dates_in_months list with null  month",
  assert_equals_any(dates_in_months([],[6]),[]));

test("get_nth long string lists get 2nth.",
  assert_equals_string(get_nth (["hi", "there", "how", "are", "you"],2),"there"));

test("get_nth short string list get 1nth",
  assert_equals_string(get_nth (["hi", "dude"],1),"hi"));

test("get_nth one string list get 1nth",
  assert_equals_string(get_nth (["hi"],1),"hi"));

test("date_to_string June 1, 2013",
  assert_equals_string(date_to_string (2013, 6, 1), "June 1, 2013"));

test("date_to_string June 7, 2018",
  assert_equals_string(date_to_string (2018, 6, 7), "June 7, 2018"));

test("number_before_reaching_sum 1",
  assert_equals_int(number_before_reaching_sum (10, [1,2,3,4,5]),3));

test("number_before_reaching_sum 2",
  assert_equals_int(number_before_reaching_sum (15, [1,2,3,4,5]),4));

test("number_before_reaching_sum 3",
  assert_equals_int(number_before_reaching_sum (1, [1,2,3,4,5]),0));

test("number_before_reaching_sum 4",
  assert_equals_int(number_before_reaching_sum (0, [1,2,3,4,5]),0));

test("what_month 1",
  assert_equals_int(what_month 70,3));

test("what_month 2",
  assert_equals_int(what_month 364,12));

test("what_month 3",
  assert_equals_int(what_month 10,1));

test("what_month 4",
  assert_equals_int(what_month 40,2));

test("month_range 1",
  assert_equals_any(month_range (31, 34),[1,2,2,2]));

test("month_range 2",
  assert_equals_any(month_range (59,62),[2,3,3,3]));

test("month_range 3",
  assert_equals_any(month_range (80,62),[0]));

test("oldest 1",
  assert_equals_any(oldest([(2012,2,28),(2011,3,31),(2011,4,28)]),SOME (2011,3,31)));

test("oldest 2",
  assert_equals_any(oldest([]),NONE));

test("oldest 3",
  assert_equals_any(oldest([(2000,2,28),(2011,3,31),(2011,4,28)]),SOME (2000,2,28)));

test("oldest 4",
  assert_equals_any(oldest([(2000,2,28)]),SOME (2000,2,28)));

test("number_in_monts_challenge: 1",
  assert_equals_int(number_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,2]),3));

test("dates_in_months_challenge 1",
  assert_equals_any(dates_in_months_challenge([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,2]),[(2012,2,28),(2011,3,31),(2011,4,28)]));

run();
