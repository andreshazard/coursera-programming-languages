use "testing.sml";
open SmlTests;

use "week4_homework.sml";

test("only_capitals:",
  assert_equals_any(only_capitals ["A","B","C"], ["A","B","C"]));

test("only_capitals:",
  assert_equals_any(only_capitals ["a","B","C"], ["B","C"]));

test("only_capitals:",
  assert_equals_any(only_capitals ["a","b","h"], []));

test("longest_string1",
  assert_equals_string(longest_string1 ["A", "bc", "C"], "bc"));

test("longest_string2",
  assert_equals_string(longest_string1 ["A", "bc", "C"], "bc"));

test("longest_string3",
  assert_equals_any(longest_string3 ["A", "bc", "C"], "bc"));

test("longest_string4",
  assert_equals_any(longest_string4 ["A","B","C"], "C"));

test("longest_capitalized",
  assert_equals_any(longest_capitalized ["A", "bc", "C"], "A"));

test("rev_string",
  assert_equals_any(rev_string "abc", "cba"));



run();
