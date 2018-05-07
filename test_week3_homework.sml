use "testing.sml";
open SmlTests;

use "week3_homework.sml";

test("all_except_option1",
  assert_true(all_except_option("string",["string"]) = SOME []));

test("all_except_option2",
  assert_true(all_except_option("string",[]) = NONE ));

test("all_except_option3",
  assert_true(all_except_option("string",["pene"]) = NONE ));

test("all_except_option4",
  assert_true(all_except_option("string",["pene","string"]) = SOME ["pene"]));

test("all_except_option5",
  assert_true(all_except_option("string",["string","pene"]) = SOME ["pene"]));

test("all_except_option6",
  assert_true(all_except_option("string",["string","pene", "brazo"]) = SOME ["pene", "brazo"]));

test("get_substitutions1-1",
  assert_equals_string_list(get_substitutions1([["foo"],["there"]], "foo"), []));

test("get_substitutions1-2",
  assert_equals_string_list(get_substitutions1([["foo","asd"],["there"]], "foo"), ["asd"]));

test("get_substitutions1-3",
  assert_equals_string_list(get_substitutions1([["foo","asd"],["there","pene"]], "there"), ["pene"]));

test("get_substitutions2-1",
  assert_equals_string_list(get_substitutions2([["foo"],["there"]], "foo"), []));

test("get_substitutions2-2",
  assert_equals_string_list(get_substitutions2([["foo","asd"],["there"]], "foo"), ["asd"]));

test("get_substitutions2-3",
  assert_equals_string_list(get_substitutions2([["foo","asd"],["there","pene","vagina"]],"there"), ["pene","vagina"]));

test("similar_names1",
  assert_equals_any(similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
    {first="Fred", middle="W", last="Smith"}),  [{first="Fred", last="Smith", middle="W"},
                                                 {first="Fredrick", last="Smith", middle="W"},
                                                 {first="Freddie", last="Smith", middle="W"},
                                                 {first="F", last="Smith", middle="W"}]));
test("similar_names2",
  assert_equals_any(similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
    {first="Pene", middle="W", last="Smith"}),  [{first="Pene", middle="W", last="Smith"}]));

test("card_color1",
  assert_equals_any(card_color(Clubs, Num 2), Black));

test("card_color2",
  assert_equals_any(card_color(Hearts, Jack ), Red));

test("card_value1",
  assert_equals_int(card_value(Clubs, Num 2), 2));

test("card_value2",
  assert_equals_int(card_value(Clubs, Ace), 11));

test("card_value3",
  assert_equals_int(card_value(Clubs, King), 10));

test("remove_card1",
  assert_equals_any(remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove), []));

test("remove_card2",
  assert_equals_any(remove_card([(Hearts, Ace), (Clubs, Jack)], (Hearts,Ace), IllegalMove), [(Clubs, Jack)]));

test("all_same_color1",
  assert_equals_bool(all_same_color [(Hearts, Ace), (Hearts, Ace)], true));

test("all_same_color2",
  assert_equals_bool(all_same_color [(Hearts, Ace), (Spades, Ace)], false));

test("sum_cards1",
  assert_equals_int(sum_cards [(Clubs, Num 2),(Clubs, Num 2)], 4));

test("sum_cards2",
  assert_equals_int(sum_cards [(Clubs, Num 2)], 2));

test("score1",
  assert_equals_int(score ([(Hearts, Num 2),(Clubs, Num 4)],10), 4));

test("score2",
  assert_equals_int(score ([(Clubs, Num 2),(Spades, Num 4)],10), 2));

test("officiate1",
  assert_equals_int(officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15),6));

test("officiate2",
  assert_equals_int(officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],42), 3));




run();
