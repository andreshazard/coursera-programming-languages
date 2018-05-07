fun is_older(date1: (int*int*int), date2: (int*int*int)) =
  let
    val date1_list = [#1 date1, #2 date1, #3 date1]
    val date2_list = [#1 date2, #2 date2, #3 date2]
    fun compare (xs : int list, ys : int list) =
      if hd xs <  hd ys then true
      else if hd xs > hd ys then false
      else if tl xs = [] then false
      else compare(tl xs, tl ys)
  in
    compare(date1_list, date2_list)
  end

fun number_in_month(dates: (int*int*int) list, month: int) =
	let
		fun update_result(dates: (int*int*int) list, result: int) =
			if #2 (hd dates) = month then result + 1 else result

		fun process_dates(dates: (int*int*int) list, result: int) =
			if null (tl dates)
			then update_result(dates, result)
			else process_dates(tl dates, update_result(dates, result))
	in
		process_dates(dates, 0)
	end

fun number_in_month_2(dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month) 

fun number_in_months(dates: (int*int*int) list, months: int list) =
  let
    fun add_to_result(months: int list,  result: int) =
      if null months orelse null dates then result
      else if null (tl months) then result + number_in_month(dates, hd months)
      else add_to_result(tl months, result + number_in_month(dates, hd months))
  in
    add_to_result(months, 0)
  end

fun dates_in_month(dates: (int*int*int) list, month: int) =
	let
		fun update_result(date: (int*int*int), result: (int*int*int) list) =
			if #2 date = month then result @ [date] else result

		fun process_dates(dates: (int*int*int) list, result: (int*int*int) list) =
			if null (tl dates)
			then update_result(hd dates, result)
			else process_dates(tl dates, update_result(hd dates, result))
	in
		process_dates(dates, [])
	end

fun dates_in_months(dates: (int*int*int) list, months: int list) =
  if null months orelse null dates then []
  else let
	  fun add_to_result(months: int list, result: (int*int*int) list) =
      if null (tl months)
      then result @ dates_in_month(dates, hd months)
      else add_to_result(tl months, result @ dates_in_month(dates, hd months))
  in
	  add_to_result(months, [])
  end

fun get_nth(xs: string list, nth: int) =
  let
    fun travel_list(xs: string list, index: int) =
      if index = nth then hd xs
      else travel_list(tl xs, index + 1)
  in
    travel_list(xs, 1)
  end

fun get_nth_2 (lst : string list, n : int) =
    if n=1
    then hd lst
    else get_nth(tl lst, n-1)

fun date_to_string(date: (int*int*int)) =
  let
    val dates_list = ["January", "February", "March", "April", "May", "June", "July", "August",
    "September", "October", "November", "December"]

    val month = get_nth(dates_list, #2 date)
    val day = Int.toString (#3 date)
    val year = Int.toString (#1 date)
  in
    month ^ " " ^ day  ^ ", " ^ year
  end

fun number_before_reaching_sum(sum: int, numbers: int list) =
  let
    fun travel_list(numbers: int list, index: int,  acc: int) =
      if acc >= sum then index - 1
      else travel_list(tl numbers, index + 1, (acc + hd (tl numbers)))
    in
      travel_list(numbers, 1, hd numbers)
    end

fun what_month(day: int) =
  let
    val day_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, day_in_months) + 1
  end

fun month_range(range: (int*int)) =
  if #1 range > #2 range then [0]
  else let
    fun process_range(day: int, result: int list) =
      if day >= #2 range then result
      else process_range(day + 1, result @ [what_month(day + 1)])
  in
    process_range(#1 range, what_month(#1 range) :: [])
  end

fun oldest(dates: (int*int*int) list) =
  if dates = ([]) then NONE
  else if tl dates = [] then SOME (hd dates)
  else let
        fun return_oldest_date(date1: (int*int*int), date2: (int*int*int)) =
          if is_older(date1, date2) then date1
          else date2

        fun compare(list_dates_candidates: (int*int*int) list, oldest: (int*int*int)) =
          if null (tl list_dates_candidates) then SOME (return_oldest_date(oldest, hd list_dates_candidates))
          else compare(tl list_dates_candidates, return_oldest_date(oldest, hd list_dates_candidates))
       in
        compare(dates, hd dates)
       end

fun contains(xs: int list, x: int) =
  if null xs then false
  else if null (tl xs) then hd xs = x
  else if hd xs = x then true
  else contains(tl xs, x)

fun remove_duplicates(xs: int list, result: int list) =
  if null (tl xs) then
    if contains(result, hd xs) then result
    else result @ [hd xs]
  else
    if contains(result, hd xs) then remove_duplicates(tl xs, result)
    else remove_duplicates(tl xs, result @ [hd xs])

fun number_in_months_challenge(dates: (int*int*int) list, months: int list) =
  number_in_months(dates, remove_duplicates(months, []))

fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) =
  dates_in_months(dates, remove_duplicates(months, []))

fun reasonable_date(date: (int*int*int)) =
  let
    val months_30 = [4,6,10,11]
    val months_31 = [1,3,5,7,8,9,12]

    fun validate_year(year: int) =
      if year <= 0 then false
      else true

    fun validate_month(month: int) =
      if month <=0 orelse month > 12 then false
      else true

    fun is_leap_year(year: int) =
      if year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0) then true
      else false

    fun validate_day(day: int) =
      if day <= 0 then false
      else if contains(months_30, #2 date) then
        if day < 31 then true
        else false
      else if contains(months_31, #2 date) then
        if day < 32 then true
        else false
      else if #2 date = 2 then
        if is_leap_year(#1 date) then
          if day < 30 then true
          else false
        else
          if day < 29 then true
          else false
      else false


  in
    if validate_year(#1 date) andalso validate_month(#2 date) andalso
      validate_day(#3 date) then true
    else false
  end
