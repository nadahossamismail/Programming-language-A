fun is_older (date1: int*int*int , date2: int*int*int)=
    (#1 date1 < #1 date2) orelse ((#1 date1 = #1 date2) andalso (#2 date1 < #2 date2)) orelse ((#1 date1 = #1 date2) andalso (#2 date1 = #2 date2)
											       andalso (#3 date1 < #3 date2))
												  
fun number_in_month (dates :( int*int*int) list  , month : int)=
    if null dates
    then 0
    else if #2 (hd dates)= month
    then 1 + number_in_month(tl dates,month)
     else number_in_month(tl dates , month)
										
fun number_in_months (dates : (int*int*int) list ,months :  int list )=
    if null months
    then 0
    else number_in_month(dates, (hd months))+ number_in_months (dates , (tl months))

fun dates_in_month (dates : (int*int*int) list , month : int)= 
    if null dates
     then []
    else if #2 (hd dates ) = month
    then (#1 (hd dates),month,#3 (hd dates))::dates_in_month(tl dates , month)
    else dates_in_month(tl dates , month)
		       
fun dates_in_months (dates : (int*int*int) list , months : int list)=
    if null months
    then []
    else  dates_in_month (dates , (hd months))@ dates_in_months(dates,(tl months))
							       
fun get_nth (str : string list , n : int )=
    if n = 1
    then hd str
    else get_nth((tl str),n-1)

fun date_to_string (date : (int*int*int) )=
    let val months = ["January","February","March","April","May","June","July","August","Septemper","October","November","December"]
    in
        get_nth(months,#2 date)^" "^ Int.toString(#3 date)^", "^Int.toString(#1 date)
    end
	
fun number_before_reaching_sum (sum: int ,numbers: int list)=
    if sum - (hd numbers)<= (hd (tl numbers))
    then (hd numbers)
    else number_before_reaching_sum(( sum - (hd numbers)),(tl numbers))

fun what_month (day : int )=
    let val days =[(31,1),(28,2),(31,3),(30,4),(31,5),(30,6),(31,7),(31,8),
		   (30,9),(31,10),(30,11),(31,12)]
        fun month (day: int  , days :(int* int) list)=
	    if day - (#1(hd days))<=0
	     then (#2 (hd days))
	   else  if day - (#1(hd days)) <= (#1(hd(tl days)))
	    then(#2( hd (tl days)))
	    else month(day - (#1(hd days)),(tl days))
    in
          month (day,days)
    end

fun month_range (day1: int , day2:int)=
        if day1 > day2
	then []
        else what_month(day1) ::  month_range(day1 +1 , day2)

fun oldest (dates : (int*int*int) list)=
    if null dates
    then NONE
    else
	let fun max (dates : (int*int*int) list)=
		if null (tl dates)
	        then hd dates
	        else let val ans = max (tl dates)
		     in if is_older(hd dates , ans)
			then hd dates
		        else ans
                     end
	in
	    SOME (max dates)
	end
					      
