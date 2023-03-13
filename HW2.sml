(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(*
fun all_except_option (str , lst)=
             let fun remaining_lst(str,lst)=
		  case lst of
		      []=>[]
		    | x::xs => if same_string(str,x)
			       then xs
			       else x::remaining_lst(str,xs)
		 fun find_string (str,lst)=
		     case lst of
			 []=>false
			|x::xs  =>if same_string(str,x)
				  then true
				  else find_string(str,xs)
	     in  if find_string(str,lst)
		 then SOME (remaining_lst(str,lst))
		 else NONE
	end
*)		  
fun all_except_option (str, lst)=
    case lst of
	[]=>NONE
       |x::xs  =>if same_string(str,x)
		 then SOME xs
		 else case all_except_option(str,xs) of
			  NONE    =>NONE
			 |SOME y  => SOME (x::y)
					   
fun get_substitutions1 ( lst, str)=
    case lst of
	[]=>[]
       |x::xs => case all_except_option(str,x)of
		     SOME x => x@ get_substitutions1(xs,str)
		    |NONE =>  get_substitutions1(xs,str)
						
fun get_substitutions2 (lst,str)=
  let  fun f (lst,str,acc)=
	    case lst of
		[]=>acc
	       |x::xs  =>case all_except_option(str,x) of
			     SOME x =>f(xs,str, acc@x)
			    |NONE  =>f(xs,str, acc)
    in f(lst,str,[])
    end
	
fun similar_names (lst,{first=fr,middle=m,last=l})=
    let val sub = get_substitutions1(lst,fr)
     in
let  fun f (lst , {first=fr,middle=m,last=l})=
	 case lst of
	     []=>[]
	    |x::xs  =>{first=x,middle=m,last=l}::f(xs,{first=fr,middle=m,last=l})
in
  {first=fr,middle=m,last=l}::f(sub,{first=fr,middle=m,last=l})
     end
end

 (* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank
datatype color = Red | Black
datatype move = Discard of card | Draw 
exception IllegalMove

	  (* put your solutions for problem 2 here *)
	 
fun card_color card=
    case card of
	(suit,_)=> if suit= Spades orelse suit= Clubs
                       then Black
		       else Red
   
fun card_value card =
    case card of
	(_ ,Num n)=> n
      | (_,Ace)=>11
      | _ =>10
		    		
fun remove_card (cs,c,e)=
    case cs of
	[]=>raise e
       |card::cards  => if card = c
			then cards
		        else c::remove_card(cards,c,e)
						   
fun all_same_color lst=
case lst of
  [] => true
  | _::[] => true
  | c1::(c2::cs) =>(card_color c1 = card_color c2 andalso all_same_color (c2::cs))

fun sum_cards lst =
    let fun sum(lst,s) =	    
    case lst of
	[]=> s
        |c::cs =>sum(cs ,s + card_value c)
    in
	sum(lst,0)
    end
	
fun score (lst,goal)=
    let val sum =sum_cards(lst)
	val color = all_same_color lst
	fun preliminary_score (sum , goal)=
	    if sum > goal
	    then (3*(sum-goal))
	    else (goal-sum)
    in
	if color
	then preliminary_score(sum,goal) div 2
	else preliminary_score(sum,goal) 			
end
fun  officiate (card_lst,move_lst,goal)=
let
   fun f (card_lst,move_lst,held_lst,goal)=
	case move_lst of
	    []=>score(held_lst,goal)
	   |m::ms  => case m of
                       	Discard c => f(card_lst,ms, remove_card(held_lst,c, IllegalMove),goal)
		       | Draw => case card_lst of
				     []=>score(held_lst,goal)
				    |c::cs  => if sum_cards(held_lst)>goal
					       then score(held_lst,goal)
                                               else f(cs,ms,c::held_lst ,goal)
in  f(card_lst,move_lst,[],goal)
end

 
    
    
