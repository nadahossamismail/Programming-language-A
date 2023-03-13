(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
			       
fun only_capitals lst=
    List.filter (fn x => Char.isUpper(String.sub(x,0))) lst
		
(* 2 *)

fun longest_string1 lst=
    foldl (fn(x,y)=> if String.size x >  String.size y
		     then x
		     else y) "" lst

(* 3 *)
	  
fun  longest_string2 lst=
    foldl (fn(x,y)=> if String.size x >= String.size y
		     then x
		     else y) "" lst

(* 4 *)
	  
fun longest_string_helper f lst =
    foldl (fn (x,y)=> if f(String.size x,String.size y) then x else y )"" lst
	  
val longest_string3 = longest_string_helper (fn(x,y)=>x>y)
val longest_string4 = longest_string_helper (fn(x,y)=>x>=y)
(* 5 *)
					    
val longest_capitalized = longest_string1 o only_capitals
				
(* 6 *)

val rev_string =implode o rev  o explode

(* 7 *)
				     
fun first_answer f lst =
    case lst of
	[]=> raise NoAnswer
       |x::xs=> case f x of
		    SOME x => x
		   |NONE=> first_answer f xs
					
(* 8 *)					
					
fun all_answers f lst =
    let fun ans (f,lst,acc)=
	    case lst of
		[]=> SOME acc
	       |x::xs=> case f x of
			    NONE => NONE
			 |SOME x => ans (f,xs,x@acc)
               
    in ans (f,lst,[])
    end
	
(* 9a *)

fun count_wildcards p =
    g (fn x => 1) (fn x =>0) p
    
(* 9b *)

fun count_wild_and_variable_lengths p =
    g (fn x => 1) (fn x =>String.size x) p 
    
(* 9c *)
fun  count_some_var (str,p)=
     g (fn x => 0) (fn x => if str = x then 1 else 0) p
       
(* 10 *)
 
fun check_pat p =
    let	fun all_strings (p, acc)  =
	    let fun tup lst =
		    case lst of
			[]=>acc
		       |p::ps  =>all_strings (p,acc) @ tup ps
	in  case p of
	     Variable x => x :: acc
	 | ConstructorP (x,P)  => all_strings (P,acc)
	 |TupleP ps  => tup ps
	 |_  => acc		    
     end
        fun repeated lst=
	    case lst of
		[] =>  true
	       |x::xs  => not( List.exists (fn i => i = x) xs)
					
    in     repeated ( all_strings( p, []))
    end	
						       
(* 11 *)							       
fun match (v,p) =
    case (v,p) of
        (_,Wildcard) => SOME []
       |(v,Variable s)=> SOME [(s,v)]
       |(Unit ,UnitP)=> SOME []
       |(Const n , ConstP N) => if n = N
				then SOME []
				else NONE
       |(Tuple vs ,TupleP ps)   => if List.length vs = List.length ps
				   then all_answers match (ListPair.zip(vs,ps))
				   else NONE
       |(Constructor(s1,v),ConstructorP (s2,p))  => if s1 = s2
						    then match(v,p)
						    else NONE
      
       |_  => NONE
		  
		
		  
(* 12 *)
		 
fun first_match v ps=
    case ps of
	[]=>NONE
      | p::ps => case match(v,p) of
		     NONE => first_match v ps
		  |  SOME x => SOME x
