(*#### Problem Statement #####*)
(*##### SML Program of Tourism - 1 Problem ######*)

(*** Filter Function ***)
fun filter(f,l) =
	if l=[] then []
		else if f(hd l)
			then (hd l)::(filter (f, tl l))
			else filter(f, tl l);			
			
(*** Map function ***)
fun map(f,L) = if (L=[]) then []
else f(hd(L))::(map(f,tl(L)));

(*** Gives the first Elemnt from a tuple of three elements ***)
fun getFirst(tup: int*int*int) = #1(tup);
 
(*** Returns a list where the head of each element equals "n" ***)
fun getNOrder(n, L) = filter((fn (tup) => getFirst(tup) = n), L);

(*** Getting number of people and making its list ***)
fun numPeople(NumberOfPeople:int) =
List.tabulate(NumberOfPeople, fn x => x+1);

(*** L being a list having number of People ***)
fun recurN(L: int list, Pref: (int*int*int) list) = 
if L = [] then [] 
else getNOrder(hd(L),Pref)::recurN(tl(L),Pref);

(*** Removes the head of each element of the list. ***)
fun rem(L:(int*int*int) list) = 
if L = [] then []
else [#2(hd(L)),#3(hd(L))]::rem(tl(L));

(*** Running rem() for all elements of list ***)
fun remAll(L) =
if L = [] then []
else rem(hd(L)) :: remAll(tl(L));

(*** To Concatinate two lists ***)
fun concat(x,y) = if x=[] then y
else hd(x)::concat(tl(x),y);

(***  Using Concat Function  ***)
fun concatCustom(L) =
if L = [] then []
else concat(hd(L),concatCustom(tl(L)));

(*** Removing particular element from the list ***)
fun remove(x,L) = if (L=[]) then []
else if x=hd(L)then remove(x,tl(L))
else hd(L)::remove(x,tl(L));

(***  Removing duplicate elements from the list ***)
fun removedupl(L) =
if (L=[]) then []
else hd(L)::removedupl(remove(hd(L),tl(L)));

(*** Merging a list of lists after removing duplicates ***)
fun remNConcat(L) =
if L = [] then []
else removedupl(concatCustom(hd(L)))::remNConcat(tl(L));

(*** Function to Calculate Permutation ***)
fun myInterleave(x,[]) = [[x]]
| myInterleave(x,h::t) =
	(x::h::t)::(
		map((fn l => h::l), myInterleave(x,t)));

fun appendAll(nil) = nil
| appendAll(z::zs) = z @ (appendAll(zs));

fun permute(nil) = [[]]
| permute(h::t) = appendAll(
	map((fn l => myInterleave(h,l)), permute(t)));
	
fun permuteAll(L) = 
if L = [] then []
else permute(hd(L))::permuteAll(tl(L));


(*** Function to Calculate Subsets ***)
fun insert_all(E,L) =
	if L=[] then []
	else (E::hd(L)) :: insert_all(E,tl(L));


fun powerSet(L) =
	if L=[] then [[]]
	else powerSet(tl(L)) @
		insert_all(hd(L),powerSet(tl(L)));
		
fun powerSetAll(L) =
	if L = [] then []
	else powerSet(hd(L))::powerSetAll(tl(L));

(*** Length of a List ***)
fun length(L) = if (L=nil) then 0 
	else 1+length(tl (L));

(*** Minimum of a list ***)
fun minimum(L) = List.foldl Int.min (List.sub (L,0)) L;			


(*** Getting Subset of a list ***)
fun getSubset(L) = filter((fn x => length(x) = 2), L);

fun getSubsetAll(L) =
if L = [] then []
else getSubset(hd(L))::getSubsetAll(tl(L));

(*** Checks if X is a member of List L***)
fun member(X,L) =
if L=[] then false
else if X=hd(L) then true
else member(X,tl(L));

(*** Minus of two sets ***)
fun minus([],L2) = []
| minus(L1,[]) = L1
| minus(X::Xs,L2) =
if member(X,L2)
then minus(Xs,L2)
else X::minus(Xs,L2);

(*** Final Function to give list of violations ***)
fun finalViol(Perms,Pref) =
if Perms = [] then [length(concatCustom(Pref))]
else length(minus(concatCustom(Pref),hd(Perms)))::finalViol(tl(Perms), Pref);

fun violations(NumberOfPeople:int, NumberOfLocations:int, NumberOfPreferences:int, Preferences:(int * int * int) list) =
(***minimum(finalViol(getSubsetAll(powerSetAll(permute(numPeople(NumberOfLocations)))), getSubsetAll(powerSetAll(remNConcat(remAll(recurN(numPeople(NumberOfPeople), Preferences)))))));***)
print("violations("^ (Int.toString (minimum(finalViol(getSubsetAll(powerSetAll(permute(numPeople(NumberOfLocations)))), getSubsetAll(powerSetAll(remNConcat(remAll(recurN(numPeople(NumberOfPeople), Preferences)))))))))^ ")\n");
