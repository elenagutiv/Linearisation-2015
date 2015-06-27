:-module(ls, [select_list/3]).

select_list([L|Ls],Ls2,Ls3):-
	select(L,Ls2,Rs),
	select_list(Ls,Rs,Ls3).
select_list([],Ls,Ls).