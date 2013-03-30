list(nil).
list(cons(A,As)) :- list(As).

append(nil,Bs,Bs) :- list(Bs).
append(cons(A,As),Bs,cons(A,Cs)) :- append(As,Bs,Cs).
