autoref(C,C) :- cnamea(C).
autoref(C,C) :- cnamena(C).
autoref(C,and(D,_)) :- cnamea(C), autoref(C,D).
autoref(C,and(D,_)) :- cnamena(C), autoref(C,D).
autoref(C,and(_,D)) :- cnamea(C), autoref(C,D).
autoref(C,and(_,D)) :- cnamena(C), autoref(C,D).
autoref(C,or(D,_)) :- cnamea(C), autoref(C,D).
autoref(C,or(D,_)) :- cnamena(C), autoref(C,D).
autoref(C,some(_,D)) :- cnamena(C), autoref(C,D).
autoref(C,all(_,D)) :- cnamea(C), autoref(C,D).
autoref(C,all(_,D)) :- cnamena(C), autoref(C,D).
autoref(C,D) :- cnamea(C), equiv(D,E), autoref(C,E).
autoref(C,D) :- cnamena(C), equiv(D,E), autoref(C,E).
autoref(C,or(_,D)) :- cnamea(C), autoref(C,D).
autoref(C,or(_,D)) :- cnamena(C), autoref(C,D).
autoref(C,not(D)) :- cnamea(C), autoref(C,D).
autoref(C,not(D)) :- cnamena(C), autoref(C,D).
autoref(C,some(_,D)) :- cnamea(C), autoref(C,D).

testconcept(C) :- cnamea(C), !.
testconcept(C) :- cnamena(C), !.
testconcept(not(C)) :- testconcept(C), !.
testconcept(or(C1,C2)) :- testconcept(C1), testconcept(C2), !.
testconcept(and(C1,C2)) :- testconcept(C1), testconcept(C2), !.
testconcept(some(R,C)) :- rname(R), testconcept(C), !.
testconcept(all(R,C)) :- rname(R), testconcept(C), !.


remplace(C,C) :- cnamea(C).
remplace(not(C),not(CC)) :- remplace(C,CC).
remplace(and(C1,C2),and(CC1,CC2)) :- remplace(C1,CC1),remplace(C2,CC2).
remplace(or(C1,C2),or(CC1,CC2)) :- remplace(C1,CC1),remplace(C2,CC2) .
remplace(some(R,C),some(R,CC)) :- remplace(C,CC).
remplace(all(R,C),all(R,CC)) :- remplace(C,CC).
remplace(C,CC) :- remplace(D,CC),equiv(C,D).

testinstance(I) :- iname(I), !.


nnf(not(and(C1,C2)),or(NC1,NC2)):- nnf(not(C1),NC1), nnf(not(C2),NC2),!.
nnf(not(or(C1,C2)),and(NC1,NC2)):- nnf(not(C1),NC1), nnf(not(C2),NC2),!.
nnf(not(all(R,C)),some(R,NC)) :- nnf(not(C),NC),!.
nnf(not(some(R,C)),all(R,NC)):- nnf(not(C),NC),!.
nnf(not(not(X)),X):-!.
nnf(not(X),not(X)):-!.
nnf(and(C1,C2),and(NC1,NC2)):- nnf(C1,NC1),nnf(C2,NC2),!.
nnf(or(C1,C2),or(NC1,NC2)):- nnf(C1,NC1), nnf(C2,NC2),!.
nnf(some(R,C),some(R,NC)):- nnf(C,NC),!. 
nnf(all(R,C),all(R,NC)) :- nnf(C,NC),!.
nnf(X,X).

#Partie 3

tri_Abox([],_,_,_,_,_):.
tri_Abox([(I,some(R,C))|Abi],Lie,Lpt,Li,Lu,Ls):-tri_Abox(Abi,[(I,some(R,C))|Lie],Lpt,Li,Lu,Ls).
tri_Abox([(I,all(R,C))|Abi],Lie,Lpt,Li,Lu,Ls):-tri_Abox(Abi,Lie,[(I,all(R,C))|Lpt],Li,Lu,Ls).
tri_Abox([(I,and(C1,C2))|Abi],Lie,Lpt,Li,Lu,Ls):-tri_Abox(Abi,Lie,Lpt,[(I,and(C1,C2))|Li],Lu,Ls).
tri_Abox([(I,or(C1,C2))|Abi],Lie,Lpt,Li,Lu,Ls):-tri_Abox(Abi,Lie,Lpt,Li,[(I,or(C1,C2))|Lu],Ls).
tri_Abox([(Element)|Abi],Lie,Lpt,Li,Lu,Ls):-tri_Abox(Abi,Lie,Lpt,Li,Lu,[(Element)|Ls]).

#enleve(X,[X|L],L) :-!.
#enleve(X,[Y|L],[Y|L2]) :- enleve(X,L,L2).

#tri_Abox([],_,_,_,_,_):.
#tri_Abox([(I,some(R,C))|Abi],Lie,Lpt,Li,Lu,Ls):-tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),enleve((I,some(R,C),[(I,some(R,C))|Abi],Lie).
#tri_Abox([(I,all(R,C))|Abi],Lie,Lpt,Li,Lu,Ls):-tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),enleve(I,all(R,C),[(I,all(R,C))|Lpt],Lpt).
#tri_Abox([(I,and(C1,C2))|Abi],Lie,Lpt,Li,Lu,Ls):-tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),enleve(I,and(C1,C2),[(I,and(C1,C2))|Li],Lie).
#tri_Abox([(I,or(C1,C2))|Abi],Lie,Lpt,Li,Lu,Ls):-tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),enleve((I,or(C1,C2),[(I,or(C1,C2))|Lu],Lu).
#tri_Abox([(Element)|Abi],Lie,Lpt,Li,Lu,Ls):-tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),enleve((Element,[(Element)|Ls],Ls).


complete_some([],_,_,_,_,_):.#liste Lie vide
complete_some([(I,and(C1,C2))|Lie],Lpt,Li,Lu,Ls,Abr):- #cas vrai
complete_some([(Elem)|Lie],Lpt,Li,Lu,Ls,Abr):-complete_some(Lie,Lpt,Li,Lu,Ls,Abr).# cas faux 


transformation_and(_,_,[],_,_,_):.# condition d'arrêt
transformation_and(Lie,Lpt,[(I,and(C1,C2))|Li],Lu,Ls,Abr):# cas vrai
transformation_and(Lie,Lpt,[(Elem)|Li],Lu,Ls,Abr):-transformation_and(Lie,Lpt,Li,Lu,Ls,Abr).#cas faux 

deduction_all(_,[],_,_,_,_):.#terminaison
deduction_all(Lie,[(I,all(R,C))|Lpt],Li,Lu,Ls,Abr)
deduction_all(Lie,[(Elem)|Lpt],Li,Lu,Ls,Abr):-deduction_all(Lie,Lpt,Li,Lu,Ls,Abr).#cas faux


transformation_or(_,_,_,[],_,_):.#terminaison
transformation_or(Lie,Lpt,Li,[ (I,or(C1,C2))|Lu],Ls,Abr)
transformation_or(Lie,Lpt,Li,[(Elem)|Lu],Ls,Abr):-transformation_or(Lie,Lpt,Li,Lu,Ls,Abr)#cas faux

