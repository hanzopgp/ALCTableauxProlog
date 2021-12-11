#enleve(X,[X|L],L) :-!.
#enleve(X,[Y|L],[Y|L2]) :- enleve(X,L,L2).

#tri_Abox([],_,_,_,_,_):.
#tri_Abox([(I,some(R,C))|Abi],Lie,Lpt,Li,Lu,Ls):-tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),enleve((I,some(R,C),[(I,some(R,C))|Abi],Lie).
#tri_Abox([(I,all(R,C))|Abi],Lie,Lpt,Li,Lu,Ls):-tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),enleve(I,all(R,C),[(I,all(R,C))|Lpt],Lpt).
#tri_Abox([(I,and(C1,C2))|Abi],Lie,Lpt,Li,Lu,Ls):-tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),enleve(I,and(C1,C2),[(I,and(C1,C2))|Li],Lie).
#tri_Abox([(I,or(C1,C2))|Abi],Lie,Lpt,Li,Lu,Ls):-tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),enleve((I,or(C1,C2),[(I,or(C1,C2))|Lu],Lu).
#tri_Abox([(Element)|Abi],Lie,Lpt,Li,Lu,Ls):-tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),enleve((Element,[(Element)|Ls],Ls).


#complete_some([],_,_,_,_,_):.#liste Lie vide
#complete_some([(I,and(C1,C2))|Lie],Lpt,Li,Lu,Ls,Abr):- #cas vrai
#complete_some([(Elem)|Lie],Lpt,Li,Lu,Ls,Abr):-complete_some(Lie,Lpt,Li,Lu,Ls,Abr).# cas faux 


#transformation_and(_,_,[],_,_,_):.# condition d'arrêt
#transformation_and(Lie,Lpt,[(I,and(C1,C2))|Li],Lu,Ls,Abr):# cas vrai
#transformation_and(Lie,Lpt,[(Elem)|Li],Lu,Ls,Abr):-transformation_and(Lie,Lpt,Li,Lu,Ls,Abr).#cas faux 

#deduction_all(_,[],_,_,_,_):.#terminaison
#deduction_all(Lie,[(I,all(R,C))|Lpt],Li,Lu,Ls,Abr)
#deduction_all(Lie,[(Elem)|Lpt],Li,Lu,Ls,Abr):-deduction_all(Lie,Lpt,Li,Lu,Ls,Abr).#cas faux


#transformation_or(_,_,_,[],_,_):.#terminaison
#transformation_or(Lie,Lpt,Li,[ (I,or(C1,C2))|Lu],Ls,Abr)
#transformation_or(Lie,Lpt,Li,[(Elem)|Lu],Ls,Abr):-transformation_or(Lie,Lpt,Li,Lu,Ls,Abr)#cas faux


genere(Nom) :-
nombre(0,[]). nombre(X,L1) :-
compteur(V),nombre(V,L1), concat([105,110,115,116],L1,L2), V1 is V+1,
dynamic(compteur/1), retract(compteur(V)), dynamic(compteur/1), assert(compteur(V1)),nl,nl,nl, name(Nom,L2).
R is (X mod 10),
Q is ((X-R)//10), chiffre_car(R,R1), char_code(R1,R2), nombre(Q,L), concat(L,[R2],L1).
chiffre_car(0,'0'). chiffre_car(1,'1'). chiffre_car(2,'2'). chiffre_car(3,'3'). chiffre_car(4,'4'). chiffre_car(5,'5'). chiffre_car(6,'6'). chiffre_car(7,'7'). chiffre_car(8,'8'). chiffre_car(9,’9’).

complete_some([(I,some(R,C))|Lie],Lpt,Li,Lu,Ls,Abr):genere(nom),concat([R(I,nom),C(I,nom)],Abr,Abr1), resolution(Lie,Lpt,Li,Lu,Ls,[Abr1]). 
complete_some([],Lpt,Li,Lu,Ls,Abr):-transformation_and([],Lpt,Li,Lu,Ls,Abr).

transformation_and(Lie,Lpt,[(I,and(C1,C2))|Li],Lu,Ls,Abr):concat([C1(I),C2(I)],Abr,Abr1),resolution(Lie,Lpt,Li,Lu,Ls,Abr1). 
transformation_and(Lie,Lpt,[],Lu,Ls,Abr):-deduction_all(Lie,Lpt,[],Lu,Ls,Abr).

deduction_all(Lie,[],Li,Lu,Ls,Abr):-transformation_or(Lie,Lpt,Li,[],Ls,Abr)
deduction_all(Lie,[(I1,all(R,C))|Lpt],Li,Lu,Ls,Abr):-member(([R(I1,I2)],Ls),concat(C(I2),Abr,Abr1),(resolution(Lie,Lpt,Li,Lu,Ls,[Abr1]).

transformation_or(_,_,_,[],_,_):
transformation_or(Lie,Lpt,Li,[ (I,or(C1,C2))|Lu],Ls,Abr):-concat([C1(I)],Abr,Abr1),resolution(Lie,Lpt,Li,Lu,Ls,Abr1),concat([C2(I)],Abr,Abr2),resolution(Lie,Lpt,Li,Lu,Ls,Abr2)



resolution(Lie,Lpt,Li,Lu,Ls,Abr):-checkclash([Abr]),
complete_some(Lie,Lpt,Li,Lu,Ls,Abr).

evolue(A, Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1):