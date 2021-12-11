equiv(sculpteur,and(personne,some(aCree,sculpture))). 
equiv(auteur,and(personne,some(aEcrit,livre))). 
equiv(editeur,and(personne,and(not(some(aEcrit,livre)),some(aEdite ,livre)))).
equiv(parent,and(personne,some(aEnfant,anything))).

cnamea(personne). 
cnamea(livre). 
cnamea(objet). 
cnamea(sculpture). 
cnamea(anything). 
cnamea(nothing).
cnamena(auteur). 
cnamena(editeur). 
cnamena(sculpteur). 
cnamena(parent).

iname(michelAnge). 
iname(david). 
iname(sonnets).
iname(vinci). 
iname(joconde).

rname(aCree). 
rname(aEcrit).
rname(aEdite). 
rname(aEnfant).

inst(michelAnge,personne).
inst(david,sculpture).
inst(sonnets,livre). 
inst(vinci,personne). 
inst(joconde,objet).

instR(michelAnge, david, aCree). 
instR(michelAnge, sonnets, aEcrit).
instR(vinci, joconde, aCree).



programme :-
premiere_etape(Tbox,Abi,Abr),
deuxieme_etape(Abi,Abe,Tbox),
troisieme_etape(Abe,Abr).



premiere_etape(Tbox,Abi,Abr) :-
creer_tbox(Tbox),
creer_abi(Abi),
creer_abr(Abr).

creer_tbox(Tbox) :- setof((C,D), equiv(C,D), Tbox).
creer_abi(Abi) :- setof((I,C), inst(I,C), Abi).
creer_abr(Abr) :- setof((I1,I2,R), instR(I1,I2,R), Abr).



deuxieme_etape(Abi,Abi1,Tbox) :-
  saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox) :-
  nl, write('Entrez le numero du type de proposition que vous demontrez :'),
  nl, write('1 Une instance donnee appartient a un concept donne.'),
  nl, write('2 Deux concepts n"ont pas d"elements en commun(ils ont une intersection vide).'),
  nl, read(R), suite(R,Abi,Abi1,Tbox).

suite(1,Abi,Abi1,Tbox) :-
  acquisition_prop_type1(Abi,Abi1,Tbox),!.
suite(2,Abi,Abi1,Tbox) :-
  acquisition_prop_type2(Abi,Abi1,Tbox),!.
suite(_,Abi,Abi1,Tbox) :-
  nl, write('Cette reponse est incorrecte.'),
  nl, saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

acquisition_prop_type1(Abi,Abi1,Tbox) :-
  nl, write('Entrez une instance :'),
  nl, read(I), testinstance(I),
  nl, write('Entrez un concept :'),
  nl, read(C), testconcept(C),
  remplace(C,CC),
  nnf(not(CC),NCC),
  Abi1 = [(I,NCC)|Abi].

acquisition_prop_type2(Abi,Abi1,Tbox) :-
  nl, write('Entrez un concept (1) :'),
  nl, read(C1), testconcept(C1),
  nl, write('Entrez un concept (2) :'),
  nl, read(C2), testconcept(C2),
  remplace(and(C1,C2),CC),
  nnf(CC,NCC),
  Abi1 = [(I,NCC)|Abi].




troisieme_etape(Abi,Abr) :-
tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),
resolution(Lie,Lpt,Li,Lu,Ls,Abr),
nl, write('Youpiiiiii, on a demontre la proposition initiale !!!').

compteur(1).

concat([],L1,L1).
concat([X|Y],L1,[X|L2]) :- concat(Y,L1,L2).

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


genere(Nom) :-
compteur(V), nombre(V,L1), 
concat([105,110,115,116],L1,L2),
V1 is V+1,
dynamic(compteur/1), 
retract(compteur(V)), 
dynamic(compteur/1),
assert(compteur(V1)),nl,nl,nl, 
name(Nom,L2).
nombre(0,[]). 
nombre(X,L1) :-
R is (X mod 10),
Q is ((X-R)//10),
chiffre_car(R,R1),
char_code(R1,R2), 
nombre(Q,L), 
concat(L,[R2],L1).
chiffre_car(0,'0'). 
chiffre_car(1,'1'). 
chiffre_car(2,'2'). 
chiffre_car(3,'3'). 
chiffre_car(4,'4'). 
chiffre_car(5,'5'). 
chiffre_car(6,'6'). 
chiffre_car(7,'7'). 
chiffre_car(8,'8'). 
chiffre_car(9,'9').

testconcept(C) :- cnamea(C), !.
testconcept(C) :- cnamena(C), !.
testconcept(not(C)) :- testconcept(C), !.
testconcept(or(C1,C2)) :- testconcept(C1), testconcept(C2), !.
testconcept(and(C1,C2)) :- testconcept(C1), testconcept(C2), !.
testconcept(some(R,C)) :- rname(R), testconcept(C), !.
testconcept(all(R,C)) :- rname(R), testconcept(C), !.


remplace(C,C) :- cnamea(C).
remplace(not(C),not(CC)) :- remplace(C,CC).
remplace(and(C1,C2),and(CC1,CC2)) :- remplace(C1,CC1), remplace(C2,CC2).
remplace(or(C1,C2),or(CC1,CC2)) :- remplace(C1,CC1), remplace(C2,CC2).
remplace(some(R,C),some(R,CC)) :- remplace(C,CC).
remplace(all(R,C),all(R,CC)) :- remplace(C,CC).
remplace(C,CC) :- equiv(C,D), remplace(D,CC).


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




tri_Abox([],[],[],[],[],[]).
tri_Abox([(I,some(R,C))|Abi],[(I,some(R,C))|Lie],Lpt,Li,Lu,Ls) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
tri_Abox([(I,all(R,C))|Abi],Lie,[(I,all(R,C))|Lpt],Li,Lu,Ls) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
tri_Abox([(I,and(C1,C2))|Abi],Lie,Lpt,[(I,and(C1,C2))|Li],Lu,Ls) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
tri_Abox([(I,or(C1,C2))|Abi],Lie,Lpt,Li,[(I,or(C1,C2))|Lu],Ls) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
tri_Abox([E|Abi],Lie,Lpt,Li,Lu,[E|Ls]) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).


checkclash([]).
checkclash([E|Abr]):-member(no(E),Abr).
checkclash([no(E)|Abr]):-member(E,Abr).


complete_some([(A,some(R,C))|Lie],Lpt,Li,Lu,Ls,Abr) :-
  genere(B),
  resolution(Lie,Lpt,Li,Lu,[(B,C)|Ls],[(A,B,R)|Abr]).
complete_some([],Lpt,Li,Lu,Ls,Abr):-transformation_and([],Lpt,Li,Lu,Ls,Abr).


transformation_and(Lie,Lpt,[(I,and(A,B))|Li],Lu,Ls,Abr):-resolution(Lie,Lpt,Li,Lu,[(A,I),(B,I)|Ls],Abr).
transformation_and(Lie,Lpt,[],Lu,Ls,Abr):-deduction_all(Lie,Lpt,[],Lu,Ls,Abr).


deduction_all(Lie,[],Li,Lu,Ls,Abr):-transformation_or(Lie,Lpt,Li,[],Ls,Abr).
deduction_all(Lie,[(I,all(R,C))|Lpt],Li,Lu,Ls,Abr):-
genere(B),
resolution(Lie,Lpt,Li,Lu,[(B,C)|Ls],[(I,B,R)|Abr]).



transformation_or(Lie,Lpt,Li,[ (I,or(C1,C2))|Lu],Ls,Abr):-resolution(Lie,Lpt,Li,Lu,[(B,I)|Ls],Abr),
resolution(Lie,Lpt,Li,Lu,[(A,I)|Ls],Abr).
transformation_or(_,_,_,[],_,_).


resolution(Lie,Lpt,Li,Lu,Ls,Abr):-checkclash(Ls),
complete_some(Lie,Lpt,Li,Lu,Ls,Abr).