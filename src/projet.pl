  #- [donnéeprojet ],[ bibliotheque].

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
suite(R,Abi,Abi1,Tbox) :-
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
nl, write('on est rentre a la maison'),
tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),
resolution(Lie,Lpt,Li,Lu,Ls,Abr),
nl, write('Youpiiiiii, on a demontre la proposition initiale !!!').





#la liste Lie des assertions du type (I,some(R,C))
#- la liste Lpt des assertions du type (I,all(R,C))
##- la liste Li des assertions du type (I,and(C1,C2))
#- la liste Lu des assertions du type (I,or(C1,C2))