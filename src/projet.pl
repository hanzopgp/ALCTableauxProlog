%% =================== Donnees du projet, Abox et Tbox =================== %%

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

%% =================== Partie principale =================== %%

%% Lance les 3 etapes du projet avec les bons arguments
programme :- 
  write("=========================================Premiere etape========================================="), nl,
  premiere_etape(Tbox,Abi,Abr),
  write("=========================================Deuxieme etape========================================="), nl,
  deuxieme_etape(Abi,Abe,Tbox),
  write("=========================================Troisieme etape========================================="), nl,
  troisieme_etape(Abe,Abr).

 %% La premiere etape consiste a creer la TBox, l'Abi et l'Abr
premiere_etape(Tbox,Abi,Abr) :-
  creer_tbox(Tbox),
  creer_abi(Abi),
  creer_abr(Abr).

%% La deuxieme etape est l'entree du programme, on choisit la
%% proposition a demontrer avant de lancer la resolution
deuxieme_etape(Abi,Abi1,Tbox) :-
  saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

%% La troisieme etape fait un tri dans la Abox et lance la resolution
troisieme_etape(Abi,Abr) :-
  tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),
  nl,write("Visualisation Abox :"),
  affiche_evolution_Abox(Ls,Lie,Lpt,Li,Lu,Abr,[],[],[],[],[],[]),
  resolution(Lie,Lpt,Li,Lu,Ls,Abr),
  nl, write('==> Resolution effectue <==').

%% =================== Fonctions utiles pour la premiere etape =================== %%

creer_tbox(Tbox) :- 
  setof((C,D), 
  equiv(C,D), Tbox).

creer_abi(Abi) :- 
  setof((I,C), 
  inst(I,C), Abi).

creer_abr(Abr) :- 
  setof((I1,I2,R), 
  instR(I1,I2,R), Abr).

%% =================== Fonctions utiles pour la deuxieme etape =================== %% 

%% Fonction faisant l'affichage demandant les entree et recuperant le type
%% de proposition a demontrer
saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox) :-
  nl, write('Entrez le numero du type de proposition que vous demontrez :'),
  nl, write('1. Une instance donnee appartient a un concept donne.'),
  nl, write('2. Deux concepts n\'ont pas d\'elements en commun (ils ont donc une intersection vide).'),
  nl, read(R), 
  suite(R,Abi,Abi1,Tbox).

%% Fonction permettant de recuperer la proposition suivant le type de
%% proposition choisit et de checker si l'utilisateur a bien entree
%% 1 ou 2
suite(1,Abi,Abi1,Tbox) :-
  acquisition_prop_type1(Abi,Abi1,Tbox),!.
suite(2,Abi,Abi1,Tbox) :-
  acquisition_prop_type2(Abi,Abi1,Tbox),!.
suite(_,Abi,Abi1,Tbox) :-
  nl, write('Reponse incorrecte.'),
  nl, saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

%% Fonction permettant de tester si l'entree utilisateur pour les
%% proposition de type 1 est correct. Si l'entree est correct alors
%% on prend la negation et on la met sous forme normale negative
acquisition_prop_type1(Abi,Abi1,Tbox) :-
  nl, write('Entrer l\'instance :'),
  nl, read(I), testinstance(I),
  nl, write('Entrer le concept :'),
  nl, read(C), nl, testconcept(C),
  remplace(C,CC),
  nnf(not(CC),NCC),
  Abi1 = [(I,NCC)|Abi].

%% Fonction permettant de tester si les entrees de l'utilisateur pour 
%% les propositions de type 2 sont correct. Si les deux entrees sont 
%% correct alors on met l'entree sous forme normale negative 
acquisition_prop_type2(Abi,Abi1,Tbox) :-
  nl, write('Entrer le premier concept :'),
  nl, read(C1), 
  testconcept(C1),
  nl, write('Entrer le deuxieme concept :'),
  nl, read(C2), 
  testconcept(C2),
  remplace(and(C1,C2),CC),
  nnf(CC,NCC),
  Abi1 = [(I,NCC)|Abi].

%% =================== Fonctions utiles pour la troisieme etape =================== %% 

%% Initialisation du compteur permettant de nommer nos nouvelles variables
compteur(1).

%% La fonction genere permet de generer de nouvelles variables lors de l'utilisation
%% de la regle il existe
genere(Nom) :-
  compteur(V), 
  nombre(V,L1), 
  concat([105,110,115,116],L1,L2),
  V1 is V+1,
  dynamic(compteur/1), 
  retract(compteur(V)), 
  dynamic(compteur/1),
  assert(compteur(V1)),
  nl,nl,nl, 
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

%% La fonction autoref permet de checker si il y a une reference circulaire dans
%% notre Abox/Tbox. Cela pourrait faire tourner notre programme en boucle il est
%% donc important de verifier cela
autoref(C,C) :- 
  cnamea(C).
autoref(C,C) :- 
  cnamena(C).
autoref(C,and(D,_)) :- 
  cnamea(C), 
  autoref(C,D).
autoref(C,and(D,_)) :- 
  cnamena(C), 
  autoref(C,D).
autoref(C,and(_,D)) :- 
  cnamea(C), 
  autoref(C,D).
autoref(C,and(_,D)) :- 
  cnamena(C), 
  autoref(C,D).
autoref(C,or(D,_)) :- 
  cnamea(C), 
  autoref(C,D).
autoref(C,or(D,_)) :- 
  cnamena(C), 
  autoref(C,D).
autoref(C,some(_,D)) :- 
  cnamena(C), 
  autoref(C,D).
autoref(C,all(_,D)) :- 
  cnamea(C), 
  autoref(C,D).
autoref(C,all(_,D)) :- 
  cnamena(C), 
  autoref(C,D).
autoref(C,D) :- 
  cnamea(C), 
  equiv(D,E), 
  autoref(C,E).
autoref(C,D) :- 
  cnamena(C), 
  equiv(D,E), 
  autoref(C,E).
autoref(C,or(_,D)) :- 
  cnamea(C), 
  autoref(C,D).
autoref(C,or(_,D)) :- 
  cnamena(C), 
  autoref(C,D).
autoref(C,not(D)) :- 
  cnamea(C), 
  autoref(C,D).
autoref(C,not(D)) :- 
  cnamena(C), 
  autoref(C,D).
autoref(C,some(_,D)) :- 
  cnamea(C), 
  autoref(C,D).

%% Cette fonction permet de tester si les variables utilisees dans nos regles
%% sont bien a leur place. Par exemple il ne peut y avoir que deux concepts
%% autour d'un and ou d'un or
testconcept(C) :- 
  cnamea(C), !.
testconcept(C) :- 
  cnamena(C), !.
testconcept(not(C)) :- 
  testconcept(C), !.
testconcept(or(C1,C2)) :- 
  testconcept(C1), 
  testconcept(C2), !.
testconcept(and(C1,C2)) :- 
  testconcept(C1), 
  testconcept(C2), !.
testconcept(some(R,C)) :- 
  rname(R), 
  testconcept(C), !.
testconcept(all(R,C)) :- 
  rname(R), 
  testconcept(C), !.

%% Cette fonction remplace les entree par leurs definitions
remplace(C,C) :- 
  cnamea(C).
remplace(not(C),not(CC)) :- 
  remplace(C,CC).
remplace(and(C1,C2),and(CC1,CC2)) :- 
  remplace(C1,CC1), 
  remplace(C2,CC2).
remplace(or(C1,C2),or(CC1,CC2)) :- 
  remplace(C1,CC1), 
  remplace(C2,CC2).
remplace(some(R,C),some(R,CC)) :- 
  remplace(C,CC).
remplace(all(R,C),all(R,CC)) :- 
  remplace(C,CC).
remplace(C,CC) :- equiv(C,D), 
  remplace(D,CC).

%% Cette fonction permet de concatener deux listes
concat([],L1,L1).
concat([X|Y],L1,[X|L2]) :- 
  concat(Y,L1,L2).

%% Cette fonction test si l'entree est bien une instance
testinstance(I) :- 
  iname(I), !.

%% Cette fonction met l'entree sous forme normale negative
nnf(not(and(C1,C2)),or(NC1,NC2)):- 
  nnf(not(C1),NC1), 
  nnf(not(C2),NC2),!.
nnf(not(or(C1,C2)),and(NC1,NC2)):- 
  nnf(not(C1),NC1), 
  nnf(not(C2),NC2),!.
nnf(not(all(R,C)),some(R,NC)) :- 
  nnf(not(C),NC),!.
nnf(not(some(R,C)),all(R,NC)):- 
  nnf(not(C),NC),!.
nnf(not(not(X)),X):-!.
nnf(not(X),not(X)):-!.
nnf(and(C1,C2),and(NC1,NC2)):- 
  nnf(C1,NC1),
  nnf(C2,NC2),!.
nnf(or(C1,C2),or(NC1,NC2)):- 
  nnf(C1,NC1), 
  nnf(C2,NC2),!.
nnf(some(R,C),some(R,NC)):- 
  nnf(C,NC),!. 
nnf(all(R,C),all(R,NC)) :- 
  nnf(C,NC),!.
nnf(X,X).

%% Cette fonction trie la Abox avant la resolution
tri_Abox([],[],[],[],[],[]).
tri_Abox([(I,some(R,C))|Abi],[(I,some(R,C))|Lie],Lpt,Li,Lu,Ls) :- 
  tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
tri_Abox([(I,all(R,C))|Abi],Lie,[(I,all(R,C))|Lpt],Li,Lu,Ls) :- 
  tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
tri_Abox([(I,and(C1,C2))|Abi],Lie,Lpt,[(I,and(C1,C2))|Li],Lu,Ls) :- 
  tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
tri_Abox([(I,or(C1,C2))|Abi],Lie,Lpt,Li,[(I,or(C1,C2))|Lu],Ls) :- 
  tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
tri_Abox([E|Abi],Lie,Lpt,Li,Lu,[E|Ls]) :- 
  tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).

%% Cette fonction est la boucle principale de la resolution.
%% A chaque appel, elle check premierement si il y a un clash.
%% Elle check ensuite si on peut appliquer la regle il existe
resolution(Lie,Lpt,Li,Lu,Ls,Abr):-
  checkclash(Ls).
resolution(Lie,Lpt,Li,Lu,Ls,Abr):-
  complete_some(Lie,Lpt,Li,Lu,Ls,Abr).

%% Cette fonction permet de check si il y a un clash dans notre
%% liste Ls
checkclash([(I,C)|Ls]) :-
  nnf(not(C),NC),
  member((I,NC),Ls),
  write("!!!!!!!! Clash sur "),
  write(I), write(" !!!!!!!!"), nl.
checkclash([_|Ls]) :- 
  checkclash(Ls).


%% Cette fonction est appele a chaque modification de liste pendant
%% la resolution
evolue((I,and(A,B)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, [(I,and(A,B))|Li], Lu, Ls).
evolue((I,all(R,C)), Lie, Lpt, Li, Lu, Ls, Lie, [(I,all(R,C))|Lpt], Li, Lu, Ls).
evolue((I,some(R,C)), Lie, Lpt, Li, Lu, Ls, [(I,some(R,C))|Lie], Lpt, Li, Lu, Ls).
evolue((I,or(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, [(I,or(C1,C2))|Lu], Ls).
evolue(Elem, Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, [(Elem)|Ls]).

%% =================== Predicats des differentes regles de resolution =================== %% 

%% Regle il existe
complete_some([],Lpt,Li,Lu,Ls,Abr):-
  transformation_and([],Lpt,Li,Lu,Ls,Abr).
complete_some([(A,some(R,C))|Lie],Lpt,Li,Lu,Ls,Abr) :-
  genere(B),
  Abr1 = Abr,
  evolue((B,C),Lie,Lpt,Li,Lu,Ls,Lie1,Lpt1,Li1,Lu1,Ls1),
  Abr2 = [(A,B,R)|Abr],
  write("=========================================Application du some========================================="), nl,
  affiche_evolution_Abox(Ls,Lie,Lpt,Li,Lu,Abr1,Ls1,Lie1,Lpt1,Li1,Lu1,Abr2),
  resolution(Lie1,Lpt1,Li1,Lu1,Ls1,[(A,B,R)|Abr]),
  write("=========================================Fin du some========================================="), nl.

%% Regle et
transformation_and(Lie,Lpt,[],Lu,Ls,Abr):-
  deduction_all(Lie,Lpt,[],Lu,Ls,Abr).
transformation_and(Lie,Lpt,[(I,and(A,B))|Li],Lu,Ls,Abr):-
  evolue((I,A),Lie,Lpt,Li,Lu,Ls,Lie1,Lpt1,Li1,Lu1,Ls1),
  evolue((I,B),Lie1,Lpt1,Li1,Lu1,Ls1,Lie2,Lpt2,Li2,Lu2,Ls2),
  write("=========================================Application du and========================================="), nl,
  affiche_evolution_Abox(Ls,Lie,Lpt,Li,Lu,[],Ls2,Lie2,Lpt2,Li2,Lu2,[]),
  resolution(Lie2,Lpt2,Li2,Lu2,Ls2,Abr),
  write("=========================================Fin du and========================================="), nl.


%% Regle quelquesoit
deduction_all(Lie,[],Li,Lu,Ls,Abr):-
  transformation_or(Lie,[],Li,Lu,Ls,Abr).
deduction_all(Lie,[(I,all(R,C))|Lpt],Li,Lu,Ls,Abr):-
  member((I,B,R),Abr),
  evolue((B,C),Lie,Lpt,Li,Lu,Ls,Lie1,Lpt1,Li1,Lu1,Ls1),
  write("=========================================Application du all========================================="), nl,
  affiche_evolution_Abox(Ls,Lie,Lpt,Li,Lu,[],Ls1,Lie1,Lpt1,Li1,Lu1,[]),
  resolution(Lie1,Lpt1,Li1,Lu1,Ls1,Abr),
  write("=========================================Fin du all========================================="), nl.


%% Regle ou
transformation_or(Lie,Lpt,Li,[(I,or(C,D))|Lu],Ls,Abr):- 
  evolue((I,C),Lie,Lpt,Li,Lu,Ls,Lie1,Lpt1,Li1,Lu1,Ls1),
  write("=========================================Application du or========================================="), nl,
  write("-----------------------------------------Noeud 1 du or-----------------------------------------"), nl, nl,
  affiche_evolution_Abox(Ls,Lie,Lpt,Li,Lu,[],Ls1,Lie1,Lpt1,Li1,Lu1,[]),
  evolue((I,D),Lie,Lpt,Li,Lu,Ls,Lie2,Lpt2,Li2,Lu2,Ls2),
  resolution(Lie1,Lpt1,Li1,Lu1,Ls1,Abr),
  write("-----------------------------------------Noeud 2 du or-----------------------------------------"), nl, nl,
  affiche_evolution_Abox(Ls1,Lie1,Lpt1,Li1,Lu1,[],Ls2,Lie2,Lpt2,Li2,Lu2,[]),
  print_diff(Ls2,Ls1),
  resolution(Lie2,Lpt2,Li2,Lu2,Ls2,Abr),
  write("=========================================Fin du or========================================="), nl.

%% =================== Partie affichage =================== %%

%% Affiche l'evolution des qu'il y a eu une modification dans une des listes
affiche_evolution_Abox(Ls1,Lie1,Lpt1,Li1,Lu1,Abr1,Ls2,Lie2,Lpt2,Li2,Lu2,Abr2):-
  nl,write(" --> Ls :"),
  print_diff(Ls2,Ls1),
  nl,write(" --> Lie :"),
  print_diff(Lie2,Lie1),
  nl,write(" --> Lpt :"),
  print_diff(Lpt2,Lpt1),
  nl,write(" --> Li :"),
  print_diff(Li2,Li1),
  nl,write(" --> Lu :"),
  print_diff(Lu2,Lu1),
  nl,write(" --> Abr :"),
  print_diff(Abr2,Abr1),
  nl,nl,nl.

%% Trouve une difference dans les listes et appelle la traduction prefixe - infixe
print_diff([],[]).
print_diff([],L2):-
  trad_infix(L2).
print_diff(L1,L2):-
  diff_list(L1,L2,R),
  trad_infix(R).

%% Retourne les elements differents entre deux listes
diff_list([],[],[]).
diff_list(L1,L2,R) :- 
  findall(E,(member(E,L1),not(member(E,L2))),R).

%% Affiche la traduction infixe d'une entree prefixe
trad_infix([]).
trad_infix([(I,not(A))|LS]):-
  write(I), 
  write(":¬("),
  trad_infix(A),
  write(")").
trad_infix(not(A)):-
  write("¬("),
  trad_infix(A),
  write(")").
trad_infix([(I,and(A,B))|LS]):-
  write(I), 
  write(":("),
  trad_infix(A),
  write(" ⊓ "),
  trad_infix(B),
  write(")").
trad_infix(and(A,B)):-
  write("("),
  trad_infix(A),
  write(" ⊓ "),
  trad_infix(B),
  write(")").
trad_infix([(I,all(R,C))|LS]):-
  write(I), 
  write(":∀("),
  trad_infix(R),
  write("."),
  trad_infix(C),
  write(")").
trad_infix(all(R,C)):-
  write("∀("),
  trad_infix(R),
  write("."),
  trad_infix(C),
  write(")").
trad_infix([(I,some(R,C))|LS]):-
  write(I), 
  write(":∃("),
  trad_infix(R),
  write("."),
  trad_infix(C),
  write(")").
trad_infix(some(R,C)):-
  write("∃("),
  trad_infix(R),
  write("."),
  trad_infix(C),
  write(")").
trad_infix([(I,or(A,B))|LS]):-
  write(I), 
  write(":("),
  trad_infix(A),
  write(" ⊔ "),
  trad_infix((B)),
  write(")").
trad_infix(or(A,B)):-
  write("("),
  trad_infix(A),
  write(" ⊔ "),
  trad_infix((B)),
  write(")").
trad_infix((I)):-
  write(I).