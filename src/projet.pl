% Tbox
[
(sculpteur,and(personne,some(aCree,sculpture))),
(auteur,and(personne,some(aEcrit,livre))),
(editeur,and(personne,and(not(some(aEcrit,livre)),some(aEdite,livre)))),
(parent,and(personne,some(aEnfant,anything)))
].

% Abox
[(michelAnge,personne), (david,sculpture), (sonnets,livre), (vinci,personne), (joconde,objet)].
[(michelAnge,david,aCree), (michelAnge,sonnet,aEcrit), (vinci,joconde,aCree)].

% forme normale negative 
nnf(not(and(C1,C2)),or(NC1,NC2)):- nnf(not(C1),NC1), nnf(not(C2),NC2),!. nnf(not(or(C1,C2)),and(NC1,NC2)):- nnf(not(C1),NC1), nnf(not(C2),NC2),!.
nnf(not(all(R,C)),some(R,NC)) :- nnf(not(C),NC),!. nnf(not(some(R,C)),all(R,NC)):- nnf(not(C),NC),!. nnf(not(not(X)),X):-!.
nnf(not(X),not(X)):-!.
nnf(and(C1,C2),and(NC1,NC2)):- nnf(C1,NC1),nnf(C2,NC2),!. nnf(or(C1,C2),or(NC1,NC2)):- nnf(C1,NC1), nnf(C2,NC2),!. nnf(some(R,C),some(R,NC)):- nnf(C,NC),!. nnf(all(R,C),all(R,NC)) :- nnf(C,NC),!.
nnf(X,X).

% autoref 
% autoref(C,D):-
% autoref(C,or(C1,C2)):-autoref(C,C1),autoref(C,C2).
% autoref(C,and(C1,C2)) :- autoref(C,C1).
% auroref(C,and(C1,C2)) :- autoref(C,C2).
% autoref(C,C).

% Partie II
% programme :-
%     premiere_etape(Tbox,Abi,Abr).
%     deuxieme_etape(Abi,Abi1,Tbox).
%     troisieme_etape(Abi1,Abr).
 
% partie 1
% premiere_etape(Tbox,Abi,Abr):-
 
% partie 2
% deuxieme_etape(Abi,Abi1,Tbox) :- saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).
% saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox) :-
% nl,write('Entrez le numero du type de proposition que vous voulez demontrer :'),nl,
% write('1 Une instance donnee appartient a un concept donne.'),nl, write('2 Deux concepts n"ont pas d"elements en commun(ils ont une intersection vide).’),nl, read(R), suite(R,Abi,Abi1,Tbox).
% suite(1,Abi,Abi1,Tbox) :- acquisition_prop_type1(Abi,Abi1,Tbox),!.
% suite(2,Abi,Abi1,Tbox) :- acquisition_prop_type2(Abi,Abi1,Tbox),!.
% suite(R,Abi,Abi1,Tbox) :-
% nl,write('Cette reponse est incorrecte.'),nl, saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).
