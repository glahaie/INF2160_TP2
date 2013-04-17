/**********************************************************************
* TP2
* Scenario de tests
***********************************************************************/

:- ensure_loaded('Agencement.pl').

/*--------------------------------------------------------------
* Chargement des modules nécessaires au scénario de tests
*-------------------------------------------------------------*/

composant(meuble ,b1,(1.0,1.0,1.0),position(0.0, 0.0, 0.0),coin).
composant(electro,b2,(1.0,1.0,1.0),position(1.0, 0.0, 0.0),laveVaisselle).
composant(meuble,b3,(1.0,1.0,1.0),position(0.0, 1.0, 0.0),bas).
composant(meuble,b4,(1.0,1.0,1.0),position(2.0, 0.0, 0.0),bas).
composant(meuble,h1,(1.0,1.0,0.5),position(2.0, 0.0, 2.0),haut).
composant(meuble,h2,(1.0,1.0,0.5),position(3.0, 0.0, 2.0),haut).
composant(meuble,b5,(1.0,1.0,1.0),position(4.0, 0.0, 0.0),haut).
composant(meuble,b6,(1.0,1.0,1.0),position(5.0, 0.0, 0.0),haut).

bloc(0,[axCmp(y,b1),axCmp(y,b3)]).
bloc(1,[axCmp(x,b1),axCmp(x,b2),axCmp(x,b4)]).
bloc(2,[axCmp(x,b5),axCmp(x,b6)]).
bloc(3,[axCmp(x,h1),axCmp(x,h2)]).
bloc(4,[axCmp(x,b1),axCmp(x,b4),axCmp(y,b2)]).
bloc(5,[axCmp(x,b1),axCmp(x,b2)]).
bloc(6,[axCmp(y,b1)]).
bloc(7,[axCmp(x,b1),axCmp(x,b2),axCmp(y,b3),axCmp(x,b4)]).

blocs([1,2,3]).

agencement(ag,[b1,b2,b3,b4,b5,b6,h1,h2]).
agencement(vide,[]).
agencement(un,[h1]).
agencement(deux,[b1,b3]).

% ----------------------------------------------------------------
% Pr�dicats de test
% Version SICStus Prolog
% ----------------------------------------------------------------

testXX(M,P,1) :- nl, write(M), write(': '),
                 on_exception(Erreur,P,(write(Erreur),nl,fail)),
                 !, write('OK'), nl.
testXX(_,_,0) :- write('ERREUR'), nl.

testTP(Num) :- trXX(Num,_).

memesListes1([X|Xs],Ys) :-
	member(X,Ys),
	memesListes1(Xs,Ys).
memesListes1([],_).

memesListes(Xs,Ys) :-
	memesListes1(Xs,Ys),
	memesListes1(Ys,Xs).

% ----------------------------------------------------------------
% Description des tests
% ----------------------------------------------------------------

trXX(1,N) :- testXX('#1. lesIds: bloc vide',	 lesIds([],[]),N).
trXX(2,N) :- testXX('#2. lesIds: liste d''un bloc avec un composant',
	 (bloc(6,BlocUn),lesIds([BlocUn],[[b1]])),N).
trXX(3,N) :-  testXX('#3. lesIds: liste d''un bloc avec plusieurs composants',
	 (bloc(0,Bloc),lesIds([Bloc],[[b1,b3]])),N).
trXX(4,N) :- testXX('#4. lesIds: liste de plusieurs blocs',
	 (bloc(1,Bloc1),bloc(2,Bloc2),lesIds([Bloc1,Bloc2],[[b1,b2,b4],[b5,b6]])),N).
trXX(5,N) :- testXX('#5. lesComposantsDuType : agencement vide, rien',
         lesComposantsDuType(vide,four,[]),N).
trXX(6,N) :- testXX('#6. lesComposantsDuType : agencement un composant, rien',
         lesComposantsDuType(un,four,[]),N).
trXX(7,N) :- testXX('#7. lesComposantsDuType : agencement un composant, un composant',
         lesComposantsDuType(un,haut,[h1]),N).
trXX(8,N) :- testXX('#8. lesComposantsDuType : agencement 2 composants, un composant',
         lesComposantsDuType(deux,coin,[b1]),N).
trXX(9,N) :- testXX('#9. lesComposantsDuType : agencement plusieurs composants, un composant',
         lesComposantsDuType(ag,coin,[b1]),N).
trXX(10,N) :- testXX('#10. lesComposantsDuType : agencement plusieurs composants, plusieurs composants',
         (lesComposantsDuType(ag,bas,Bs),memesListes(Bs,[b3,b4])),N).
trXX(11,N) :- testXX('#11. estStrictementInclus : bloc vide',
         (bloc(0,B0),estStrictementInclus([],B0)),N).
trXX(12,N) :- testXX('#12.estStrictementInclus : bloc 6 avec 1',
         (bloc(6,B6),bloc(5,B5),estStrictementInclus(B6,B5)),N).
trXX(13,N) :- testXX('#13. estStrictementInclus : bloc1 avec 4, blocs egaux',
         (bloc(1,B1),bloc(4,B4),\+ estStrictementInclus(B1,B4)),N).
trXX(14,N) :- testXX('#14. estStrictementInclus : bloc 1 avec bloc 7',
         (bloc(1,B1),bloc(7,B7),estStrictementInclus(B1,B7)),N).
trXX(15,N) :- testXX('#15. estStrictementInclus : bloc 0 avec 1',
         (bloc(0,B0),bloc(1,B1),\+ estStrictementInclus(B0,B1)),N).
trXX(16,N) :- testXX('#16. lesVoisins : agencement vide',
	 lesVoisins(gauche,x,vide,b1,[]),N).
trXX(17,N) :- testXX('#17. lesVoisins : pas voisins',
	 lesVoisins(gauche,x,un,h1,[]),N).
trXX(18,N) :- testXX('#18. lesVoisins : un voisin gauche sur x',
	 (lesVoisins(gauche,x,ag,b2,Bs),memesListes(Bs,[b1])),N).
trXX(19,N) :- testXX('#19. lesVoisins : un voisin droite sur y',
	 (lesVoisins(droite,y,ag,b3,Bs),memesListes(Bs,[b1])),N).
trXX(20,N) :- testXX('#20. lesVoisins : desvoisins gauche sur x',
	 (lesVoisins(gauche,x,ag,b4,Bs),memesListes(Bs,[b1,b2])),N).

% ----------------------------------------------------------------
% Mettre a on le flag "single_var_warnings" 
% ----------------------------------------------------------------
%:- prolog_flag(single_var_warnings,_,on).             

% ----------------------------------------------------------------
% Impression et calcul du résultat
% ----------------------------------------------------------------
/*
testTP :- nl,
  write('\n\nTESTS POUR -- lesIds(...)\n==================================================================\n'),
  trXX(1,N1),
  trXX(2,N2),
  trXX(3,N3),
  trXX(4,N4),
  Nm is (N1+N2)*0.5+N3+N4,
  nl,write('NOTE POUR CETTE PARTIE: '),write(Nm),write('/3'),nl,nl,
  write('\n\nTESTS POUR -- lesComposantsDuType(...)\n==================================================================\n'),
  trXX(5,N5),
  trXX(6,N6),
  trXX(7,N7),
  trXX(8,N8),
  trXX(9,N9),
  trXX(10,N10),
  Nn is (N5+N6)*0.5+N7+N8+N9+N10,
  nl,write('NOTE POUR CETTE PARTIE: '),write(Nn),write('/5'),nl,nl,
  write('\n\nTESTS POUR -- estStrictementInclus(...)\n==================================================================\n'),
  trXX(11,N11),
  trXX(12,N12),
  trXX(13,N13),
  trXX(14,N14),
  trXX(15,N15),
  Np is (N11+N12)*0.5+N13+N14+N15,
  nl,write('NOTE POUR CETTE PARTIE: '),write(Np),write('/4'),nl,nl,
  write('\n\nTESTS POUR -- lesVoisins(...)\n==================================================================\n'),
  trXX(16,N16),
  trXX(17,N17),
  trXX(18,N18),
  trXX(19,N19),
  trXX(20,N20),
  No is (N16+N17)*0.5+N18+N19+N20,
  nl,write('NOTE POUR CETTE PARTIE: '),write(No),write('/4'),nl,nl,
  nl,write('NOTE FINALE: '), N is (Nm+Nn+Np+No),
  write(N),write('/16'),nl.

:- testTP.
*/
