/*********************************************************************
* Cours		: INF2160
* Session	: Hiver 20013
* Objet		: Travail pratique 2
* Titre		: Module de gestion d'un agencement
* 
* Auteur	: Bernard Lefebvre
*********************************************************************/

laDimension(Idc,D) :- composant(_,Idc,D,_,_).
laPosition(Idc,P) :- composant(_,Idc,_,P,_).
leType(Idc,Ty) :- composant(_,Idc,_,_,Ty).

% lesComposantsDuBloc(Bloc,Compos)
% unifie Compos à la liste des composants localisés dans ce bloc
lesComposantsDuBloc([axCmp(_,Composant)|AxCmps],[Composant|Composants]) :-
                    lesComposantsDuBloc(AxCmps,Composants).
lesComposantsDuBloc([],[]).

% faireBloc(Axe,Composants,Bloc)
% unifie Bloc à une bloc formé à l'aide d'une liste de composants sur l'axe Axe
faireBloc(Axe,[C|Cs],[axCmp(Axe,C)|AxCmps]) :- faireBloc(Axe,Cs,AxCmps).
faireBloc(_,[],[]).

% intersectionBlocs(Bloc1,Bloc2,BlocI)
% unifie BlocI à intersection de Bloc1 et de Bloc2, les composants communs aux 2 blocs
% se retrouvent dans l'intersection indépendemment de l'axe sur lequel ils se trouvent
intersectionBlocs([axCmp(Axe,C)|AxCmps],Bloc,[axCmp(Axe,C)|AxCmpsI]) :-
	lesComposantsDuBloc(Bloc,Compos),
	member(C,Compos), !, intersectionBlocs(AxCmps,Bloc,AxCmpsI).
intersectionBlocs([_|AxCmps],Bloc,BlocI) :-
	intersectionBlocs(AxCmps,Bloc,BlocI).
intersectionBlocs([],_,[]).

% unionBlocs(Bloc1,Bloc2,BlocU)
% unifie BlocI à l'union de Bloc1 et de Bloc2, les composants des 2 blocs
% se retrouvent dans l'union une seule fois indépendemment de l'axe sur lequel ils se trouvent
unionBlocs([axCmp(_,C)|AxCmps],Bloc,BlocU) :-
	lesComposantsDuBloc(Bloc,Compos),
	member(C,Compos), !, unionBlocs(AxCmps,Bloc,BlocU).
unionBlocs([axCmp(Axe,C)|AxCmps],Bloc,[axCmp(Axe,C)|AxCmpsU]) :-
	unionBlocs(AxCmps,Bloc,AxCmpsU).
unionBlocs([],Bloc,Bloc).

estCategorie(meuble,Idc) :- composant(meuble,Idc,_,_,_).
estCategorie(electro,Idc) :- composant(electro,Idc,_,_,_).

laX(Idc,X) :- laPosition(Idc,position(X,_,_)).

laY(Idc,Y) :- laPosition(Idc,position(_,Y,_)).

laZ(Idc,Z) :- laPosition(Idc,position(_,_,Z)).

laHauteur(Idc,H) :- laDimension(Idc,(H,_,_)).

laLargeur(Idc,L) :- laDimension(Idc,(_,L,_)).

laProfondeur(Idc,P) :- laDimension(Idc,(_,_,P)).

% leComposant(IdAgencement,IdComposant)
% unifie IdComposant à un identifaint de composant de l'agencement d'identifiant
% IdAgencement
leComposant(Ida,Idc) :-
	agencement(Ida,IdCs),
	member(Idc,IdCs).

% estVoisin(Dir,Axe,C1,C2)
% retourne vrai si C1 est un voisin dans la direction Dir de C2 sur l'axe Axe
estVoisin(droite,Axe,C1,C2) :- estVoisin(gauche,Axe,C2,C1).
estVoisin(gauche,Axe,C1,C2) :-
	laX(C1,X1),
	laX(C2,X2),
	laY(C1,Y1),
	laY(C2,Y2),
	laZ(C1,Z1),
	laZ(C2,Z2),
	laLargeur(C2,L2),
	(Axe = x -> Xl2 is X2 + L2, Xl2 == X1, Y1 == Y2, Z1 == Z2;
	    Yl2 is Y2 - L2, Yl2 == Y1, X1 == X2, Z1 == Z2).

% À réaliser
% lesVoisins(Dir,Axe,Ag,Compo,Voisins)
% Dir est une direction
% Axe est un axe
% Ag est un agencement
% Compo est un composant 
% Voisins s'unifie à la liste des voisins de Compo sur l'axe Axe et dans la direction Dir
lesVoisins(_,_,_,Compo,[Compo]).

% leBloc(Ax,Ag,Compo,Bloc)
% Axe est un axe
% Ag est un agencement
% Bloc s'unfie au bloc sur l'axe Axe qui contient le composant Compo
leBloc(Axe,Ag,Compo,Bloc) :-
	lesVoisins(gauche,Axe,Ag,Compo,B1),
	lesVoisins(droite,Axe,Ag,Compo,B2),
	append(B1,[Compo|B2],B12),
	faireBloc(Axe,B12,Bloc).

% sontDansMemeBloc(Ag,C1,C2)
% Axe est un axe
% C1 et C2 sont des composants
% le prédicat est vrai si et seulement si C1 et C2 font partie d'un même bloc
sontDansMemeBloc(Ag,C1,C2) :-
	leBloc(x,Ag,C1,Bx),
	leBloc(y,Ag,C1,By),
	lesComposantsDuBloc(Bx,Cx),
	lesComposantsDuBloc(By,Cy),
	(member(C2,Cx);member(C2,Cy)).

% estDansBlocs(Blocs,C) 
% retourne vrai si le composant C se trouve dans un des blocs de la liste de blocs Blocs
estDansBlocs([Bloc|_],C) :-
	lesComposantsDuBloc(Bloc,Compos),
	member(C,Compos),!.
estDansBlocs([_|Blocs],C) :-
	estDansBlocs(Blocs,C).

% lesBlocs(Axe,Ag,Compos,Blocs)
% Axe est x ou y
% Ag est un agencement
% Compos est une liste de composants
% unifie Blocs à la liste des blocs sur l'axe formés avec chacun des composants de compos
lesBlocs(_,_,[],[]).
lesBlocs(Axe,Ag,[Compo|Compos],Blocs) :-
	lesBlocs(Axe,Ag,Compos,Blocps),
	(estDansBlocs(Blocps,Compo) -> Blocs = Blocps;
	    leBloc(Axe,Ag,Compo,Bloc),
	    Blocs = [Bloc|Blocps]).

% lesBlocs(Axe,Ag,Blocs)
% Axe est x ou y
% Ag est un agencement
% unifie Blocs à la liste des blocs sur l'axe
lesBlocs(Axe,Ag,Blocs) :-
	agencement(Ag,Compos),
	lesBlocs(Axe,Ag,Compos,Blocs).

% lesBlocs(Ag,Blocs)
% Ag est un agencement
% unifie Blocs à la liste des blocs de l'agencement
% Cetet liste est obtenue en prenant les listes correspondants à chaque axe
% et en éliminant les blocs d'un axe qui sont inclus strictement dans les blocs de l'autre axe
lesBlocs(Ag,Blocs) :-
	lesBlocs(x,Ag,BlocsX),
	lesBlocs(y,Ag,BlocsY),
	elimineBlocs(BlocsY,BlocsX,BlocsYp),
	elimineBlocs(BlocsX,BlocsY,BlocsXp),
	append(BlocsXp,BlocsYp,Blocs).

elimineBlocs([BlocY|BlocsY],BlocsX,Blocs) :-
	elimineBloc(BlocY,BlocsX),!,
	elimineBlocs(BlocsY,BlocsX,Blocs).
elimineBlocs([BlocY|BlocsY],BlocsX,[BlocY|Blocs]) :-
	elimineBlocs(BlocsY,BlocsX,Blocs).
elimineBlocs([],_,[]).

elimineBloc(BlocY,[BlocX|_]) :-
	estStrictementInclus(BlocY,BlocX),!.
elimineBloc(BlocY,[_|BlocsX]) :-
	elimineBloc(BlocY,BlocsX).

% À réaliser
% estStrictementInclus(BlocY,BlocX)
% est vrai si et seulemnt si BlocY est strictement inclus dans BlocX
% peu importe l'ordre des composants dans les blocs.
estStrictementInclus(_,_).

% À réaliser
% lesComposantsDuType(IdAgencement,Ty,Compos)
% unifie Compos à la liste des composants de l'agencement qui sont du type Ty
lesComposantsDuType(_,_,[]).

% possedeTypes(Ag,Types)
% Est vrai si et seulement si l'agencement Ag possède tous les types de composants
% de la liste Types
possedeTypes(Ag,[Ty|Types]) :-
	lesComposantsDuType(Ty,Ag,[_|_]),
	possedeTypes(Ag,Types).
possedeTypes(_,[]).

% À réaliser
% lesIds(Blocs,Css)
% unifie Css à la liste des listes des identificateurs des composants d'une liste de blocs
lesIds(_,[]).
