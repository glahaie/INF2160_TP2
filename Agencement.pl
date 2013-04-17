/*********************************************************************
* Cours		: INF2160
* Session	: Hiver 20013
* Objet		: Travail pratique 2
* Titre		: Module de gestion d'un agencement
* 
* Auteur	: Bernard Lefebvre
*
* Modifi� par Guillaume Lahaie
*             LAHG04077707
*             Derni�re modification: 17 avril 2013
*
*             Les pr�dicats lesIds,lesComposantsDuType, estSrictementInclus
*             et les voisins ont �t� d�finis pour le TP2. Les pr�dicats
*             lesComposantsDuType2 et sousListe ont aussi �t� d�finis
*             pour la r�alisation des pr�dicats mentionn�s.
*********************************************************************/

laDimension(Idc,D) :- composant(_,Idc,D,_,_).
laPosition(Idc,P) :- composant(_,Idc,_,P,_).
leType(Idc,Ty) :- composant(_,Idc,_,_,Ty).

% lesComposantsDuBloc(Bloc,Compos)
% unifie Compos � la liste des composants localis�s dans ce bloc
lesComposantsDuBloc([axCmp(_,Composant)|AxCmps],[Composant|Composants]) :-
                    lesComposantsDuBloc(AxCmps,Composants).
lesComposantsDuBloc([],[]).

% faireBloc(Axe,Composants,Bloc)
% unifie Bloc � une bloc form� � l'aide d'une liste de composants sur l'axe Axe
faireBloc(Axe,[C|Cs],[axCmp(Axe,C)|AxCmps]) :- faireBloc(Axe,Cs,AxCmps).
faireBloc(_,[],[]).

% intersectionBlocs(Bloc1,Bloc2,BlocI)
% unifie BlocI � intersection de Bloc1 et de Bloc2, les composants communs aux 2 blocs
% se retrouvent dans l'intersection ind�pendemment de l'axe sur lequel ils se trouvent
intersectionBlocs([axCmp(Axe,C)|AxCmps],Bloc,[axCmp(Axe,C)|AxCmpsI]) :-
	lesComposantsDuBloc(Bloc,Compos),
	member(C,Compos), !, intersectionBlocs(AxCmps,Bloc,AxCmpsI).
intersectionBlocs([_|AxCmps],Bloc,BlocI) :-
	intersectionBlocs(AxCmps,Bloc,BlocI).
intersectionBlocs([],_,[]).

% unionBlocs(Bloc1,Bloc2,BlocU)
% unifie BlocI � l'union de Bloc1 et de Bloc2, les composants des 2 blocs
% se retrouvent dans l'union une seule fois ind�pendemment de l'axe sur lequel ils se trouvent
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
% unifie IdComposant � un identifaint de composant de l'agencement d'identifiant
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lesVoisins(Dir,Axe,Ag,Compo,Voisins)
% Dir est une direction
% Axe est un axe
% Ag est un agencement
% Compo est un composant 
% Voisins s'unifie � la liste des voisins de Compo sur l'axe Axe et dans la 
% direction Dir. Ici, la liste des voisins comprend tous les blocs li�s dans
% la direction donn�e. Par exemple, si b1 est un voisin de b2 et b2 est un 
% voisin de Compo, alors b1 et b2 seront dans la liste Voisins.
%
%On tente donc de trouver un voisin de Compo � partir de la liste des composants
%de l'agencement donn�. Si on trouve un voisin, on cherche alors les voisins de
%ce composant dans la liste originale, dans la m�me direction donn�e (sinon, on
%pourrait obtenir une r�curision sans fin). On ajoute ce r�sultat � la liste Voisins,
%et on continue ensuite � traiter le reste de la liste 
%
%Exemple d'utilisation:
%| ?- lesVoisins(droite,y,ag,b3,Bs).
%Bs = [b1] ? 
%yes
%| ?- lesVoisins(gauche,x,ag,b4,Bs).
%Bs = [b2,b1] ? 
%yes
lesVoisins(Dir,Axe,Ag,Compo,Voisins) :- agencement(Ag,L),
                                        lesVoisins(Dir,Axe,Ag,L,Compo, Voisins),!.

lesVoisins(_,_,_,[],_,[]).
lesVoisins(Dir,Axe,Ag,[X|Xs],Compo,[X|Ys]) :- estVoisin(Dir,Axe,Compo,X),
                                lesVoisins(Dir,Axe,Ag,X,Voisins),append(Voisins,Zs,Ys),
                                lesVoisins(Dir,Axe,Ag,Xs,Compo,Zs).
lesVoisins(Dir,Axe,Ag,[X|Xs],Compo,Ys) :- \+ estVoisin(Dir,Axe,Compo,X),
                                                lesVoisins(Dir,Axe,Ag,Xs,Compo,Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
% le pr�dicat est vrai si et seulement si C1 et C2 font partie d'un m�me bloc
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
% unifie Blocs � la liste des blocs sur l'axe form�s avec chacun des composants de compos
lesBlocs(_,_,[],[]).
lesBlocs(Axe,Ag,[Compo|Compos],Blocs) :-
	lesBlocs(Axe,Ag,Compos,Blocps),
	(estDansBlocs(Blocps,Compo) -> Blocs = Blocps;
	    leBloc(Axe,Ag,Compo,Bloc),
	    Blocs = [Bloc|Blocps]).

% lesBlocs(Axe,Ag,Blocs)
% Axe est x ou y
% Ag est un agencement
% unifie Blocs � la liste des blocs sur l'axe
lesBlocs(Axe,Ag,Blocs) :-
	agencement(Ag,Compos),
	lesBlocs(Axe,Ag,Compos,Blocs).

% lesBlocs(Ag,Blocs)
% Ag est un agencement
% unifie Blocs � la liste des blocs de l'agencement
% Cetet liste est obtenue en prenant les listes correspondants � chaque axe
% et en �liminant les blocs d'un axe qui sont inclus strictement dans les blocs de l'autre axe
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%estStrictementInclus(BlocY,Blocx):
%BlocX et BlocY sont des listes de blocs
%Le pr�dicat v�rifie si tous les composants de BlocX sont inclus dans
%les blocs de BlocX, tel que BlocX contient au moins un autre composant
%qui n'est pas contenu dans BlocY (selon la d�finition ensembliste d'un
%ensemble strictement inclus dans un autre ensemble).
%On obtient donc la liste des composants des deux blocs, et ensuite on compare
%on v�rifie si la liste des composants de BlocX est une sous-liste de la liste
%de BlocY, et aussi si la liste des composants de BlocY n'est pas une sous-liste
%de BlocX.
%
%Je d�finie ici le pr�dicat sousListe pour v�rifier si une liste est une 
%sous-liste d'une autre liste. On pourrait aussi utiliser subset, d�fini
%par prolog, mais je pr�f�re le faire moi-m�me.
%
%Exemple d'utilisation:
%| ?- bloc(1,B1),bloc(7,B7),estStrictementInclus(B1,B7).
%B1 = [axCmp(x,b1),axCmp(x,b2),axCmp(x,b4)],
%B7 = [axCmp(x,b1),axCmp(x,b2),axCmp(y,b3),axCmp(x,b4)] ? 
%yes
estStrictementInclus(By,Bx) :- lesComposantsDuBloc(By, Cys),
                               lesComposantsDuBloc(Bx, Cxs),
                               sousListe(Cys,Cxs),\+sousListe(Cxs,Cys).

%sousListe(L1,L2):
%L1 et L2 sont deux listes, de n'importe quel type d'�l�ments
%sousListe v�rifie si la liste L1 est une sous-liste de L2.
%Exemples d'utilisation:
%| ?- sousListe([1,2,3],[1,2,3,4,5,6]).
%yes
%| ?- sousListe([1,2,3],[1,3,4,5,6]).
%no
sousListe([],_).
sousListe([X|Xs],Liste) :- member(X,Liste),sousListe(Xs,Liste).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%lesComposantsDuType(IdAgencement, Ty, Compos):
%idAgencement: un agencement, fourni selon son identificateur
%Ty: le type de composant recherch�
%Compos: s'unifie � la liste des composants de type Ty contenus dans l'agencement
%idAgencement. La liste Compos doit contenir tous les �l�ments qui s'unifient, et
%non une sous-liste. Je d�finie un deuxi�me pr�dicat pour v�rifier la liste des
%composants, alors que lesComposantsDuType obtient la liste des composants
%d'un agencement qui est utilis� par le second pr�dicat.
%
%On obtient tout d'abord la liste des composants de l'agencement, et ensuite on
%v�rifie chaque agencement. Si l'agencement est du type recherch�, on l'ins�re
%dans Compos, sinon, on passe au prochain �l�ment de la liste.
%Exemple d'utilisation:
%| ?- lesComposantsDuType(ag,bas,Bs).
%Bs = [b3,b4] ? 
%yes
lesComposantsDuType(Id,Ty,Compos) :- agencement(Id,Ags),
                                     lesComposantsDuType2(Ty,Ags,Compos).


%lesComposantsDuType'(Ty,Composants,Compos).
%Ty est le type de composant recherch�.
%Composants est la liste comprenants tous les composants.
%Compos s'unifie � la liste des composants du type Ty.
%Compos doit contenir tous les �l�ments du type recherche, et non seulement
%une sous-liste.
%exemple d'utilisation:
%| ?- lesComposantsDuType2(haut,[b1,b2,b3,b4,b5,b6,h1,h2],Compos).
%Compos = [b5,b6,h1,h2] ? 
%yes
lesComposantsDuType2(Ty,[X|Xs],[X|Q]) :- composant(_,X,_,_,Ty),!,
                                         lesComposantsDuType2(Ty,Xs,Q).
lesComposantsDuType2(Ty,[_|Xs], Q) :- !,lesComposantsDuType2(Ty,Xs,Q).
lesComposantsDuType2(_,[],[]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% possedeTypes(Ag,Types)
% Est vrai si et seulement si l'agencement Ag poss�de tous les types de composants
% de la liste Types
possedeTypes(Ag,[Ty|Types]) :-
	lesComposantsDuType(Ty,Ag,[_|_]),
	possedeTypes(Ag,Types).
possedeTypes(_,[]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% lesIds(Blocs,Css)
% Blocs: Liste de blocs
% Css: Liste de liste d'identificateurs de composants.
% lesIds Unifient Css avec la liste de listes ce composants des blocs de la 
% liste Blocs.Il est possible aussi d'unifier la liste Blocs avec les
% blocs associ�s � la liste de listes de composants, toutefois l'information
% dans les blocs sera incompl�te, on ne peut obtenir la direction de l'axe.
% Exemple d'utilisation:
%| ?- bloc(1,Bloc1),bloc(2,Bloc2),lesIds([Bloc1,Bloc2],Ids).
%Ids = [[b1,b2,b4],[b5,b6]],
%Bloc1 = [axCmp(x,b1),axCmp(x,b2),axCmp(x,b4)],
%Bloc2 = [axCmp(x,b5),axCmp(x,b6)] ? 
%yes 

lesIds([X|Xs], [C|Css]) :- lesComposantsDuBloc(X, C),lesIds(Xs,Css).
lesIds([],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
