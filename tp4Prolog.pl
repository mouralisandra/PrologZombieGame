%Travail réalisé par :
% Eya Ridene,Sandra Mourali, Ksontini Mariem, Azzabi Sofienne
%Exercice 1 : Données du jeu

%1/ Déclaration des relations :

:- dynamic rocher/2.
:- dynamic arbre/2.

%2/ Définition de la relation vache(X, Y, Race, Etat) :

:- dynamic vache/4.

vache(2, 3, brune, vivante).
vache(4, 5, simmental, vivante).
vache(6, 1, alpine_herens, zombie).

% 3/ Définition de la relation dimitri(X, Y) :
%
:- dynamic dimitri/2.

%4/ Définition des faits largeur(X) et hauteur(Y) :

largeur(10).
hauteur(10).

%5/ Définition des faits :

nombre_rochers(7).
nombre_arbres(11).
nombre_vaches(brune, 2).
nombre_vaches(simmental, 4).
nombre_vaches(alpine_herens, 1).

%Exercice 2 : Initialisation 1

%1/ Régle occupe(X,Y) :

occupe(X, Y) :- rocher(X, Y).
occupe(X, Y) :- arbre(X, Y).
occupe(X, Y) :- vache(X, Y, _, _).
occupe(X, Y) :- dimitri(X, Y).

%2/ Régle libre(X,Y) :
libre(X,Y):-largeur(L),hauteur(H),repeat,X is random(L),Y is random(H),(not(occupe(X,Y))),!.

%3/ Régles placer_rochers(N), placer_arbres(N), placer_vaches(Race, N), et placer_dimitri :

placer_rochers(0).
placer_rochers(N) :- N > 0, libre(X, Y), assert(rocher(X, Y)), N1 is N - 1, placer_rochers(N1).
placer_arbres(0).
placer_arbres(N) :- N > 0, libre(X, Y), assert(arbre(X, Y)), N1 is N - 1, placer_arbres(N1).

placer_vaches(_, 0).
placer_vaches(Race, N) :- N > 0, libre(X, Y), assert(vache(X, Y, Race, vivante)), N1 is N - 1, placer_vaches(Race, N1).
placer_dimitri :- libre(X, Y), assert(dimitri(X, Y)).

%4/ Régle vaches(L) :

vaches(L) :- bagof((X,Y), vache(X, Y, _, _), L).

%5/ Régle creer_zombie :

creer_zombie :-
vaches(L),
length(L, N), random(1, N, K),
nth1(K, L, (X, Y)),
retract(vache(X, Y, Race, _)),
assert(vache(X, Y, Race, zombie)).

%Exercice 3 : le jeu des vaches zombies

% 1
question(R) :-
    write('Dans quelle direction voulez-vous déplacer Dimitri ? (reste, nord, sud, est, ouest) '),
    read(X),
    (   member(X, [reste, nord, sud, est, ouest]) ->
        R = X
    ;
        write('Direction invalide. Veuillez réessayer.'), nl,
        question(R)
    ).

% 2

zombification(X, Y) :-
    vache(X, Y, _, zombie),
    ((X1 is X-1, Y1 is Y, vache(X1, Y1, _, vivante), retract(vache(X1, Y1, R, _)), assert(vache(X1, Y1, R, zombie)));
     (X2 is X+1, Y2 is Y, vache(X2, Y2, _, vivante), retract(vache(X2, Y2, R, _)), assert(vache(X2, Y2, R, zombie)));
     (X3 is X, Y3 is Y-1, vache(X3, Y3, _, vivante), retract(vache(X3, Y3, R, _)), assert(vache(X3, Y3, R, zombie)));
     (X4 is X, Y4 is Y+1, vache(X4, Y4, _, vivante), retract(vache(X4, Y4, R, _)), assert(vache(X4, Y4, R, zombie)))
    ).

zombification_liste([]).
zombification_liste([(X,Y)|L]) :- zombification(X, Y), zombification_liste(L).

zombification :-
    vaches(L),
    zombification_liste(L).

% 3

deplacement_vache(X, Y, Direction) :-
    (
        (Direction = nord, X1 is X-1, Y1 is Y);
        (Direction = sud, X1 is X+1, Y1 is Y);
        (Direction = est, X1 is X, Y1 is Y+1);
        (Direction = ouest, X1 is X, Y1 is Y-1);
        (Direction = reste, X1 is X, Y1 is Y)
    ),
    X1 > 0, X1 =<10,
    Y1 > 0, Y1 =< 10,
    libre(X1, Y1),
    retract(vache(X, Y, Race, Etat)),
    assert(vache(X1, Y1, Race, Etat)).

deplacement_vaches(R):-
  vache(X,Y,_,_),
  deplacement_vache(X,Y,R).

deplacement_vaches(_).


% 4

deplacement_joueur(Direction) :-
    dimitri(X, Y),
    (   (Direction = nord, X1 is X-1, Y1 is Y);
        (Direction = sud, X1 is X+1, Y1 is Y);
        (Direction = est, X1 is X, Y1 is Y+1);
        (Direction = ouest, X1 is X, Y1 is Y-1);
        (Direction = reste, X1 is X, Y1 is Y)
    ),
    X1 > 0, X1 =< 10,
    Y1 > 0, Y1 =< 10,
    libre(X1, Y1),
    retract(dimitri(X, Y)),
    assert(dimitri(X1, Y1)).

% 5

verification:-
  dimitri(X,Y),
  X1 is X+1,not(vache(X1,Y,_,zombie)),
  X2 is X-1,not(vache(X2,Y,_,zombie)),
  Y1 is Y-1,not(vache(X,Y1,_,zombie)),
  Y2 is Y+1,not(vache(X,Y2,_,zombie)).

% le reste est le code prédéfini du jeu

 initialisation :-
  nombre_rochers(NR),
  placer_rochers(NR),
  nombre_arbres(NA),
  placer_arbres(NA),
  nombre_vaches(brune, NVB),
  placer_vaches(brune, NVB),
  nombre_vaches(simmental, NVS),
  placer_vaches(simmental, NVS),
  nombre_vaches(alpine_herens, NVH),
  placer_vaches(alpine_herens, NVH),
  placer_dimitri,
  creer_zombie,
  !.

affichage(L, _) :-
  largeur(L),
  nl.

affichage(L, H) :-
  rocher(L, H),
  print('O'),
  L_ is L + 1,
  affichage(L_, H).

affichage(L, H) :-
  arbre(L, H),
  print('T'),
  L_ is L + 1,
  affichage(L_, H).
affichage(L, H) :-
  dimitri(L, H),
  print('D'),
  L_ is L + 1,
  affichage(L_, H).

affichage(L, H) :-
  vache(L, H, brune, vivante),
  print('B'),
  L_ is L + 1,
  affichage(L_, H).
affichage(L, H) :-
  vache(L, H, brune, zombie),
  print('b'),
  L_ is L + 1,
  affichage(L_, H).

affichage(L, H) :-
  vache(L, H, simmental, vivante),
  print('S'),
  L_ is L + 1,
  affichage(L_, H).
affichage(L, H) :-
  vache(L, H, simmental, zombie),
  print('s'),
  L_ is L + 1,
  affichage(L_, H).

affichage(L, H) :-
  vache(L, H, alpine_herens, vivante),
  print('H'),
  L_ is L + 1,
  affichage(L_, H).
affichage(L, H) :-
  vache(L, H, alpine_herens, zombie),
  print('h'),
  L_ is L + 1,
  affichage(L_, H).

affichage(L, H) :-
  \+ occupe(L, H),
  print('.'),
  L_ is L + 1,
  affichage(L_, H).

affichage(H) :-
  hauteur(H).

affichage(H) :-
  hauteur(HMax),
  H < HMax,
  affichage(0, H),
  H_ is H + 1,
  affichage(H_).

affichage :-
  affichage(0),!.



jouer :-
  initialisation,
  tour(0, _).

tour_(_, _) :-
  \+ verification,
  write('Dimitri s\'est fait mordre'),!.
tour_(N, R) :-
  verification,
  M is N + 1,
  tour(M, R).

tour(N, R) :-
  affichage,
  question(R),
  deplacement_joueur(R),
  deplacement_vaches(R),
  zombification,
  tour_(N, R).
