%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Apellidos:     Cerezo Pomykol
% Nombre:        Jan
% Nº Matrícula:  a180305
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(_,_,[assertions]).
:- use_package(nativeprops).
:- use_module(library(unittest/unittest_props)).
:- doc(title, "Documentación de la práctica").
:- doc(author, "Cerezo Pomykol, Jan (a180305)").

:- pred alumno_prode(A, B, C, D)
   # "Cerezo Pomykol, Jan, a180305".
alumno_prode('Cerezo', 'Pomykol', 'Jan', 'a180305').

% @@@@@@@@@@@@@@@@@@@@ nat @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- prop nat(N)
   # "@var{N} es un número natural.".
nat(0).
nat(s(X)) :-
    nat(X).

% ···················· tests nat ··········································
:- test nat(N)
   : (N = 0) + not_fails # "Caso base.".

:- test nat(N)
   : (N = s(s(0))) + not_fails # "Caso natural.".

:- test nat(N)
   : (N = a) + fails # "Caso no natural.".

% @@@@@@@@@@@@@@@@@@@@ is_list @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- prop is_list(L) :: (is_list(L))
   # "@var{L} es una lista.".
is_list([_|Li]) :-
    is_list(Li).
is_list([]).

% ···················· tests is_list ······································
:- test is_list(L)
   : (L = []) + not_fails # "Caso base.".

:- test is_list(L)
   : (L = [a, b]) + not_fails # "Caso lista.".

:- test is_list(L)
   : (L = 0) + fails # "Caso no lista.".

% @@@@@@@@@@@@@@@@@@@@ even @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- prop even(N) :: (nat(N))
   # "@var{N} es un número natural par.".
even(0).
even(s(X)) :-
    odd(X).

% ···················· tests even ·········································
:- test even(N)
   : (N = 0) + not_fails # "Caso base (0 es par).".

:- test even(N)
   : (N = s(s(s(s(0))))) + not_fails # "Caso par (4).".

:- test even(N)
   : (N = s(s(s(0)))) + fails # "Caso impar (3).".

% @@@@@@@@@@@@@@@@@@@@ odd @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- prop odd(N) :: (nat(N))
   # "@var{N} es un número natural impar.".
odd(s(X)) :-
    even(X).

% ···················· tests odd ··········································
:- test odd(N)
   : (N = 0) + fails # "Caso base.".

:- test odd(N)
   : (N = s(s(s(s(0))))) + fails # "Caso par (4).".

:- test odd(N)
   : (N = s(s(s(0)))) + not_fails # "Caso impar (3).".

% @@@@@@@@@@@@@@@@@@@@ lon @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- prop lon(N, L) :: (nat(N), is_list(L))
   # "La lista @var{L} es de longitud @var{N}.".
lon(0, []).
lon(s(N), [_|T]) :-
    lon(N, T).

% ···················· tests lon ··········································
:- test lon(N, L)
   : (L = [])
   => (N = 0) + not_fails # "Caso base.".

:- test lon(N, L)
   : (N = s(s(s(0))), L = [a, b, c])
   + not_fails # "Caso 1.".

% @@@@@@@@@@@@@@@@@@@@ suma @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred suma(X, Y, Z) :: (nat(X), nat(Y), nat(Z))
   # "@var{Z} es la suma de @var{X} e @var{Y}.".
suma(0, X, X).         % 0 + X = X
suma(s(X), Y, s(Z)) :- % (X+1)+y = Z+1  => X+Y = Z
    suma(X, Y, Z).

% ···················· tests suma ·········································
:- test suma(X, Y, Z)
   : (X = 0 , Y = s(s(0)))
   => (Z = s(s(0))) + not_fails # "Caso base.".

:- test suma(X, Y, Z)
   : (X = s(0) , Y = s(s(0)))
   => (Z = s(s(s(0)))) + not_fails # "Caso de suma 1 (1 + 2 = 3).".

:- test suma(X, Y, Z)
   : (X = s(s(s(0))), Y = 0)
   => (Z = s(s(s(0)))) + not_fails # "Caso de suma 2 (3 + 0 = 3).".

:- test suma(X, Y, Z)
   : (X = s(s(s(0))) , Y = s(s(0)))
   => (Z = s(s(s(s(s(0)))))) + not_fails # "Caso de suma 3 (3 + 2 = 5).".

:- test suma(X, Y, Z)
   : (Y = s(s(0)) , Z = s(s(0)))
   => (X = 0) + not_fails # "Caso de resta 1 (2 - 2 = 0).".

:- test suma(X, Y, Z)
   : (Y = s(s(s(0))), Z = s(s(s(0))))
   => (X = 0) + not_fails # "Caso de resta 2 (3 - 3 = 0).".

:- test suma(X, Y, Z)
   : (Y = s(s(0)) , Z = s(s(s(s(s(0))))))
   => (X = s(s(s(0)))) + not_fails # "Caso de resta 3 (5 - 2 = 3).".

% @@@@@@@@@@@@@@@@@@@@ times @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred times(X, Y, Z) :: (nat(X), nat(Y), nat(Z))
   # "@var{Z} es el producto de @var{X} e @var{Y}.".
times(0, _, 0).      % 0 * Y = 0
times(s(X), Y, Z) :- % (X+1)*Y = X*Y + Y
    times(X, Y, Zi),
    suma(Y, Zi, Z).

% ···················· tests times ········································
:- test times(X, Y, Z)
   : (X = 0, Y = s(0))
   => (Z = 0) + not_fails # "Caso base.".

:- test times(X, Y, Z)
   : (X = s(0), Y = s(0))
   => (Z = s(0)) + not_fails # "Caso de multiplicación 1 (1 * 1 = 1).".

:- test times(X, Y, Z)
   : (X = s(0), Y = 0)
   => (Z = 0) + not_fails # "Caso de multiplicación 2 (1 * 0 = 0).".

:- test times(X, Y, Z)
   : (X = s(s(0)), Y = s(s(s(0))))
   => (Z = s(s(s(s(s(s(0))))))) + not_fails # "Caso de multiplicación 3 (2 * 3 = 6).".

% @@@@@@@@@@@@@@@@@@@@ concat @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred concat(L1, L2, L3) :: (is_list(L1), is_list(L2), is_list(L3))
   # "@var{L3} es la lista resultante de concatenar @var{L1} y @var{L2}.".
concat([],Ys,Ys).
concat([X|Xs],Ys,[X|Zs]) :-
    concat(Xs,Ys,Zs).

% ···················· tests concat ·······································
:- test concat(L1, L2, L3)
   : (L1 = [], L2 = [a, b])
   => (L3 = [a, b]) + not_fails # "Caso 1.".

:- test concat(L1, L2, L3)
   : (L1 = [a], L2 = [])
   => (L3 = [a]) + not_fails # "Caso 2.".

:- test concat(L1, L2, L3)
   : (L1 = [], L2 = [])
   => (L3 = []) + not_fails # "Caso 3.".
   
:- test concat(L1, L2, L3)
   : (L1 = [a, b, c], L2 = [d, e])
   => (L3 = [a, b, c, d, e]) + not_fails # "Caso 4.".

:- test concat(L1, L2, L3)
   : (L2 = [d, e], L3 = [a, b, c, d, e])
   => (L1 = [a, b, c]) + not_fails # "Caso 5.".

:- test concat(L1, L2, L3)
   : (L1 = [a, b, c], L3 = [a, b, c, d, e])
   => (L2 = [d, e]) + not_fails # "Caso 6.".

% @@@@@@@@@@@@@@@@@@@@ nums @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred nums(N, L) :: (nat(N), is_list(L))
   # "@var{L} es la lista con los números naturales desde @var{N} hasta 1 en orden descendente.".
nums(0, []).
nums(s(N), [s(N)|Li]) :-
    nums(N, Li). % el elemento n-esimo (desde el final) de la lista es igual a N.

% ···················· tests nums ·········································
:- test nums(N, L)
   : (N = 0)
   => (L = []) + not_fails # "Caso base.".

:- test nums(N, L)
   : (N = s(s(s(0))))
   => (L = [s(s(s(0))), s(s(0)), s(0)]) + not_fails # "Caso 1.".

:- test nums(N, L)
   : (L = [s(s(s(s(0)))), s(s(s(0))), s(s(0)), s(0)])
   => (N = s(s(s(s(0))))) + not_fails # "Caso 2.".

:- test nums(N, L)
   : (N = s(s(s(0))), L = [s(s(s(0))), s(s(0)), s(0)]) + not_fails # "Caso 3.".

% @@@@@@@@@@@@@@@@@@@@ sumlist @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred sumlist(L, S) :: (is_list(L), nat(S))
   # "@var{S} es la suma de los elementos de la lista @var{L}.".
sumlist([], 0).        % suma([]) = 0
sumlist([L|Li], S) :-  % suma([L|Li]) = L + suma(Li)
    sumlist(Li, Si),
    suma(L, Si, S).

% ···················· tests sumlist ······································
:- test sumlist(L, S)
   : (L = [])
   => (S = 0) + not_fails # "Caso base.".

:- test sumlist(L, S)
   : (L = [s(s(s(0))), s(0), 0, s(s(0))])
   => (S = s(s(s(s(s(s(0))))))) + not_fails # "Caso 1.".

% @@@@@@@@@@@@@@@@@@@@ choose_one @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred choose_one(E, L, R) :: (is_list(L), (is_list(R)))
   # "@var{R} es la lista de elementos de @var{L} que no contiene @var{E}.".
choose_one(E, [L|Li], [L|Ri]) :- % si no es el primero, seguir con el resto.
    choose_one(E, Li, Ri).
choose_one(E, [E|L], L). % si es el primero

% ···················· tests choose_one ···································
:- test choose_one(E, L, R)
   : (E = a, L = [a, b, c])
   => (R = [b, c]) + not_fails # "Caso base.".

:- test choose_one(E, L, R)
   : (E = a, L = [b, c, a, d])
   => (R = [b, c, d]) + not_fails # "Caso 1.".

:- test choose_one(E, L, R)
   : (L = [a, b, c], R = [a, c])
   => (E = b) + not_fails # "Caso 2.".

% @@@@@@@@@@@@@@@@@@@@ perm @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred perm(L, LP) :: (is_list(L), is_list(LP))
   # "@var{LP} es una permutacion de los elementos de la lista @var{L}.".
perm(L, [LP|LPi]) :-
    choose_one(LP, L, Li), % buscar siguiente alternativa.
    perm(Li, LPi).
perm([], []).

% ···················· tests perm ·········································
:- test perm(L, LP)
   : (L = [])
   => (LP = []) + not_fails # "Caso base.".

:- test perm(L, LP)
   : (L = [a, b, c]) +
   ( try_sols(10),
     solutions([perm(L, [c, b, a]), perm(L, [c, a, b]),
                perm(L, [b, c, a]), perm(L, [b, a, c]),
                perm(L, [a, c, b]), perm(L, [a, b, c])])) # "Caso 1.".

% @@@@@@@@@@@@@@@@@@@@ split @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred split(L, L1, L2) :: (is_list(L), is_list(L1), is_list(L2))
   # "@var{L1} contiene los elementos de las posiciones pares de @var{L}, @var{L2} los de las impares.".
split(L, L1, L2) :-
    split_even(L, L1, L2).

:- pred split_even(L, L1, L2) :: (is_list(L), is_list(L1), is_list(L2))
   # "@var{L1} comienza por el primer elemento de @var{L}.
      Predicado auxiliar para separar los elementos de las posiciones pares.".
split_even([], [], []). % caso base
split_even([L|Li], [L|L1], L2) :-
    split_odd(Li, L1, L2).
:- pred split_odd(L, L1, L2) :: (is_list(L), is_list(L1), is_list(L2))
   # "@var{L2} comienza por el primer elemento de @var{L}.
      Predicado auxiliar para separar los elementos de las posiciones impares.".
split_odd([L|Li], L1, [L|L2]) :-
    split_even(Li, L1, L2).

% ···················· tests split ········································
% No tiene sentido hacer tests de split_even/3 y split_odd/3.
:- test split(L, L1, L2)
   : (L = [])
   => (L1 = [], L2 = []) + not_fails # "Caso base.".

:- test split(L, L1, L2)
   : (L = [a, b])
   => (L1 = [a], L2 = [b]) + not_fails # "Caso 1 ([a, b]).".

:- test split(L, L1, L2)
   : (L = [s(s(s(s(0)))), s(s(s(0))), s(s(0)), s(0)])
   => (L1 = [s(s(s(s(0)))), s(s(0))], L2 = [s(s(s(0))), s(0)]) + not_fails # "Caso 2 ([4, 3, 2, 1]).".

:- test split(L, L1, L2)
   : (L = [a, b, c]) + fails # "Caso 3 (lista de longitud impar).".

:- test split(L, L1, L2)
   : (L1 = [a, a, a], L2 = [b, b, b])
   => (L = [a, b, a, b, a, b]) + not_fails # "Caso 4.".

% @@@@@@@@@@@@@@@@@@@@ sumlists @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred sumlists(N, L1, L2, S) :: (nat(N), is_list(L1), is_list(L2), nat(S))
   # "@var{L1} y @var{L2} son listas que contienen todos los números 
      naturales desde 1 hasta @var{N}, además la suma los elementos
      de cada lista es @var{S}.".
sumlists(N, L1, L2, S) :-
    even(N),               % comprobar si es entero
    nums(N, L),            % generar la lista (L) de N a 1
    perm(L, LP),           % obtener las permutaciones (LP) de L
    split(LP, L1, L2),     % dividir las listas LP
    sumlist(L1, S),        % comprobar las sumas de ambas listas
    sumlist(L2, S).

% ···················· tests sumlists ·····································
:- test sumlists(N, L1, L2, S)
   : (N = 0, L1 = [], L2 = [], S = 0) + not_fails # "Caso base).".

:- test sumlists(N, L1, L2, S)
   : (N = s(0)) + fails # "Caso N es impar.".

:- test sumlists(N, L1, L2, S)
   : (N = s(s(0))) + fails # "Caso 1 (no existen L1, L2).".

:- test sumlists(N, L1, L2, S)
   : (N = s(s(s(s(0))))) +
   (try_sols(10),
    solutions([
        sumlists(N, [s(0), s(s(s(s(0))))], [s(s(0)), s(s(s(0)))], s(s(s(s(s(0)))))),
        sumlists(N, [s(0), s(s(s(s(0))))], [s(s(s(0))), s(s(0))], s(s(s(s(s(0)))))),
        sumlists(N, [s(s(0)), s(s(s(0)))], [s(0), s(s(s(s(0))))], s(s(s(s(s(0)))))),
        sumlists(N, [s(s(0)), s(s(s(0)))], [s(s(s(s(0)))), s(0)], s(s(s(s(s(0)))))),
        sumlists(N, [s(s(s(0))), s(s(0))], [s(0), s(s(s(s(0))))], s(s(s(s(s(0)))))),
        sumlists(N, [s(s(s(0))), s(s(0))], [s(s(s(s(0)))), s(0)], s(s(s(s(s(0)))))),
        sumlists(N, [s(s(s(s(0)))), s(0)], [s(s(0)), s(s(s(0)))], s(s(s(s(s(0)))))),
        sumlists(N, [s(s(s(s(0)))), s(0)], [s(s(s(0))), s(s(0))], s(s(s(s(s(0))))))
    ])) # "Caso 2.".

% @@@@@@@@@@@@@@@@@@@@ divide_sub @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred divide_sub(L, N, Res, S) :: (is_list(L), nat(N), is_list(Res), nat(S))
   # "@var{Res} contiene todos los elementos de @var{L} distribuidos en listas
      de @var{N} elementos. La suma de los elementos de cada una de estas listas
      es @var{S}.".
divide_sub([], _, [], _).
divide_sub(L, N, [R|Ri], S) :-
    concat(R, Aux, L),
    lon(N, R),   % si no se pone límite superior, termina en un bucle infinito
    sumlist(R, S),  % comprobar que la suma sea igual a S
    divide_sub(Aux, N, Ri, S).

% ···················· tests divide_sub ···································
:- test divide_sub(L, N, Res, S)
   : (L = [], Res = []) + not_fails # "Caso base.".

:- test divide_sub(L, N, Res, S)
   : (L = [s(s(0)), s(s(0)), s(s(0)), s(s(0)), s(s(0)), s(s(0)), s(s(0)), s(s(0))],
      N = s(s(0)))
   => (Res = [[s(s(0)),s(s(0))],[s(s(0)),s(s(0))],
              [s(s(0)),s(s(0))],[s(s(0)),s(s(0))]],
       S = s(s(s(s(0))))) + not_fails # "Caso 1.".

:- test divide_sub(L, N, Res, S)
   : (L = [s(0), s(0), s(0), s(0), s(0), s(0)], N = s(s(0)))
   => (Res = [[s(0),s(0)],[s(0),s(0)],[s(0),s(0)]], S = s(s(0))) # "Caso 2.".

:- test divide_sub(L, N, Res, S)
   : (L = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0], N = s(s(s(s(s(0))))))
   => (Res = [[0,0,0,0,0],[0,0,0,0,0]], S = 0) # "Caso 3.".

% @@@@@@@@@@@@@@@@@@@@ square_lists @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred square_lists(N, SQ, S) :: (nat(N), is_list(SQ), nat(S))
   # "@var{SQ} es una matriz de @var{N} x @var{N} cuyos elementos son los
      naturales desde @var{N}² hasta 1. Además todas las filas de la matriz
      suman igual (@var{S}).".
square_lists(0, [], 0).           % caso base
square_lists(N, SQ, S) :-
    times(N, N, Nsq),             % calcular N^2
    nums(Nsq, L_aux),             % generar la lista de elementos de N² hasta 1
    perm(L_aux, L_perm),          % permutar las litas
    divide_sub(L_perm, N, SQ, S). % dividir las listas y comprobar la suma

% ···················· tests square_lists ·································
:- test square_lists(N, SQ, S)
   : (N = s(0)) +
   (try_sols(1),
    solutions([square_lists(N, [[s(0)]], s(0))])) # "Caso 1.".
:- test square_lists(N, SQ, S)
   : (N = s(s(0))) +
   (try_sols(8),
    solutions([
        square_lists(N, [[s(0), s(s(s(s(0))))], [s(s(0)), s(s(s(0)))]], s(s(s(s(s(0)))))),
        square_lists(N, [[s(0), s(s(s(s(0))))], [s(s(s(0))), s(s(0))]], s(s(s(s(s(0)))))),
        square_lists(N, [[s(s(0)), s(s(s(0)))], [s(0), s(s(s(s(0))))]], s(s(s(s(s(0)))))),
        square_lists(N, [[s(s(0)), s(s(s(0)))], [s(s(s(s(0)))), s(0)]], s(s(s(s(s(0)))))),
        square_lists(N, [[s(s(s(0))), s(s(0))], [s(0), s(s(s(s(0))))]], s(s(s(s(s(0)))))),
        square_lists(N, [[s(s(s(0))), s(s(0))], [s(s(s(s(0)))), s(0)]], s(s(s(s(s(0)))))),
        square_lists(N, [[s(s(s(s(0)))), s(0)], [s(s(0)), s(s(s(0)))]], s(s(s(s(s(0)))))),
        square_lists(N, [[s(s(s(s(0)))), s(0)], [s(s(s(0))), s(s(0))]], s(s(s(s(s(0))))))
        ])) # "Caso 2.".