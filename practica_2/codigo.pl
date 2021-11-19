%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Apellidos:     Cerezo Pomykol
% Nombre:        Jan
% Nº Matrícula:  a180305
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(_,_,[assertions, classic, regtypes]).
:- use_package(nativeprops).
:- use_module(library(unittest/unittest_props)).
:- doc(title, "Documentación de la segunda práctica").
:- doc(author, "Cerezo Pomykol, Jan (a180305)").

:- pred alumno_prode(A, B, C, D)
   # "Cerezo Pomykol, Jan, a180305".
alumno_prode('Cerezo', 'Pomykol', 'Jan', 'a180305').

% @@@@@@@@@@@@@@@@@@@@ is_list @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- prop is_list(L) :: (is_list(L))
   # "@var{L} es una lista.".
is_list([]).
is_list([_|Li]) :-
    is_list(Li).

% ···················· tests is_list ······································
:- test is_list(L)
   : (L = []) + not_fails # "Caso 1 (lista vacía).".

:- test is_list(L)
   : (L = [a, b]) + not_fails # "Caso 2 (lista correcta).".

:- test is_list(L)
   : (L = 0) + fails # "Caso 3 (lista incorrecta).".

% @@@@@@@@@@@@@@@@@@@@ len @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- prop len(L, N) :: (is_list(L))
   # "La lista @var{L} tiene longitud @var{N}.".
len([], 0).
len([_|L], N) :-
    len(L, M),
    N is M + 1.

% ···················· tests len ··········································
:- test len(L, N)
   : (L = [])
   => (N = 0) + not_fails # "Caso 1 (longitud de una lista vacía).".

:- test len(L, N)
   : (L = [a, b, c])
   => (N = 3) + not_fails # "Caso 2 (longitud de una lista no vacía).".

% @@@@@@@@@@@@@@@@@@@@ concat @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred concat(L1, L2, L3) :: (is_list(L1), is_list(L2), is_list(L3))
   # "@var{L3} es la lista resultante de concatenar @var{L1} y @var{L2}.".
concat([],Ys,Ys).
concat([X|Xs],Ys,[X|Zs]) :-
    concat(Xs,Ys,Zs).

% ···················· tests concat ·······································
:- test concat(L1, L2, L3)
   : (L1 = [], L2 = [a, b])
   => (L3 = [a, b]) + not_fails # "Caso 1 (una lista tiene longitud 0).".

:- test concat(L1, L2, L3)
   : (L1 = [a], L2 = [])
   => (L3 = [a]) + not_fails # "Caso 2 (una lista tiene longitud 0).".

:- test concat(L1, L2, L3)
   : (L1 = [], L2 = [])
   => (L3 = []) + not_fails # "Caso 3 (las dos listas tienen longitud 0).".

:- test concat(L1, L2, L3)
   : (L1 = [a, b, c], L2 = [d, e])
   => (L3 = [a, b, c, d, e]) + not_fails # "Caso 4 (concatenar dos listas de longitud mayor que 0).".

:- test concat(L1, L2, L3)
   : (L2 = [d, e], L3 = [a, b, c, d, e])
   => (L1 = [a, b, c]) + not_fails # "Caso 5 (restar dos listas).".

:- test concat(L1, L2, L3)
   : (L1 = [a, b, c], L3 = [a, b, c, d, e])
   => (L2 = [d, e]) + not_fails # "Caso 6 (restar dos listas).".


% @@@@@@@@@@@@@@@@@@@@ partir @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred partir(L, P1, P2) :: (is_list(L), is_list(P1), is_list(P2))
   # "@var{P1} y @var{P2} son dos listas no vacías que concatenadas forman @var{L}.".
partir(T, P1, P2) :-
    concat(P1, P2, T),
    len(P1, N1),
    N1 > 0,
    len(P2, N2),
    N2 > 0.

% ···················· tests partir ·······································
:- test partir(L, A, B)
   : (A = [a, b], B = [c, d])
   => (L = [a, b, c, d]) + not_fails # "Caso 1 (concatenar listas).".

:- test partir(L, A, B)
   : (A = [], B = [a]) + fails # "Caso 2 (listas de longitud 0).".

:- test partir(L, A, B)
   : (L = [a, b, c]) +
   ( try_sols(10),
     solutions([partir(L, [a], [b, c]), partir(L, [a, b], [c])])) # "Caso 3 (obtener partes a partir de la lista).".

:- test partir(L, A, B)
   : (L = [a, b, c, d, e]) +
   ( try_sols(10),
     solutions([partir(L, [a], [b, c, d, e]), partir(L, [a, b], [c, d, e]),
                partir(L, [a, b, c], [d, e]), partir(L, [a, b, c, d], [e])]))
   # "Caso 4 (obtener partes a partir de la lista).".

% @@@@@@@@@@@@@@@@@@@@ parentesis @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred parentesis(P, N, R) :: (is_list(P), integer(N), is_list(R))
   # "@var{R} es la lista '(@var{P})@var{N}' si la longitud de @var{P} >= 2, '@var{P}@var{N}' en caso contrario.".
parentesis([P1,P2|P], N, R) :-
    integer(N),
    concat(['(', P1, P2|P], [')', N], R).
parentesis([P1], N, R) :-
    integer(N),
    concat([P1], [N], R).

% ···················· tests parentesis ···································
:- test parentesis(P, N, R)
   : (P = [a, b, c], N = 5)
   => (R = ['(', a, b, c, ')', 5]) + not_fails # "Caso 1 (normal con paréntesis).".

:- test parentesis(P, N, R)
   : (P = [a], N = 1)
   => (R = [a, 1]) + not_fails # "Caso 2 (normal sin paréntesis).".

:- test parentesis(P, N, R)
   : (N = 2, R = ['(', a, b, c, ')', 2])
   => (P = [a, b, c]) + not_fails # "Caso 3 (reverso con paréntesis).".

:- test parentesis(P, N, R)
   : (N = 2, R = [a, 2])
   => (P = [a]) + not_fails # "Caso 4 (reverso sin paréntesis).".

:- test parentesis(P, N, R)
   : (N = 3, R = ['(', a, b, ')', 4]) + fails # "Caso 5 (reverso con paréntesis, N incorrecto).".

:- test parentesis(P, N, R)
   : (N = 3, R = [a, 4]) + fails # "Caso 6 (reverso sin paréntesis, N incorrecto).".

:- test parentesis(P, N, R)
   : (N = 3, R = ['(', a, ')', 3]) + fails # "Caso 7 (reverso, R incorrecto).".

% @@@@@@@@@@@@@@@@@@@@ se_repite @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred se_repite(Cs, Parte, Num0, Num) :: (is_list(Cs), is_list(Parte), integer(Num0), integer(Num))
   # "@var{Cs} se compone repitiendo @var{Num}-@var{Num0} veces @var{Parte}.".
se_repite([], _, Num, Num).
se_repite(L, L, Num0, Num) :-
    Num is Num0 + 1.
se_repite(Cs, Parte, Num0, Num) :-
    partir(Cs, Parte, Resto),
    X is Num0 + 1,
    se_repite(Resto, Parte, X, Num).

% ···················· tests se_repite ····································
:- test se_repite(Cs, Parte, Num0, Num)
   : (Cs = [a, b, c], Parte = [a, b, c], Num0 = 0)
   => (Num = 1) + not_fails # "Caso 1 (lista igual que la parte).".

:- test se_repite(Cs, Parte, Num0, Num)
   : (Cs = [a, b, a, b, a, b, a, b], Parte = [a, b], Num0 = 2)
   => (Num = 6) + not_fails # "Caso 2 (repetición de una secuencia de caracteres).".

:- test se_repite(Cs, Parte, Num0, Num)
   : (Cs = [a, b, c, d], Parte = [a], Num0 = 0) + fails # "Caso 3 (número de repeticiones incorrecto).".

:- test se_repite(Cs, Parte, Num0, Num)
   : (Cs = [], Parte = [a, b], Num0 = 0)
   => (Num = 0) + not_fails # "Caso 4 (lista vacía, no se repite ninguna secuencia).".

% @@@@@@@@@@@@@@@@@@@@ repeticion @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred repeticion(L, R) :: (is_list(L), is_list(R))
   # "@var{R} es el resultado de comprimir la lista @var{L} por repetición.".
repeticion(L, R) :-
    partir(L, A, _),
    se_repite(L, A, 0, N),
    parentesis(A, N, R).

% ···················· tests repeticion ···································
:- test repeticion(L, R)
   : (L = [a, b, a, b, a, b, a, b]) +
   ( try_sols(10),
     solutions([repeticion(L, ['(', a, b, ')', 4]), repeticion(L, ['(', a, b, a, b, ')', 2])])) # "Caso 1 (repetición de dos caracteres distintos).".

:- test repeticion(L, R)
   : (L = [a, a, a, a]) +
   ( try_sols(10),
     solutions([repeticion(L, [a, 4]), repeticion(L, ['(', a, a, ')', 2])]))
   # "Caso 2 (repetición del mismo carácter).".

:- test repeticion(L, R)
   : (L = [a, b, c, a, b, c, a, b, c, a, b, c, a, b, c])
   => (R = ['(', a, b, c, ')', 5]) + not_fails # "Caso 3 (repetición de una secuencia de caracteres).".

:- test repeticion(L, R)
   : (L = []) + fails # "Caso 4 (lista vacía).".

:- test repeticion(L, R)
   : (L = [a, a, a, b]) + fails # "Caso 5 (no se encuentra una secuencia que se repita).".

% @@@@@@@@@@@@@@@@@@@@ division @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred division(I, C) :: (is_list(I), is_list(C))
   # "@var{C} es una lista resutante de aplicar compresión por división y repetición a la lista @var{I}.".
division(I, C) :-
    partir(I, A, B),
    compresion_recursiva(A, C1),
    compresion_recursiva(B, C2),
    concat(C1, C2, C).

% @@@@@@@@@@@@@@@@@@@@ menor_lon @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred menor_lon(L, Min) :: (is_list(L), is_list(Min))
   # "@var{L} es un conjunto de listas, @var{Min} es la sublista de menor longitud.".
menor_lon([R], R).
menor_lon([L1, L2|Li], R) :-
    len(L1, Aux1),
    len(L2, Aux2),
    Aux1 < Aux2,
    menor_lon([L1|Li], R).

menor_lon([L1, L2|Li], R) :-
    len(L1, Aux1),
    len(L2, Aux2),
    Aux1 >= Aux2,
    menor_lon([L2|Li], R).

% ···················· tests menor_lon ····································
:- test menor_lon(L, R)
   : (L = [[a, b, c], [a, b], [a, b, c, d]])
   => (R = [a, b]) + not_fails # "Caso 1 (sublistas de distinta longitud).".

:- test menor_lon(L, R)
   : (L = [[a, b], [a, b], [a, b]])
   => (R = [a, b]) + not_fails # "Caso 2 (sublistas de la misma longitud).".


% @@@@@@@@@@@@@@@@@@@@ compresion_recursiva @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred compresion_recursiva(I, C) :: (is_list(I), is_list(C))
   # "@var{C} es el resultado óptimo de comprimir la lista @var{I} por repetición y división.".
compresion_recursiva(I, C) :-
    mejor_compresion_memo(I, C), !.
compresion_recursiva(I, I).

% ···················· tests compresion_recursiva ·························
:- test compresion_recursiva(I, C)
   : (I = [a, b, a, b, a, b])
   => (C = ['(', a, b, ')', 3]) + not_fails # "Caso 1 (compresión sólo por repetición).".

:- test compresion_recursiva(I, C)
   : (I = [a, a, a, a, a])
   => (C = [a, 5]) + not_fails # "Caso 2 (compresión sólo por repetición).".

:- test compresion_recursiva(I, C)
   : (I = [a, b, c, d])
   => (C = [a, b, c, d]) + not_fails # "Caso 3 (no se puede comprimir la lista).".

:- test compresion_recursiva(I, C)
   : (I = [a, b, a, b, a, b, a, a, a])
   => (C = ['(', a, b, ')', 3, a, 3]) + not_fails # "Caso 4 (compresión con repetición y división).".

:- test compresion_recursiva(I, C)
   : (I = [d, d, d, a, b, c, a, b, c, a, b, c, a, a, a, a, a, e, f, e, g, e, g, e, g])
   => (C = [d, 3, '(', a, b, c, ')', 3, a, 5, e, f, '(', e, g, ')', 3]) + not_fails # "Caso 5 (compresión con repetición y división).".

% @@@@@@@@@@@@@@@@@@@@ compresion @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred compresion(I, C) :: (is_list(I), is_list(C))
   # "@var{C} es el resultado de comprimir la lista @var{I} por repetición y división.".
compresion(I, C) :-
    repeticion(I, C);
        division(I, C).

% @@@@@@@@@@@@@@@@@@@@ comprimir @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred comprimir(I, C) :: (is_list(I), is_list(C))
   # "@var{C} es el resultado de comprimir la lista @var{I} por repetición y división.".
comprimir(I, C) :-
    limpia_memo,
    compresion_recursiva(I, C).

% @@@@@@@@@@@@@@@@@@@@ limpia_memo @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred limpia_memo
   # "Limpia la base de hechos.".
limpia_memo :-
    retractall(memo(_, _)).

% @@@@@@@@@@@@@@@@@@@@ mejor_compresion @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred mejor_compresion(I, C) :: (is_list(I), is_list(C))
   # "@var{C} es el resultado óptimo de comprimir la lista @var{I} por repetición y división. Se generan todas las posibilidades y se selecciona la mejor.".
mejor_compresion(I, C) :-
    findall(Caux, compresion(I, Caux), Aux),
    menor_lon(Aux, C).

% @@@@@@@@@@@@@@@@@@@@ mejor_compresion_memo @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
:- pred mejor_compresion_memo(I, C) :: (is_list(I), is_list(C))
   # "@var{C} es el resultado óptimo de comprimir la lista @var{I} por repetición y división. Se emplea la técnica de memorización".
mejor_compresion_memo(I, C) :-
    memo(I, C), !.
mejor_compresion_memo(I, C) :-
    mejor_compresion(I, C),
    assert(memo(I, C)).

:- dynamic memo/2.
