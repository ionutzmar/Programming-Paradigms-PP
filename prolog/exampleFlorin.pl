% in Prolog, avem predicate, adica niste statement-ul, propozitii sau fapte
% in fisierul Prolog, predicatele care se afla aici sunt adevarate, iar cele care NU se afla sunt automat false

areMere(anca). % A NU SE UITA DE . (PUNCT) IN PROLOG, PENTRU A MARCA INCHEIEREA UNUI STATEMENT / PREDICAT
% valorile / constantele incep cu litera mica
% variabilele incep cu litera mare
eBaiatMisto(valentin). % Valentin este un baiat misto


% structuri - avem fapte ca variabile, care se comporta aici ca simple variabile


arePapuciGucci(abi).
bateRoacheri(abi).
eRegePaRomania(abi).
areMultiBani(abi, bani(abi, 100000)). % bani(X, Y) reprezinta o structura care se comporta pur si simplu ca un parametru


% regula - un set de fapte
% abi are talent daca el are papuci Gucci, bate roacheri si e rege pa Romania
areTalent(Y) :- arePapuciGucci(Y), bateRoacheri(Y), eRegePaRomania(Y).

% faceti in terminalul de swipl urmatoarele interogari: areTalent(X)., arePapuciGucci(X).

% backtrackingul sta la baza interogarilor in swipl de fapte
% operatorii =:= si is forteaza evaluarea expresiilor, pe cand = verifica egalitatea structurala

% faceti in terminalul swipl urmatoarele interogari:
% 
%
%
%

% liste in Prolog
% [] - lista vida
% [a, b, c] - lista cu elementele a, b, c
% [Head | Tail] - lista cu primul element Head si cu restul elementelor in Tail
% [A, B, C, D | Tail] - lista formata prin adaugarea A, B, C, D in fata listei Tail

% argumentele predicatelor
% predicat (+Arg1, -Arg2, ?Arg3).
% + = parametru de intrare, instantiat cand se incearca satisfacerea predicatului
% - = parametru de iesire, instantiat cand este satisfacut predicatul
% ? = poate fi parametru de intrare sau de iesire

% in Prolog avem un element important din Haskell: pattern matching
% de ce? Pentru ca in Prolog nu avem conditionala if ... else if, iar pattern matching inlocuieste conditionala if
% care este necesara in cazul de stop al recursivitatii, care e de asemenea prezenta in Prolog

%list_length(+Lista, -Lungime)
list_length([], 0). % mai intai facem cazul de baza pentru backtracking / recursivitate
list_length([_ | Tail], N) :- list_length(Tail, N1), N is N1 + 1. % continuam cautarea
% de ce nu punem Head in loc de _? vom avea warning cu Singleton variables, pt ca Head nu este folosit aici => folosim _, la caz general

is_member(X, [X | _]). % caz de baza
is_member(X, [_ | Tail]) :- is_member(X, Tail). % continuam cautarea

same_list([], []) :- !.
same_list([Head | Rest1], [Head | Rest2]) :- same_list(Rest1, Rest2).
same_list([Head1 | _], [Head2 | _]) :- Head1 \= Head2, fail.

% findall functioneaza ca un filter
% P reprezinta elementul rezultat din filtrare, iar L reprezinta lista filtrata care il contine pe elementul P
% la findall avem, de regula, un member ca sa stim de unde luam elementele (adica sa stim ce lista filtram noi), apoi
% urmeaza conditiile de filtrare
check_even(X, L) :- findall(P, (member(P, X), P mod 2 =:= 0), L).
% in Haskell asta s-ar traduce asa
% filter (\x -> x `mod` 2 == 0) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
% sau (mai degraba asa, prin list comprehensions)
% [x | x <- lst, x `mod` 2 == 0] - vedeti asemanarea? findall mai degraba functioneaza ca un list comprehension, fix in stilul asta

% in Prolog putem sa avem overload la functii (predicate mai bine zis)
arataBine(andreea).
arataBine(anca).
arataBine(irina).
three_some(X, Y, Z) :- arataBine(X), arataBine(Y), arataBine(Z).
two_some(X, Y) :- arataBine(X), arataBine(Y), X \= Y.
three_some(X, Y) :- three_some(X, Y, andreea), X \= Y, !.

