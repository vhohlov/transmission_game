
:- ensure_loaded('t3t.pl').


% parse(+Raw, -Problem)
parse(R, P):- parseNodes(R, P1), getColliniar(R, L), setCollAll(L, P1, P).

%parseNodes(+Raw, -Problem)
parseNodes([],[]).
parseNodes([H|T], P):- parseType(H, R1), parseNodes(T, R2), myConcat(R1, R2, P), !.

%parseType(+L, -R)
%L - lista cu surse in mod Raw
%R - lista de surse prelucrate
%sourse(NodS, Out, Collinear)
parseType(sources(L), R):-getSource(L, R).  

%L - lista cu receptoare in mod Raw
%R - lista de receptoare prelucrate
%receiv(NodR, In, Collinear)
parseType(receivers(L), R):- getReceiver(L, R).

%L - lista cu transmitatoare in mod Raw
%R - lista cu transmitatoare prelucrate
%transmit(NodR, In, Out, Collinear).
parseType(transceivers(L), R):- getTransceivers(L, R).

%ignora aceste date
parseType(collinear(_),[]).
parseType(star_transceivers(_),[]).
parseType(source_transceivers(_),[]).

%getSource(+L, -R).
%L - lista de surse
%R - lista de surse in format source(NodS, Out, Coliniari)
getSource([], []).
getSource([H|[]], [source(H, 1, [])]). 
getSource([H,H1|T], [R|P]):- integer(H1), R = source(H, H1, []), getSource(T, P).
getSource([H,H1|T], [R|P]):- \+integer(H1), R = source(H, 1, []), getSource([H1|T],P).

%getReceiver(+L, -R).
%L - lista de receptori
%R - lista de receptori in format receiv(NodR, In, Coliniari)
getReceiver([], []).
getReceiver([H|[]], [receiv(H, 1, [])]). 
getReceiver([H,H1|T], [R|P]):- integer(H1), R = receiv(H, H1, []), getReceiver(T, P).
getReceiver([H,H1|T], [R|P]):- \+integer(H1), R = receiv(H, 1, []), getReceiver([H1|T],P).

%getTranceivers(+L, -R).
%L - lista de transmitatori
%R - lista de surse in format trans(NodT, In, Out, Coliniari)
getTransceivers([], []).
getTransceivers([H|[]], [trans(H, 1, 1, [])]). 
getTransceivers([H,H1|T], [R|P]):- integer(H1), R = trans(H, H1, H1, []), getTransceivers(T, P).
getTransceivers([H,H1|T], [R|P]):- \+integer(H1), R = trans(H, 1, 1, []), getTransceivers([H1|T], P).


%getColliniar(+Raw, -L).
%returneaza Lista cu Coliniari
getColliniar([],[]):-!.
getColliniar([collinear(L)|_], L):-!.
getColliniar([_|T], L):- getColliniar(T, L).

%setCollAll(+L,+Problem, -Prob1)
%L - lista de liste de coliniari
%Problem - Nodurile nivelului fara liste de adiacenta
%Returneaza Lista de Noduri cu lista de coliniari setat
setCollAll([], P1, P1).
setCollAll([Hc|Tc], P1, P):-setColliniar(Hc, P1, P2), setCollAll(Tc, P2, P).


%setColliniar(+L, +Problem, -Prob1)
%L - lista de coliniari
%Problem - lista de noduri cu liste de coliniari nefinale
%Prob1 - lista de noduri cu L adaugat la noduri corespunzator
setColliniar(_,[],[]).
setColliniar(Hc, [Hp|Tp],[source(Nods, In, CC)|P]):- Hp = source(Nods, In, Cs),
														  member(Nods, Hc),
														  myConcat([Hc], Cs, CC),!,
														  setColliniar(Hc, Tp, P).

setColliniar(Hc, [Hp|Tp],[receiv(Nodr, Out, CC)|P]):- Hp = receiv(Nodr, Out, Cr),
														  member(Nodr, Hc),
														  myConcat([Hc], Cr, CC),!,
														  setColliniar(Hc, Tp, P).

setColliniar(Hc, [Hp|Tp],[trans(Nodt, In, Out, CC)|P]):- Hp = trans(Nodt, In, Out, Ct),
														      member(Nodt, Hc),
														      myConcat([Hc], Ct, CC),!,
														      setColliniar(Hc, Tp, P).

setColliniar(Hc, [Hp|Tp],[Hp|P]):-setColliniar(Hc, Tp, P).



% myConcat/3
% myConcat(+List1, +List2, -RList)
myConcat([], M, M).
myConcat([H|T], M, [H|R]):- myConcat(T, M, R).


% describe(+Problem)
describe(P):- writeln('Sursa  Latime  Coliniari'), descSource(P),
			  writeln('Receptor  Latime  Coliniari'), descReceive(P),
			  writeln('Transmitator Latime_In Latime_Out Coliniari'),descTransceiver(P).	

%descSource(+L)
%L - lista de surse
descSource([]):- writeln('').
descSource([source(NodS, Out, Col)|T]):- writef('%3R', [NodS]),
										 writef('\t%3R', [Out]),
										 format("\t~w~n", [Col]),
										 descSource(T), !.
descSource([_|T]):- descSource(T), !.

%descReceive(+L)
%L - lista de receptori
descReceive([]):-writeln('').
descReceive([receiv(NodR, Out, Col)|T]):- writef('%5R', [NodR]),
										  writef('\t%6R', [Out]),
										  format("  \t~w~n", [Col]),
										  descReceive(T), !.
descReceive([_|T]):- descReceive(T), !.

%%descTranceiver(+L)
%L - lista de transmitatori.
descTransceiver([]):-writeln('').
descTransceiver([trans(NodT, In ,Out, Col)|T]):- writef('%5R', [NodT]),
										     	 writef('\t%10R', [In]),
										    	 writef('\t%4R', [Out]),
										    	 format(" \t~w~n", [Col]),
										     	 descTransceiver(T), !.
descTransceiver([_|T]):- descTransceiver(T), !.


%solve(+P, -S).
solve(P, []):-getGenerator(P,G), G = 0, testSol(P),!.
solve(P, S):-getGenerator(P,G), genState(G, P, P1,S1), solve(P1, S2), myConcat(S1, S2, S).

%testSol(+P).
%testeaza corectitudinea solutiei
testSol([]).
testSol([source(_,0,_)|T]):- testSol(T),!.
testSol([receiv(_,0,_)|T]):- testSol(T),!.
testSol([trans(_,0,0,_)|T]):- testSol(T),!.
 
%getGenerator(+P,-G).
%P - lista de noduri si proprietati
%G - Nodul de la care se genereaza solutii 
%Afla care e urmatorul element ce ar trebui sa genereze o stare
%DC nu mai sunt elemente ce genereaza legaturi, return 0
getGenerator([source(_, 0, _)|T], G):- getGenerator(T, G),!.
getGenerator([trans(_,_, 0, _)|T], G):- getGenerator(T, G),!.
getGenerator([receiv(_,_,_)|T], G):- getGenerator(T, G), !.
getGenerator([G|_], G):-!.
getGenerator([], 0).


%genState(A,B, P, S).
%A - nod din care se genereaza solutia
%B - nod ce completeaza generarea lui A 
%P - Problema dupa prelucrarea curent
%S - Solutia intermediara/finala la momentul curent

%Caz cand generatorul sursa e ultimul element din lista
genState(source(NodS, 0, Cs), [source(NodS,_,_)|[]], [source(NodS, 0, Cs)], []):-!.

%Daca generatorul sursa are latime 0 se intoarce Solutia
%Lista Noua e egala cu lista veche ce a ramas neprelucrata
genState(source(_, 0, _), P1, P1, []):-!.

%Daca Out > 0, si T == [], solutia nu e valida 
genState(source(_, Out, _), [], [], []):-Out > 0, fail.

%Generator sursa si Nodul e acelasi
%Intoarce S, si Lista Noua concatenata cu generatorul cu latime 0
genState(source(NodS, Out, Cs), [source(NodS, _, _)|T], [source(NodS, 0, Cs)|P1], S):- 
																			     !,genState(source(NodS, Out, Cs), T, P1, S).

%Generator sursa si Nodul sunt surse
%Intoarce S si Lista cu Nodul nemodificat
%Continua cu acelasi Generator si Lista cu urmatorul Nod
genState(source(NodS, Out, Cs), [source(NodS1, Out1, Cs1)|T], [source(NodS1, Out1, Cs1)|P1], S):- 
																							\+NodS = NodS1, 
																							!,genState(source(NodS, Out, Cs), T, P1, S).

%Generator sursa -> Receptor
%Genereaza un segment de solutie
%Avanseaza cu Tail de Noduri si Generator
genState(source(NodS, Out, Cs), [receiv(NodR, In, Cr)|T], P1, [[NodS, NodR, R]|S]):- 
																\+isColliniar(Cs,NodS,NodR),
																getMinim(Out, In, R), R > 0,
																Out1 is Out - R, In1 is In - R,
																genState(source(NodS, Out1, Cs), T, P, S), 
																myConcat([receiv(NodR, In1, Cr)], P, P1).															
%Generator sursa -> Transmitator
%Genereaza un segment de solutie
%avanseaza cu Tail de Noduri si Generator
genState(source(NodS, Out, Cs),[trans(NodT, InT, OutT, Ct)|T], P1, [[NodS, NodT, R]|S]):-
																\+isColliniar(Cs,NodS,NodT),
																getMinim(Out, InT, R), R > 0,
																Out1 is Out - R, In is InT - R,
																genState(source(NodS, Out1, Cs), T, P, S), 
																myConcat([trans(NodT, In, OutT,Ct)], P, P1).
															

%Avanseaza cu urmatorul nod, trecand peste nodul curent
%Se aplica pentru noduri Receptoare si Transmitatoare
genState(source(NodS, Out, Cs), [H,N|T], [H|P], S):- 
												genState(source(NodS, Out, Cs), [N|T], P, S).
														


%Caz cand generatorul transmitator e ultimul element din lista
genState(trans(NodT, In, 0, Ct), [trans(NodT,_,_,_)], [trans(NodT, In, 0, Ct)], []):-!.

%Daca generatorul are latime 0 se intoarce Solutia
%Lista Noua e egala cu lista veche ce a ramas neprelucrata
genState(trans(_, _, 0,_), P1, P1, []):-!.

%Daca Out > 0, si T == [], solutia nu e valida 
genState(trans(_, _,Out, _), [], [], []):-Out > 0, fail.

%Generator Transmitator si Nodul sunt surse
%Intoarce S si Lista cu Nodul nemodificat
%Continua cu acelasi Generator si Lista cu urmatorul Nod
genState(trans(NodT, In, Out, Ct), [source(NodS, OutS, Cs)|T], [source(NodS, OutS, Cs)|P1], S):-  
																							!,genState(trans(NodT, In, Out, Ct), T, P1, S).

%Generator transmitator si Nodul e acelasi 
%Intoarce S, si Lista Noua concatenata cu generatorul cu latime 0
genState(trans(NodT, In, Out, Ct), [trans(NodT,_ ,_ , _)|T], [trans(NodT, In, 0, Ct)|P1], S):- 
																			    !, genState(trans(NodT, In, Out, Ct), T, P1, S).


%Generator transmitator si Nodul e transmitator
%Intoarce S si Lista cu Nodul nemodificat
%Continua cu acelasi Generator si Lista cu urmatorul Nod
genState(trans(NodT,In, Out, Ct), [trans(NodT1, In1, Out1, Ct1)|T], P1, [[NodT, NodT1, R]|S]):-
																		\+isColliniar(Ct,NodT,NodT1), 
																		getMinim(Out, In1, R), R > 0,
																		OutP is Out - R, In1P is In1 - R,
																		genState(trans(NodT, In, OutP, Ct), T, P, S), 
																		myConcat([trans(NodT1, In1P, Out1,Ct1)], P, P1).
%Generator transmitator -> Receptor
%Genereaza un segment de solutie
%avanseaza cu Tail de Noduri si Generator
genState(trans(NodT, In, Out, Ct), [receiv(NodR, InR, Cr)|T], P1, [[NodT, NodR, R]|S]):- 
																\+isColliniar(Ct,NodT,NodR),
																getMinim(Out, InR, R), R > 0,
																Out1 is Out - R, InR1 is InR - R,
																genState(trans(NodT, In, Out1, Ct), T, P, S), 
																myConcat([receiv(NodR, InR1, Cr)], P, P1).

%Pt Receptor(Pt Noduri fara !)
%Avanseaza cu urmatorul nod, pasiduse pe sine
genState(trans(NodT, In,Out, Ct), [H,N|T], [H|P], S):- 
												genState(trans(NodT, In,Out, Ct), [N|T], P, S).

%Verifica daca e coliniar pe toate listele de coliniaritate
%isColliniar(+L, +A, +B)
%L - Lista de liste de coliniaritate
% A, B - noduri care se verifica daca sunt coliniare
isColliniar([], _, _):-!,fail.
isColliniar([H|T], A, B):- \+member(B, H),!,isColliniar(T,A,B).
isColliniar([H|_], A, B):- checkline(H,A,B).

%Verifica daca e coliniar pe o lista de coliniaritate
%checkline(+L, +A, +B)
%L - Lista de noduri coliniare
% A, B - noduri care se verifica daca sunt coliniare
checkline([],_,_):-!.
checkline([_|[]],_,_):-!.
checkline([M,N|_], A, B):- A = M, B = N, !,fail.
checkline([M,N|_], A, B):- A = N, B = M, !,fail.
checkline([_,N|T], A, B):- checkline([N|T], A, B),!.

%getMinim(+A, +B, -R)
%intoarce minimul a 2 numere
getMinim(A, B, B):- A > B,!.
getMinim(A, B, A):- B > A,!.
getMinim(A, B, A):- A == B,!.

