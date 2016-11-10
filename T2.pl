
dynamic ouro.
dynamic powerup.

dynamic buraco.
dynamic inimigod.
dynamic inimigoD.
dynamic teleport.

dynamic notp.

dynamic possivelp.

dynamic dependente.
dynamic perigo.
dynamic desconhecido.
dynamic visitado.

setlim(Nome,Val):-nb_setval(Nome,Val).

init():-
	setlim(lim_Y,3),
	setlim(lim_X,4),
	set(init,1,1,_),
	set(-1,-1,_),
	retractall(dependente(_,_,_,_,_)),
	retractall(possivelp(_,_,_)).

adj(X,Y,norte,X,Y2):- nb_getval(lim_Y,LIM), Y<LIM, Y2 is Y+1.
adj(X,Y,sul,X,Y2):- Y>1, Y2 is Y-1.
adj(X,Y,leste,X2,Y):- nb_getval(lim_X,LIM), X<LIM, X2 is X+1.
adj(X,Y,oeste,X2,Y):- X>1, X2 is X-1.


set(mapa,X,Y,buraco):-
	assert(buraco(mapa,X,Y)).
set(mapamental,X,Y,buraco):-
	not(buraco(mapamental,X,Y)),
	assert(buraco(mapamental,X,Y)),
	set(inimigo,X,Y,notp),
	set(teleport,X,Y,notp),
	retractall(dependente(_,X,Y,_,_)),
	retractall(dependente(_,_,_,X,Y)).

set(Nome,X,Y,inimigod):-assert(inimigod(Nome,X,Y)).
set(Nome,X,Y,inimigoD):-assert(inimigoD(Nome,X,Y)).
set(Nome,X,Y,teleport):-assert(teleport(Nome,X,Y)).
set(Nome,X,Y,powerup):-assert(powerup(Nome,X,Y)).
set(Nome,X,Y,ouro):-assert(ouro(Nome,X,Y)).

set(Nome,X,Y,possivelp):-assert(possivelp(Nome,X,Y)).

set(Nome,X,Y,notp):-assert(notp(Nome,X,Y)).


set(X,Y,desconhecido):-assert(desconhecido(X,Y)).
set(X,Y,visitado):-assert(visitado(X,Y)).

setdependente(Nome,X,Y,dependente,X2,Y2):-
	assert(dependente(Nome,X,Y,X2,Y2)),
	assert(dependente(Nome,X2,Y2,X,Y)).

reset(Nome,X,Y,buraco):-retract(buraco(Nome,X,Y)).
reset(Nome,X,Y,inimigod):-retract(inimigod(Nome,X,Y)).
reset(Nome,X,Y,inimigoD):-retract(inimigoD(Nome,X,Y)).
reset(Nome,X,Y,inimigo):-retract(inimigo(Nome,X,Y)).
reset(Nome,X,Y,teleport):-retract(teleport(Nome,X,Y)).
reset(Nome,X,Y,ouro):-retract(ouro(Nome,X,Y)).
reset(Nome,X,Y,powerup):-retract(powerup(Nome,X,Y)).

reset(Nome,X,Y,possivelp):-retract(possivelp(Nome,X,Y)).
reset(X,Y,desconhecido):-retract(desconhecido(X,Y)).



perigo(X,Y):- buraco(mapamental,X,Y); inimigo(mapamental,X,Y); teleport(mapamental,X,Y).
safe(X,Y):-not(perigo(X,Y)).

possivelperigo(X,Y):- possivelp(buraco,X,Y); possivelp(inimigo,X,Y); possivelp(teleport,X,Y).


sentirbrisa(Nome,X,Y):-
	adj(X,Y,_,X2,Y2),
	buraco(Nome,X2,Y2),!.

ouvirpassos(Nome,X,Y):-
	adj(X,Y,_,X2,Y2),
	(inimigod(Nome,X2,Y2);inimigoD(X,Y),!).

verflash(Nome,X,Y):-
	adj(X,Y,_,X2,Y2),
	teleport(Nome,X2,Y2),!.



observa(X,Y):- set(X,Y,visitado),
				retractall(dependente(_,X,Y,_,_)),
				retractall(dependente(_,_,_,X,Y)),
				reset(mapamental,X,Y,_),
				set(X,Y,visitado).

%  ATUALIZAR CASA

observa(X,Y):- 
	not(sentirbrisa(mapa,X,Y)),

	adj(X,Y,_,X2,Y2),
	set(buraco,X2,Y2,notp),
	retractall(possivelp(buraco,X2,Y2)),  %%-- marca as adjacencias como nao buraco

	retractall(dependente(buraco,X2,Y2,_,_)), 
	retractall(dependente(buraco,_,_,X2,Y2)).

observa(X,Y):-
	sentirbrisa(mapa,X,Y),
	sentirbrisa(mapamental,X,Y),
	adj(X,Y,_,X2,Y2),
	not(buraco(mapamental,X2,Y2)),
	set(buraco,X2,Y2,notp).

observa(X,Y):-
	sentirbrisa(mapa,X,Y),
	not(sentirbrisa(mapamental,X,Y)),
	adj(X,Y,_,X2,Y2),

	possivelp(buraco,X2,Y2),
	not(dependente(buraco,X2,Y2,_,_)),
	set(mapamental,X2,Y2,buraco).
	

observa(X,Y):-
	sentirbrisa(mapa,X,Y),
	not(sentirbrisa(mapamental,X,Y)),
	adj(X,Y,_,X2,Y2),

	not(notp(buraco,X2,Y2)),

	adj(X,Y,_,X3,Y3),
	(X3\=X2; Y3\=Y2),
	not(notp(buraco,X3,Y3)),
	set(buraco,X2,Y2,possivelp),
	not(dependente(buraco,X2,Y2,X3,Y3)),
	setdependente(buraco,X2,Y2,dependente,X3,Y3).

faspramim():-
		set(mapa,2,3,buraco),
		observa(2,2),
		observa(3,1),
		observa(3,1),
		observa(1,3).

