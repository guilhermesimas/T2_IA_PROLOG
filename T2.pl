
mapaset(Nome,M):- nb_setval(Nome,M).
mapaget(Nome,M):- nb_getval(Nome,M).

setlim(Nome,Val):-nb_setval(Nome,Val).

createmap(Nome):- mapaset(Nome,[['.',t,'.'],['.','.',p],['.',d,'.']]).
createmapmental(Nome):- mapaset(Nome,[[x,x,x],[x,'.',x],[x,x,x]]).

createlistaperigo(Nome):-nb_setval(Nome,[]).
setlistaperigo(Nome,L):-nb_setval(Nome,L).
getlistaperigo(Nome,L):-nb_getval(Nome,L).



%--------------FUNÇOES DE LISTA---------------------%


% replace(Lista,Index,Elem,NovaLista)

replace([_|T], 0, X, L):-L=[X|T],!.
replace([H|T], I, X, [H|R]):-
        I > -1, 
        NI is I-1,
        replace(T, NI, X, R), !.
replace(L, _, _, L).


adiciona(X,L,[X|L]).

adiciona2(X,[],L):-L=[X],!.
adiciona2(X,[H|T],L):-
	adiciona2(X,T,L2),
	L=[H|L2],
	!.


membro( X, [X|_] ).
membro( X, [_|R] ) :- membro( X, R ).

excluiRow([],Elem,[]).
excluiRow([H|T],Elem,L):-
	
	(
		member(Elem,H),
		excluiRow(T,Elem,L),
		!
		)
	;
	(
		excluiRow(T,Elem,L1),
		L=[H|L1]
		)
	.



exclui([],Elem,[]).
exclui([H|T],Elem,L):-

	delete(H,Elem,L2),
	exclui(T,Elem,L3),
	L=[L2|L3].
	%replace(L,N,L4,L2).

pertence(L,Elem):- L=[H|T], ((membro(Elem, H),!) ; pertence(T,Elem)).



%--------------END FUNÇOES DE LISTA---------------------%

%--------------AUXILIARES---------------------------%

% Row = M[N]
row(M, N, Row) :- nth1(N, M, Row).

% C = M[Y][X]
get_char(Y,X,Nome,C):- 

	mapaget(Nome,M), 
	row(M,X,Row),
	nth1(Y,Row,C). 



change(Nome,X,Y,P):- 

	mapaget(Nome,M),
	row(M,Y,Row),
	XN is X-1,
	replace(Row,XN,P,Row2),
	YN is Y-1,
	replace(M,YN,Row2,M2),
	mapaset(Nome,M2). 


changeadj(X,Y,C):-

	(
		adj(X,Y,X2,Y2),
		get_char(X2,Y2,mapamental,C2), 
		C2=x,
		change(mapamental,X2,Y2,C)
	);
	!.

excluiradj(Nome,X,Y):-
	(
		adj(X,Y,Xadj2,Yadj2),
		getlistaperigo(Nome,Lperigo),
		exclui(Lperigo,[Xadj2,Yadj2],Lperigonovo),
		setlistaperigo(Nome,Lperigonovo)
	);
	!.


checaadj(X,Y):-
	(
		adj(X,Y,Xadj,Yadj),
	
		getlistaperigo(listaburaco,Lb),
		not(pertence(Lb,[Xadj,Yadj])),
	
		getlistaperigo(listainimigo,Li),
		not(pertence(Li,[Xadj,Yadj])),
	
		getlistaperigo(listateletransporte,Lt),
		not(pertence(Lt,[Xadj,Yadj])),
	
		get_char(Xadj,Yadj,mapamental,C),
		(C='?';C=x),
		change(mapamental,Xadj,Yadj,np)
	);
	!.

random(X,Y):- 
	X=3,Y=3.



%TESTE

iniciar(Estado):- Estado = estado(1,1,norte,100,50,5,3).

%END TESTE

%-----------------------------------AÇOES---------------------------------%

acao(estado(X,Y,D,S,V,M,O), mover_para_frente, Estado2):- mover(X,Y,D,X2,Y2), S2 is S-1, Estado2 = estado(X2,Y2,D,S2,V,M,O), !.

acao(estado(X,Y,D,S,V,M,O), virar_a_direita, Estado2):- virar(D,D2), S2 is S-1, Estado2 = estado(X,Y,D2,S2,V,M,O) , !.

acao(estado(X,Y,D,S,V,M,O), pegar_objeto, Estado2):- pegar(X,Y,V,O,S,V2,O2,S2), S3 is S2-1, Estado2 = estado(X,Y,D,S3,V2,M,O2) , !.

acao(estado(X,Y,D,S,V,M,O), atirar, Estado2):- atirar(X,Y,D,M,M2), S2 is S-10, Estado2 = estado(X,Y,D,S2,V,M2,O) , !.


mover(X,Y,norte,X,Y2):- nb_getval(lim_Y,LIM), ((Y<LIM, Y2 is Y+1,!) ; (Y2 = Y)).
mover(X,Y,sul,X,Y2):- (Y>1, Y2 is Y-1,!) ; Y2=Y.
mover(X,Y,leste,X2,Y):-nb_getval(lim_X,LIM), ((X<LIM, X2 is X+1,!) ; X2 = X).
mover(X,Y,oeste,X2,Y):- (X>1, X2 is X-1,!) ; X2=X.

virar(norte,leste).
virar(sul,oeste).
virar(leste,sul).
virar(oeste,norte).

pegar(X,Y,V,O,S,V2,O2,S2):- 

	get_char(X,Y,mapa,C),
	((C = o, change(mapa,X,Y,'.'), O2 is O-1, S2 is S+1000,V2=V,!);
	 (C = u, change(mapa,X,Y,'.'), V2 is V+20,(V2>100, V2=100), S2=S, O2=0,!);
	 (V2=V,O2=O,S2=S)).


atirar(X,Y,norte,M,M2):-
	M>0,
	M2 is M-1,
	YN is Y+1,
	get_char(X,YN,mapa,C),
	CN is C - 20,
	change(mapa,X,YN,CN).

atirar(X,Y,sul,M,M2):-
	M>0,
	M2 is M-1,
	YN is Y-1,
	get_char(X,YN,mapa,C),
	CN is C - 20,
	change(mapa,X,YN,CN).

atirar(X,Y,leste,M,M2):-
	M>0,
	M2 is M-1,
	XN is X+1,
	get_char(XN,Y,mapa,C),
	CN is C - 20,
	change(mapa,XN,Y,CN).

atirar(X,Y,oeste,M,M2):-
	M>0,
	M2 is M-1,
	XN is X-1,
	get_char(XN,Y,mapa,C),
	CN is C - 20,
	change(mapa,XN,Y,CN).


%------------------------SUGESTOES---------------------%


sugestao(estado(X,Y,D,_,V,M,O),R):-
	get_char(X,Y,mapamental,C),
	C=i,
	O=0,
	R=sair,
	!.
	
sugestao(estado(X,Y,D,_,V,M,O),R):- 
	get_char(X,Y,mapamental,C),
	C=o,
	R=pegar_objeto,
	!.

sugestao(estado(X,Y,D,_,V,M,O),R):-
	V<50,
	R=pegar_powerup,
	!.

sugestao(estado(X,Y,D,_,V,M,O),R):-
	adj2(X,Y,D,X2,Y2),
	get_char(X2,Y2,mapamental,C),
	C=np,
	R=mover_para_frente,
	!.

sugestao(estado(X,Y,D,_,V,M,O),R):-
	adj2(X,Y,_,X2,Y2),
	get_char(X2,Y2,mapamental,C),
	C=np,
	R=virar_a_direita,
	!.

	sugestao(estado(X,Y,D,_,V,M,O),R):-
	adj2(X,Y,D,X2,Y2),
	get_char(X2,Y2,mapamental,C),
	C='.',
	R=mover_para_frente,
	!.

sugestao(estado(X,Y,D,_,V,M,O),R):-
	adj2(X,Y,_,X2,Y2),
	get_char(X2,Y2,mapamental,C),
	C='.',
	R=virar_a_direita,
	!.

sugestao(estado(X,Y,D,_,V,M,O),R):-
	R=chutar.


adj2(X,Y,norte,X,Y2):- nb_getval(lim_Y,LIM), Y<LIM, Y2 is Y+1.
adj2(X,Y,sul,X,Y2):- Y>1, Y2 is Y-1.
adj2(X,Y,leste,X2,Y):- nb_getval(lim_X,LIM), X<LIM, X2 is X+1.
adj2(X,Y,oeste,X2,Y):- X>1, X2 is X-1.


%---------------------AGIR--------------------%

agir(estado(X,Y,D,S,V,M,O),estado(XN,YN,DN,SN,VN,MN,ON)):-
	atualiza(estado(X,Y,D,S,V,M,O),estado(X2,Y2,D2,S2,V2,M2,O2)),	
	sugestao(estado(X2,Y2,D2,_,V2,M2,O2),R),
	acao(estado(X2,Y2,D2,S2,V2,M2,O2), R, estado(X3,Y3,D3,S3,V3,M3,O3)),
	atualiza(estado(X3,Y3,D3,S3,V3,M3,O3),estado(XN,YN,DN,SN,VN,MN,ON)).	


%----------------------------OBSERVAR----------------------------%

atualiza(estado(X,Y,D,S,V,M,O),estado(X2,Y2,D,S2,V2,M,O)):-

	observalocal(estado(X,Y,D,S,V,M,O),estado(X2,Y2,D,S2,V2,M,O)),
	V2>0, %tratarmorte
	(((X2\=X;Y2\=Y),atualiza(estado(X2,Y2,D,S2,V2,M,O),estado(X3,Y3,D,S3,V3,M,O)),!);(X2=X,Y2=Y)),
	observa(X,Y).

observalocal(estado(X,Y,D,S,V,M,O),estado(X2,Y2,D,S2,V2,M,O)):-

	get_char(X,Y,mapa,C),
	change(mapamental,X,Y,C),
	(
		(C=p, V2=0, S2 is S-1000,X2=X,Y2=Y,!);
		(C=d, V2 is V-20, S2 is S-20, X2=X,Y2=Y,!);
		(C='D' , V2 is V-50, S2 is S-50,X2=X,Y2=Y,!);
		(C=t , random(X2,Y2), S2=S,V2=V, !);
		(X2=X,Y2=Y,S2=S,V2=V)
	).



observa(X,Y):- 
	(
		(
			checabrisas(X,Y)
		)
		,
		(
			checamonstro(X,Y)
		)
		,
		(
			checateletransporte(X,Y)	
		),
		(
			checaadj(X,Y)
		)
	).


checabrisas(X,Y):-
	(
		(
	
				sentirbrisa(X,Y,mapa),
				not(sentirbrisa(X,Y,mapamental)),
	
				Li=[],
	
				((X>1,X2 is X-1,get_char(X2,Y,mapamental,C),(C='?';C=x),adiciona([X2,Y],Li,L),!);L=Li),
				((nb_getval(lim_X,LIMX), X<LIMX, X3 is X+1,get_char(X3,Y,mapamental,C1),(C1='?';C1=x),adiciona([X3,Y],L,L2),!);L2=L),
				((Y>1,Y2 is Y-1,get_char(X,Y2,mapamental,C2),(C2='?';C2=x),adiciona([X,Y2],L2,L3),!);L3=L2),
			 	((nb_getval(lim_Y,LIM), Y<LIM, Y3 is Y+1,get_char(X,Y3,mapamental,C3),(C3='?';C3=x),adiciona([X,Y3],L3,L4),!);L4=L3),
			 	
			 	
				getlistaperigo(listaburaco,L5),

				((
					not(member(L4,L5)),
					adiciona(L4,L5,L6),
					setlistaperigo(listaburaco,L6),
		
					checarestantesburaco(p,listaburaco),
					changeadj(X,Y,'?')
				);
				!)
		)
		;
		(
				not(sentirbrisa(X,Y,mapa)),

				excluiradj(listaburaco,X,Y),

				checarestantesburaco(p,listaburaco)
		
		)
	);
	!.

checateletransporte(X,Y):-
	(
		(
	
				sentirteletransporte(X,Y,mapa),
				not(sentirteletransporte(X,Y,mapamental)),
	
				Li=[],
	
				((X>1,XOeste is X-1,get_char(XOeste,Y,mapamental,C),(C='?';C=x),adiciona([XOeste,Y],Li,L),!);L=Li),
				((nb_getval(lim_X,LIMX), X<LIMX, XLeste is X+1,get_char(XLeste,Y,mapamental,C1),(C1='?';C1=x),adiciona([XLeste,Y],L,L2),!);L2=L),
				((Y>1,YSul is Y-1,get_char(X,YSul,mapamental,C2),(C2='?';C2=x),adiciona([X,YSul],L2,L3),!);L3=L2),
			 	((nb_getval(lim_Y,LIM), Y<LIM, YNorte is Y+1,get_char(X,YNorte,mapamental,C3),(C3='?';C3=x),adiciona([X,YNorte],L3,L4),!);L4=L3),
			 	
			 	
				getlistaperigo(listateletransporte,L5),

				((
					not(member(L4,L5)),
					adiciona(L4,L5,L6),
					setlistaperigo(listateletransporte,L6),
		
					checarestantesteletransporte(t,listateletransporte),
					changeadj(X,Y,'?')
				);
				!)
			)
			;
			(
				not(sentirteletransporte(X,Y,mapa)),
	
				excluiradj(listateletransporte,X,Y),
	
				checarestantesteletransporte(t,listateletransporte)
		
		)
	);
	!.

checamonstro(X,Y):-

	(
		(
				sentirmonstro(X,Y,mapa),
				not(sentirmonstro(X,Y,mapamental)),
				
				Li1=[],
	
				((X>1,XOeste is X-1,get_char(XOeste,Y,mapamental,C4),(C4='?';C4=x),adiciona([XOeste,Y],Li1,L7),!);L7=Li1),
				((nb_getval(lim_X,LIMX), X<LIMX, XLeste is X+1,get_char(XLeste,Y,mapamental,C5),(C5='?';C5=x),adiciona([XLeste,Y],L7,L8),!);L8=L7),
				((Y>1,YSul is Y-1,get_char(X,YSul,mapamental,C6),(C6='?';C6=x),adiciona([X,YSul],L8,L9),!);L9=L8),
			 	((nb_getval(lim_Y,LIM), Y<LIM, YNorte is Y+1,get_char(X,YNorte,mapamental,C7),(C7='?';C7=x),adiciona([X,YNorte],L9,L10),!);L10=9),
	
				getlistaperigo(listainimigo,L11),

				((
					not(member(L10,L11)),
					adiciona(L10,L11,L12),
					setlistaperigo(listainimigo,L12),
		
					checarestantesinimigo(d,listainimigo),
					changeadj(X,Y,'?')
				);
				!)
			);
			(
				not(sentirmonstro(X,Y,mapa)),
				excluiradj(listainimigo,X,Y),
		
				
				checarestantesinimigo(d,listainimigo)
	

			)
	);
	!.


adj(X,Y,X,Y2):- nb_getval(lim_Y,LIM), Y<LIM, Y2 is Y+1.
adj(X,Y,X,Y2):- Y>1, Y2 is Y-1.
adj(X,Y,X2,Y):- nb_getval(lim_X,LIM), X<LIM, X2 is X+1.
adj(X,Y,X2,Y):- X>1, X2 is X-1.





%----------------RECURSAO AUXILIAR PARA CHECAR RESTANTES ----------------%

checarestantesinimigoaux([],_,[]).
checarestantesinimigoaux([H|T],C,L):-
		(
			(length(H,N),
				N=1,
				H=[[X,Y]],			
				change(mapamental,X,Y,C),

				getlistaperigo(listainimigo,Lini),
				excluiRow(Lini,[X,Y],T1),
				setlistaperigo(listainimigo,T1),

				getlistaperigo(listaburaco,Lbur),
				exclui(Lbur,[X,Y],LburNovo),
				setlistaperigo(listaburaco,LburNovo),

				getlistaperigo(listateletransporte,Ltel),
				exclui(Ltel,[X,Y],LtelNovo),
				setlistaperigo(listateletransporte,LtelNovo),

				
				checarestantesinimigoaux(T1,C,T2),
				L=T2,
				!
			)
			;
			(
				checarestantesinimigoaux(T,C,T1),
				L=[H|T1]
			)
		).

checarestantesburacoaux([],_,[]).
checarestantesburacoaux([H|T],C,L):-
		(
			(length(H,N),
				N=1,
				H=[[X,Y]],				
				change(mapamental,X,Y,C),

				getlistaperigo(listaburaco,Lbur),
				excluiRow(Lbur,[X,Y],T1),
				setlistaperigo(listaburaco,T1),

				getlistaperigo(listainimigo,Lini),
				exclui(Lini,[X,Y],LiniNovo),
				setlistaperigo(listainimigo,LiniNovo),


				getlistaperigo(listateletransporte,Ltel),
				exclui(Ltel,[X,Y],LtelNovo),
				setlistaperigo(listateletransporte,LtelNovo),

				checarestantesburacoaux(T1,C,T2),
				L=T2,
				!
			)
			;
			(
				checarestantesburacoaux(T,C,T1),
				L=[H|T1]
			)
		).

checarestantesteletransporteaux([],_,[]).
checarestantesteletransporteaux([H|T],C,L):-
		(
			(length(H,N),
				N=1,
				H=[[X,Y]],
				change(mapamental,X,Y,C),

				getlistaperigo(listateletransporte,Ltel),
				excluiRow(Ltel,[X,Y],T1),
				setlistaperigo(listateletransporte,T1),

				getlistaperigo(listainimigo,Lini),
				exclui(Lini,[X,Y],LiniNovo),
				setlistaperigo(listainimigo,LiniNovo),

				getlistaperigo(listaburaco,Lbur),
				exclui(Lbur,[X,Y],LburNovo),
				setlistaperigo(listaburaco,LburNovo),

				checarestantesteletransporteaux(T1,C,T2),
				L=T2,
				!
			)
			;
			(
				checarestantesteletransporteaux(T,C,T1),
				L=[H|T1]
			)
		).
%-------------CHECA INIMIGOS RESTANTES ------------------%


checarestantesinimigo(C,Nome):-

	getlistaperigo(Nome,L),

	checarestantesinimigoaux(L,C,L3),

	(
		(
			L==L3,
			!
		)
		;
		(
			setlistaperigo(Nome,L3),
			checarestantesburaco(p,listaburaco),
			checarestantesteletransporte(t,listateletransporte)
		)
	).
%-------------CHECA BURACOS RESTANTES ------------------%


checarestantesburaco(C,Nome):-

	getlistaperigo(Nome,L),

	checarestantesburacoaux(L,C,L3),

	(
		(
			L==L3,
			!
		)
		;
		(
			setlistaperigo(Nome,L3),
			checarestantesinimigo(p,listainimigo),
			checarestantesteletransporte(t,listateletransporte)
		)
	).

%-------------CHECA TELETRANSPORTES RESTANTES ------------------%


checarestantesteletransporte(C,Nome):-

	getlistaperigo(Nome,L),

	checarestantesteletransporteaux(L,C,L3),

	(
		(
			L==L3,
			!
		)
		;
		(
			setlistaperigo(Nome,L3),
			checarestantesinimigo(p,listainimigo),
			checarestantesburaco(p,listaburaco)
		)
	).


%---------------FEELS-------------%


sentirbrisa(X,Y,Nome):-
	adj(X,Y,X2,Y2),
	get_char(X2,Y2,Nome,C),
	(C=p,!).

sentirmonstro(X,Y,Nome):-
	adj(X,Y,X2,Y2),
	get_char(X2,Y2,Nome,C),
	(C=d,!).

sentirteletransporte(X,Y,Nome):-
	adj(X,Y,X2,Y2),
	get_char(X2,Y2,Nome,C),
	((C=t),!).



%-------------FUNCOES DO MAPA O MAPA---------%

addElem(Elem,Nome):-

	mapaget(mapa,M),
	last(M,L),
	adiciona2(Nome,L,LN),
	length(M,N),
	Idx is N-1,
	replace(M,Idx,LN,MN),
	mapaset(Nome,MN).

addNew(Nome):-
	mapaget(Nome,M),
	L=[],
	adiciona2(L,M,MN),
	mapaset(Nome,MN).

begin(LimX,Limy):-
		setlim(lim_Y,LimY),
		setlim(lim_X,LimX),
		createlistaperigo(listaburaco),
		createlistaperigo(listainimigo),
		createlistaperigo(listateletransporte),
		createmapmental(mapamental),
		createmap(mapa),.


%------------BEST FUNÇOES---------------%

init():-createmapmental(M,mapamental),
				createmap(M,mapa),
				createlistaperigo(listaburaco),
				createlistaperigo(listainimigo),
				createlistaperigo(listateletransporte),
				setlim(lim_Y,3),
				setlim(lim_X,3).
				%nb_setval(listainimigo,[[[1,1]],[[1,2],[2,3]]]),
				%nb_setval(listaburaco,[[[1,1],[2,3]]]).


printAll(M1,M2,L1,L2,L3):-getlistaperigo(listaburaco,L1),
				getlistaperigo(listainimigo,L2),
				getlistaperigo(listateletransporte,L3),
				 mapaget(mapa,M1),
				 mapaget(mapamental,M2).
