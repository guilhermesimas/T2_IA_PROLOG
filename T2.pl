

:- dynamic (ouro/2).
:- dynamic (powerup/3).

:- dynamic (buraco/3).
:- dynamic (inimigo/3).
:- dynamic (inimigod/4).
:- dynamic (inimigoD/4).
:- dynamic (teleport/3).
:- dynamic (saida/3).

:- dynamic (notp/3).

:- dynamic (possivelp/3).

:- dynamic (desconhecido/2).
:- dynamic (visitado/2).

:- dynamic (modified/3).

:- discontiguous set/4.
:- discontiguous set/3.


setlim(Nome,Val):-nb_setval(Nome,Val).

init():-
    setlim(lim_Y,5),
    setlim(lim_X,5),
    retractall(dependente(_,_,_,_,_)),
    retractall(possivelp(_,_,_)),
    retractall(ouro(_,_)),
    retractall(powerup(_,_,_)),
    retractall(buraco(_,_,_)),
    retractall(inimigo(_,_,_)),
    retractall(inimigod(_,_,_)),
    retractall(inimigoD(_,_,_)),
    retractall(teleport(_,_,_)),
    retractall(notp(_,_,_)),
    retractall(desconhecido(_,_)),
    retractall(visitado(_,_)).

setmapa():- assert(ouro(1,2)).

setmapa():- assert(ouro(4,3)).
    %assert(buraco(mapa,2,5));
    %assert(buraco(mapa,5,4));
    %assert(inimigod(mapa,4,2)).

adiciona(X,L,[X|L]).

adj(X,Y,norte,X,Y2):- nb_getval(lim_Y,LIM), Y<LIM, Y2 is Y+1.
adj(X,Y,sul,X,Y2):- Y>1, Y2 is Y-1.
adj(X,Y,leste,X2,Y):- nb_getval(lim_X,LIM), X<LIM, X2 is X+1.
adj(X,Y,oeste,X2,Y):- X>1, X2 is X-1.

set(mapa,X,Y,buraco):- assert(buraco(mapa,X,Y)).
set(mapamental,X,Y,buraco):- 
    not(buraco(mapamental,X,Y)),
    assert(buraco(mapamental,X,Y)),
    assert(modified(X,Y,buraco)),
    X2 is X+2, X3 is X-2,Y2 is Y-2, Y3 is Y+2, X4 is X+1, Y4 is Y+1,Y5 is Y-1, X5 is X-1,
    (set(buraco,X2,Y,notp);
        set(buraco,X3,Y,notp);
        set(buraco,X,Y2,notp);
        set(buraco,X,Y3,notp);
        set(buraco,X4,Y4,notp);
        set(buraco,X4,Y5,notp);
        set(buraco,X5,Y4,notp);
        set(buraco,X5,Y5,notp); 
        set(inimigo,X,Y,notp);
        set(teleport,X,Y,notp)).

set(mapa,X,Y,teleport):-assert(teleport(mapa,X,Y)).
set(mapamental,X,Y,teleport):-
    not(teleport(mapamental,X,Y)),
    assert(teleport(mapamental,X,Y)),
    assert(modified(X,Y,teleport)),
   X2 is X+2, X3 is X-2,Y2 is Y-2, Y3 is Y+2, X4 is X+1, Y4 is Y+1,Y5 is Y-1, X5 is X-1,
   ( set(teleport,X2,Y,notp);
       set(teleport,X3,Y,notp);
       set(teleport,X,Y2,notp);
       set(teleport,X,Y3,notp);
       set(teleport,X4,Y4,notp);
       set(teleport,X4,Y5,notp);
       set(teleport,X5,Y4,notp);
       set(teleport,X5,Y5,notp); 
       set(inimigo,X,Y,notp);
       set(buraco,X,Y,notp)).

set(mapamental,X,Y,inimigo):-
    not(inimigo(mapamental,X,Y)),
    assert(inimigo(mapamental,X,Y)),
    assert(modified(X,Y,inimigo)),
    X2 is X+2, X3 is X-2,Y2 is Y-2, Y3 is Y+2, X4 is X+1, Y4 is Y+1,Y5 is Y-1, X5 is X-1,
    (set(inimigo,X2,Y,notp);
        set(inimigo,X3,Y,notp);
        set(inimigo,X,Y2,notp);
        set(inimigo,X,Y3,notp);
        set(inimigo,X4,Y4,notp);
        set(inimigo,X4,Y5,notp);
        set(inimigo,X5,Y4,notp);
        set(inimigo,X5,Y5,notp); 
        set(teleport,X,Y,notp);
        set(buraco,X,Y,notp)).

set(Nome,X,Y,possivelp):-not(possivelp(Nome,X,Y)),assert(possivelp(Nome,X,Y)),
                         S = possivel, string_concat(S,Nome,S2),assert(modified(X,Y,S2)).

set(Nome,X,Y,notp):-not(notp(Nome,X,Y)),assert(notp(Nome,X,Y)),
                    S = not, string_concat(S,Nome,S2),assert(modified(X,Y,S2)).

set(X,Y,visitado):-not(visitado(X,Y)),assert(visitado(X,Y)),assert(modified(X,Y,visitado)).

%----------------------------------------------------------------------------------
perigo(X,Y):- buraco(mapamental,X,Y); inimigo(mapamental,X,Y); teleport(mapamental,X,Y).
safe(X,Y):-notp(buraco,X,Y),notp(inimigo,X,Y),notp(teleport,X,Y).

possivelperigo(X,Y):- possivelp(buraco,X,Y); possivelp(inimigo,X,Y); possivelp(teleport,X,Y).

%--------------------------------------------------------------------------------

sentirbrisa(Nome,X,Y):-
    adj(X,Y,_,X2,Y2),
    buraco(Nome,X2,Y2),!.

ouvirpassos(X,Y):-
    adj(X,Y,_,X2,Y2),
    ((inimigod(X2,Y2,_);inimigoD(X2,Y2,_)),!).

verflash(Nome,X,Y):-
    adj(X,Y,_,X2,Y2),
    teleport(Nome,X2,Y2),!.

%-----------------------------ATUALIZA MAPA MENTAL -------------------------------------%

observalocal(estado(X,Y,D,S,V,M,O),estado(X2,Y2,D,S2,V2,M,O)):-
    (buraco(mapa,X,Y), set(mapamental,X,Y,buraco), S2 is S-1000, V2 is 0, X2=X,Y2=Y,!);
    (inimigod(X,Y,_), set(mapamental,X,Y,inimigo), S2 is S-20, V2 is V-20,X2=X,Y2=Y,!);
    (inimigoD(X,Y,_),set(mapamental,X,Y,inimigo), S2 is S-50, V2 is V-50,X2=X,Y2=Y,!);
    (teleport(mapa,X,Y), set(mapamental,X,Y,teleport), X2 is X+1, Y2 is Y+1,S2=S,V2=V,!); %%FAZER RANDOMICO
    (powerup(mapa,X,Y), assert(powerup(mapamental,X,Y)),X2=X,Y2=Y,S2=S,V2=V,!);
    (X2=X,Y2=Y,S2=S,V2=V),
    ((set(buraco,X,Y,notp);set(inimigo,X,Y,notp);set(teleport,X,Y,notp));!).


%% observa ao redor, e atualiza a memoria de acordo com o que foi sentido
observa(X,Y):-  set(X,Y,visitado).
observa(X,Y):- (sentirbrisa(mapa,X,Y), updateMemP(X,Y,buraco) ); (not(sentirbrisa(mapa,X,Y)), updateMemSafe(X,Y,buraco)).
observa(X,Y):- (ouvirpassos(X,Y), updateMemP(X,Y,inimigo) ); (not(ouvirpassos(X,Y)), updateMemSafe(X,Y,inimigo)).
observa(X,Y):- (verflash(mapa,X,Y), updateMemP(X,Y,teleport) ); (not(verflash(mapa,X,Y)), updateMemSafe(X,Y,teleport)).
observa(_,_).

updateMemSafe(X,Y,Nome):- % marca todas as adjacencias como nao perigo
    adj(X,Y,_,X2,Y2),
    (set(Nome,X2,Y2,notp); retract(possivelp(Nome,X2,Y2))).

updateMemP(X,Y,Nome):- % marca as adjacencias que nao foram visitadas ou nao sao "nao perigo" como possiveis perigos, e entao checa se hÃ¡ mais de uma
    adj(X,Y,_,X2,Y2),
    not(visitado(X2,Y2)),
    not(notp(Nome,X2,Y2)),
    set(Nome,X2,Y2,possivelp).

countp(Nome,X,Y,C4):- % conta as adjacencias que sao possiveis perigos (ps, nao cosnegui pensar um jeito inteligente de implementar, fiz do burro)
    C is 0, 
    (((adj(X,Y,norte,X2,Y2),possivelp(Nome,X2,Y2)),C1 is C+1,!); C1=C),
    (((adj(X,Y,sul,X3,Y3),possivelp(Nome,X3,Y3)),C2 is C1+1,!); C2=C1),
    (((adj(X,Y,leste,X4,Y4),possivelp(Nome,X4,Y4)),C3 is C2+1,!); C3=C2),
    (((adj(X,Y,oeste,X5,Y5),possivelp(Nome,X5,Y5)),C4 is C3+1,!); C4=C3).

checkperigo(Nome,X,Y):- % se houver apenas um possivel perigo, marca como perigo
    countp(Nome,X,Y,C),
    C = 1,
    adj(X,Y,_,X2,Y2),
    possivelp(Nome,X2,Y2),
    set(mapamental,X2,Y2,Nome).

%----------------------------------AÃ‡Ã•ES------------------------------------------%


acao(estado(X,Y,D,S,V,M,O), mover_para_frente, Estado2):- mover(X,Y,D,X2,Y2), S2 is S-1, Estado2 = estado(X2,Y2,D,S2,V,M,O), !.
acao(estado(X,Y,D,S,V,M,O), virar_a_direita, Estado2):- virar(D,D2), S2 is S-1, Estado2 = estado(X,Y,D2,S2,V,M,O) , !.
acao(estado(X,Y,D,S,V,M,O), pegar_objeto, Estado2):- pegar_objeto(X,Y,V,O,S,V2,O2,S2), S3 is S2-1, Estado2 = estado(X,Y,D,S3,V2,M,O2) , !.
acao(estado(X,Y,D,S,V,M,O), atirar, Estado2):- atirar(X,Y,D,M,M2), S2 is S-10, Estado2 = estado(X,Y,D,S2,V,M2,O) , !.

mover(X,Y,D,X2,Y2):-adj(X,Y,D,X2,Y2).

virar(norte,leste).
virar(sul,oeste).
virar(leste,sul).
virar(oeste,norte).

pegar_objeto(X,Y,V,O,S,V,O2,S2):- 
    ouro(X,Y),
    O2 is O-1,
    S2 is S+1000,
    retractall(ouro(X,Y)),!.
pegar_objeto(X,Y,V,O,S,V2,O,S):- 
    powerup(mapa,X,Y),
    retract(powerup(_,X,Y)),
    ((V<80,V2 is V+20,!) ; (V2 = 100)),!.
pegar_objeto(_,_,V,O,S,V,O,S). 

atirar(X,Y,D,M,M2):-
    M>0,
    M2 is M-1,
    adj(X,Y,D,X2,Y2),
    inimigod(X2,Y2,V),
    V2 is V - 20,
    retract(inimigod(X2,Y2,_)),
    V2>0,
    assert(inimigod(X2,Y2,V2)).
atirar(X,Y,D,M,M2):-
    M>0,
    M2 is M-1,
    adj(X,Y,D,X2,Y2),
    inimigoD(X2,Y2,V),
    V2 is V - 20,
    retract(inimigoD(X2,Y2,_)),
    V2>0,
    assert(inimigoD(X2,Y2,V2)).


agir(estado(X,Y,D,S,V,M,O)):- %% PROBLEMA: COMO A OBSERVA TEM VÃ�RIOS CAMINHOS, ELE DA A SUGESTAO APOS EXECUTAR CADA CAMINHO, POR ISSO, A PRIMEIRA SUGESTAO 
                                %%          DADA PODE NÃƒO SER A MELHOR 
    observalocal(estado(X,Y,D,S,V,M,O),estado(X,Y,D,_,_,M,O)),
    observa(X,Y).
    


%-----------------------------SUGESTOES---------------------------------%


sugestao(estado(X,Y,_,_,_,_,0),R):-
    saida(mapa,X,Y),
    R=sair,
    !.

sugestao(estado(_,_,_,_,_,_,0),R):-
    R=astar_saida,!.

sugestao(estado(X,Y,_,_,_,_,_),R):- 
    ouro(X,Y),
    R=pegar_ouro,
    !.

sugestao(estado(_,_,_,_,V,_,_),R):-
    V<50,
    R=pegar_powerup,
    !.

sugestao(estado(X,Y,D,_,_,_,_),R):-
    adj(X,Y,D,X2,Y2),
    safe(X2,Y2),
    not(visitado(X2,Y2)),
    R=mover_para_frente,
    !.

sugestao(estado(X,Y,_,_,_,_,_),R):-
    adj(X,Y,_,X2,Y2),
    safe(X2,Y2),
    not(visitado(X2,Y2)),
    R=virar_a_direita,
    !.

sugestao(estado(_,_,_,_,_,_,_),R):-
    safe(X,Y),
    not(visitado(X,Y)),
    R=astar_safe,!. 
    
%FUNCAO AUXILIAR PRO JAVA DESENHAR MAPA MENTAL%
% X2 e Y2 são adjacencias "safe" de X,Y
safeadj(X,Y,X2,Y2)