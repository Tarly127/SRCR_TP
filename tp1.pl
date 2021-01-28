:- set_prolog_flag(discontiguous_warnings, off).
:- set_prolog_flag(single_var_warnings, off).
:- set_prolog_flag(unknown, fail).


% - Declarações Iniciais dos Predicados

:- op(900, xfy, '::').
:-dynamic '-'/1.

:-dynamic fast/1.

:-dynamic date/3.
:-dynamic adjudicante/4.
:-dynamic adjudicatario/4.
:-dynamic contrato/10.
:-dynamic last3Years/5.
:-dynamic concat/3.
:-dynamic listSum/2.
:-dynamic adjeContratos/2.
:-dynamic adjaContratos/2.
:-dynamic anoContsAdja/3.
:-dynamic descContrato/2.
:-dynamic totTresAnos/3.
:-dynamic contMaisRecenteAdja/2.
:-dynamic maisRecenteAux/3.
:-dynamic dataMaisRecente/3.
:-dynamic excecaoImp/1.
:-dynamic excecaoInc/1.
:-dynamic excecaoInt/1.

% - Extensão do Predicado Insere

insere(X):-assert(X).
insere(X):-retract(X),!,fail.

% - Extensão do Predicado Remove

remove(X):-retract(X).
remove(X):-assert(X),!,fail.

% - Extensão do Meta Predicado Evolução

evolucao(X):-
  findall(I, +X :: I, L),
  insere(X),
  testa(L).

% - Extensão do Meta Predicado Involução

involucao(X):-
  findall(I, -X :: I, L),
  remove(X),
  testa(L).


% - Declarações de Informação da Base de Conhecimento

excecaoInc(adjudicante(6, desc, 148812717, desc)).
excecaoImp(adjudicatario(6, X, 111112225, 'Rua Tal, nº9, Areosa, Porto')):-member(X,['EmpresaX','EmpresaY']).
excecaoInt(adjudicante(7, 'Cabana do Pai Tomás', 098765555, null)).

adjudicante(1, 'Freguesia da Areosa', 123456789, 'Areosa, Viana do Castelo, Viana do Castelo').
adjudicante(2, 'Conselho da Póvoa de Varzim', 987654321, 'Póvoa de Varzim, Póvoa de Varzim, Porto').
adjudicante(3, 'Freguesia de Vila Flor e Nabo', 135792468, 'Vila Flor e Nabo, Vila Flor, Bragança').
adjudicante(4, 'Freguesia de Cristelo', 246813579, 'Cristelo, Barcelos, Braga').
adjudicante(5, 'Município de Monserrate', 998877665, 'Monserrate, Viana do Castelo, Viana do Castelo').

adjudicatario(1, 'Julio Pedra e Companhia', 10010010, 'Rua Domingos, 27, Areosa, Viana do Castelo').
adjudicatario(2,'7 e 7 Lda', 111111111, 'Rua Martins, 3, Freixo de Espada a Cinta, Faro').
adjudicatario(3, 'Business Inc', 12312312, 'Rua Rua, 78, Viana do Alentejo, Beja').

contrato(1, 1, 1, aquisicaobensmoveis, ajustedireto, 'Descricao1',4000, 92, 'Areosa, Viana do Castelo', date(1999,03,03)).
contrato(2, 3, 2, locacaobensmoveis, consultaprevia, 'Descricao2',2900, 180, 'Celorico de Bastos, Braga', date(2014,09,04)).
contrato(3, 2, 3, aquisicaoservicos, ajustedireto, 'Descricao3',5000, 365, 'Guimaraes, Braga', date(2020,05,09)).
contrato(4, 1, 4, aquisicaoservicos, consultaprevia, 'Descricao4', 30090, 45, 'Evora, Evora', date(1979,01,01)).
contrato(5, 2, 5, aquisicaoservicos, concursopublico, 'Descricao5', 10000,127, 'Darque, Viana do Castelo', date(2018,09,09)).



% - Conhecimento Positivo

%Uma data tem de ter um formato AAAA/MM/DD, com os números corretos
+date(A,M,D)::(date(A,M,D),
    A>=1970,
    D>=1, (
    (member(M, [1,3,5,7,8,10,12]), D=<31);
    (member(M, [2]), D=<28);
    (member(M, [2]), 0 is mod(A,4), D=<29);
    (member(M, [4,6,9,11]), D=<30))
    ).

%O Id do adjudicante é único
+adjudicante(I,_,_,_)::(findall(I, (adjudicante(I,_,_,_)), S1), length(S1, N), N==1).

%O Id do Adjudicatário é único
+adjudicatario(I,_,_,_)::(findall(I, adjudicatario(I,_,_,_), S), length(S, N), N==1).

%O Id do contrato é único
+contrato(_,_,Id,_,_,_,_,_,_,_)::(findall(Id, contrato(_,_,Id,_,_,_,_,_,_,_), S), length(S, N), N==1).

%Os IDs do Adjudicante e do Adjudicatário têm ambos de existir
+contrato(IdAdje,IdAdja,_,_,_,_,_,_,_,_)::((
    findall(IdAdje, adjudicante(IdAdje,_,_,_), S1),
    length(S1, N1),
    N1==1),
    (findall(IdAdja, adjudicante(IdAdja,_,_,_), S2),
    length(S2, N2),
    N2==1)).

%A data tem de seguir o formato AAAA/MM/DD, com os números corretos
+contrato(_,_,_,_,_,_,_,_,_,date(A,M,D))::(
    A>=1970,
    D>=1, (
    (member(M, [1,3,5,7,8,10,12]), D=<31);
    (member(M, [2]), D=<28);
    (member(M, [2]), 0 is mod(A,4), D=<29);
    (member(M, [4,6,9,11]), D=<30))
).

%Regra dos três anos
+contrato(Adje,Adja,_,TC,_,_,V,_,_,date(A,_,_))::(last3Years(Adje,Adja,TC,A,S), listSum(S, Tot), Tot=<75000).

%O contrato só pode ser um de três procedimentos, e, no caso de ser Ajuste direto, só pode ter um de três tipos, prazo menor do que 1 ano e valor menor do que 5000€
+contrato(_,_,_,TC,TP,_,_,V,P,_,_)::
    (member(TP, [consultaprevia, concursopublico]);
    (TP==ajustedireto,
    (TC==aquisicaomoveis;TC==locacaobensmoveis;TC==aquisicaoservicos),
    V=<5000,
    P=<365)
    ).






% - Conhecimento Negativo

%Negação forte implica negação por falta de prova para todos os predicados principais
-date(A,M,D):-
  nao(date(A,M,D)),
  nao(excecaoImp(date(A,M,D))),
  nao(excecaoInc(date(A,M,D))),
  nao(excecaoInt(date(A,M,D))).
-adjudicante(I, N, Ni, M):-
  nao(adjudicante(I, N, Ni, M)),
  nao(excecaoImp(adjudicante(I, N, Ni, M))),
  nao(excecaoInc(adjudicante(I, N, Ni, M))),
  nao(excecaoInt(adjudicante(I, N, Ni, M))).
-adjudicatario(I, N, Ni, M):-
  nao(adjudicatario(I, N, Ni, M)),
  nao(excecaoImp(adjudicatario(I, N, Ni, M))),
  nao(excecaoInc(adjudicatario(I, N, Ni, M))),
  nao(excecaoInt(adjudicatario(I, N, Ni, M))).
-contrato(IdAd, IdAda, IdC, TC, TP, Desc, Val, Prazo, Local, Data):-
  nao(contrato(IdAd, IdAda, IdC, TC, TP, Desc, Val, Prazo, Local, Data)),
  nao(excecaoImp(contrato(IdAd, IdAda, IdC, TC, TP, Desc, Val, Prazo, Local, Data))),
  nao(excecaoInc(contrato(IdAd, IdAda, IdC, TC, TP, Desc, Val, Prazo, Local, Data))),
  nao(excecaoInt(contrato(IdAd, IdAda, IdC, TC, TP, Desc, Val, Prazo, Local, Data))).

% - Não se pode remover um Adjudicante se ele tiver feito um contrato
-adjudicante(ID,_,_,_)::(findall(IdC, contrato(ID,_,IdC,_,_,_,_,_,_,_), S), length(S,L), L==0).

% - Não se pode remover um Adjudicatário se ele tiver feito um contrato
-adjudicatario(ID,_,_,_)::(findall(IdC, contrato(_,ID,IdC,_,_,_,_,_,_,_), S), length(S,L), L==0).

% - QUERIES DE PROCURA

% - Procurar os Contratos de um Adjudicante (ID)
adjeContratos(IdAdje, S):-
  findall(Id, contrato(IdAdje,_,Id,_,_,_,_,_,_,_), S).

% - Procurar os Contratos de um Adjudicatário (ID)
adjaContratos(IdAdja, S):-
  findall(Id, contrato(_,IdAdja,Id,_,_,_,_,_,_,_), S).

% - Valor total dos contratos feitos num dado ano por um dado Adjudicatário
anoContsAdja(IdAdja, Ano, Total):-
  findall(V, contrato(_,IdAdja,_,_,_,_,V,_,_,date(A,_,_)), S),
  listSum(S, Total).

% - Obter a descrição de um contrato dado o ID
descContrato(Id, Desc):-
  contrato(_,_,Id,_,_,Desc,_,_,_,_).

% - Obter Total Faturado em contratos entre os mesmos Adjudicante/Adjudicatário
totAdjeAdja(IdAdja,IdAdje,Total):-
  findall(V, contrato(IdAdje, IdAdja,_,_,_,_,V,_,_,_), S),
  listSum(S, Total).

% - Obter o maior valor de um contrato de um certo Adjudicatário
maiorValorAdja(IdAdja, Valor):-
  findall(V, contrato(_,IdAdja,_,_,_,_,V,_,_,_), S),
  sort(S, S2),
  last(S2, Valor).

% - Obter a data do contrato mais recente de um Adjudicatário
contMaisRecenteAdja(IdAdja, DataCont):-
  findall(Id, contrato(_,IdAdja,Id,_,_,_,_,_,_,_), [H|T]),
  contrato(_,_,H,_,_,_,_,_,_,D),
  maisRecenteAux([H|T],D, DataCont),
  !.

% - Obter o ID do contrato mais recente de um Adjudicante
contMaisRecenteAdje(IdAdje, IdCont):-
  findall(Id, contrato(IdAdje,_,Id,_,_,_,_,_,_,_), [H|T]),
  contrato(_,_,H,_,_,_,_,_,_,D),
  maisRecenteAux([H|T],D, DataCont),
  !,
  contrato(_,_,IdCont,_,_,_,_,_,_,DataCont).

% - Obter os valores dos N contratos com maior valor
contsMaiorValor(N, ListaIds):-
  findall(V, contrato(_,_,_,_,_,_,V,_,_,_), List),
  sort(List, List2),
  reverse(List2,List3),
  take(List3, N, ListaIds).

% - ATUALIZAÇÃO DE INFORMAÇÃO DA BC

% - Atualizar o nome de um adjudicante
atualizarNomeAdjudicante(IdAdje, Nome):-
  remove(adjudicante(IdAdje, _, Nif, Morada)),
  insere(adjudicante(IdAdje, Nome, Nif, Morada)).

% - Atualizar o NIF de um adjudicante
atualizarNIFAdjudicante(IdAdje, Nif):-
  remove(adjudicante(IdAdje, Nome, _, Morada)),
  insere(adjudicante(IdAdje, Nome, Nif, Morada)).

% - Atualizar a morada de um adjudicante
atualizarMoradaAdjudicante(IdAdje, Morada):-
  remove(adjudicante(IdAdje, Nome, Nif, _)),
  insere(adjudicante(IdAdje, Nome, Nif, Morada)).

% - Atualizar o nome de um adjudicatario
atualizarNomeAdjudicatario(IdAdja, Nome):-
  remove(adjudicatario(IdAdja, _, Nif, Morada)),
  insere(adjudicatario(IdAdja, Nome, Nif, Morada)).

% - Atualizar o NIF de um adjudicatario
atualizarNIFAdjudicatario(IdAdja, Nif):-
  remove(adjudicatario(IdAdja, Nome, _, Morada)),
  insere(adjudicatario(IdAdja, Nome, Nif, Morada)).

% - Atualizar a morada de um adjudicatario
atualizarMoradaAdjudicatario(IdAdja, Morada):-
  remove(adjudicatario(IdAdja, Nome, Nif, _)),
  insere(adjudicatario(IdAdja, Nome, Nif, Morada)).

% - Atualizar o prazo de um contrato
atualizarPrazo(IdCont, NPrazo):-
  remove(contrato(A,B,IdCont,C,D,E,F,_,G,H)).
  evolucao(contrato(A,B,IdCont,C,D,E,F,NPrazo,G,H)).

% - Atualizar o valor de um contrato
atualizaValor(IdCont, NValor):-
  remove(contrato(A,B,IdCont,C,D,E,_,F,G,H)).
  evolucao(contrato(A,B,IdCont,C,D,E,NValor,F,G,H)).


% - PREDICADOS AUXILIARES

% - Auxiliar que calcula a data mais recente entre vários contratos, dada um lista com os seus IDs.
maisRecenteAux([],F,F).
maisRecenteAux([H|T], D2, A):-
  contrato(_,_,H,_,_,_,_,_,_,D1),
  dataMaisRecente(D1,D2,D3),
  maisRecenteAux(T, D3, A).

% - Predicado que calcula a maior entre duas datas.
dataMaisRecente(date(A1,M1,D1), date(A2,M2,D2), date(A1,M1,D1)):-
  (A1>A2);
  (A1==A2, M1>M2);
  (A1==A2,M1==M2,D1>=D2).
dataMaisRecente(date(A1,M1,D1), date(A2,M2,D2), date(A2,M2,D2)):-
  (A2>A1);
  (A1==A2, M2>M1);
  (A1==A2,M1==M2,D2>=D1).

% - Auxiliar do Invariante da Regra dos 3 anos que nos permite obter o valor dos contratos feitos entre um dado Adjudicatário e Adjudicante nos últimos 3 anos, de um dado tipo.
last3Years(IdAdje, IdAdja, TC, A, S):-
  findall(V,contrato(IdAdje, IdAdja,_,TC,_,_,V,_,_,date(A,_,_)), S1),
  B is A-1, findall(V,contrato(IdAdje, IdAdja,_,TC,_,_,V,_,_,date(B,_,_)), S2),
  C is A-2, findall(V,contrato(IdAdje, IdAdja,_,TC,_,_,V,_,_,date(C,_,_)), S3),
  concat(S1, S2, SA),
  concat(S3, SA, S).

% - Extensão do Predicado ListSum, que soma todos os elementos de uma lista
listSum([], 0).
listSum([H|T], S):-listSum(T, R), S is R+H.

% - Extensão do predicado Last que obtém o último elemento de uma lista.
last([V],V).
last([H|T], V):-last(T, V).

% - Extensão do Predicado Concat, que concatena duas listas
concat([],L,L).
concat([H|T],L,[H|Z]):- concat(T,L,Z).

% - Auxiliar para obter os N primeiros elementos de uma lista
take([H|_],1,[H|[]]).
take([H|T], N, [H|T2]):-M is N-1, take(T, M, T2).

% - Auxiliar que inverte uma lista
reverse([],[]).
reverse([H|T],L) :- reverse(T, L2), concat(L2, [H], L).

% - Extensão do Predicado Testa
testa([]).
testa([H|T]):-H,testa(T).


% - Extensão do Meta Predicado SI
si(Questao, verdadeiro) :- Questao.
si(Questao, falso) :- -Questao.
si(Questao, impreciso) :- nao(Questao), nao(-Questao), excecaoImp(Questao).
si(Questao, incerto) :- nao(Questao), nao(-Questao), excecaoInc(Questao).
si(Questao, interdito) :- nao(Questao), nao(-Questao), excecaoInt(Questao).

% - Extensão do Meta Predicado Não
nao(Questao) :- Questao, !, fail.
nao(Questao).
