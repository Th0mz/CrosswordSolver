:- discontiguous lista_palavras/2.
:- discontiguous lista_posicoes/2.
:- discontiguous num_colunas/2.
:- discontiguous lista_letras/2.

%------------------------------------------------------------------------------------------
% para obter um puzzle deve usar o predicado puzzle(Nome, Puzzle)
% Nome pode ser um de puz1, puz2, puz3, puz4.
% Por exemplo
%?- puzzle(puz1, Puz).
%Puz = [[ate, cre, fio, faca, feio, olha, acola, levem|...], [[_1568, _1574, _1586,
%    _1604, #, _1658|...], [#, _1352, #, _1382, _1406|...], [_1124, _1130, _1142,
%    _1160|...], [_902, #, #|...], [_680, _686|...]]].

%------------------------------------------------------------------------------------------

%------------------------------- puz1 (5 x 8) --------------------------------------------
% fica resolvido so com inicializa
% so tem 1 solucao

lista_palavras(puz1, [ate,cre,fio,faca,feio,olha,acola,levem,prima,servo]).
lista_posicoes(puz1, [[5,7], [1,3,7], [5,7], [2,3], [5,7]]).
num_colunas(puz1, 8).
lista_letras(puz1, []).
%------------------------------- puz2 (7 x 4) -----------------------------------------
% fica resolvido so com inicializa
% so tem 1 solucao

lista_palavras(puz2, [hei, hem, iam, lei, mel, meu, nem, uni]).
lista_posicoes(puz2, [[1,2,3,4], [4,6], [2,6], [4,5,6,7]]).
num_colunas(puz2, 7).
lista_letras(puz2, []).
%------------------------------- puz3 (6 x 6) -----------------------------------------
% nao fica resolvido so com inicializa
% tem solucao unica

lista_palavras(puz3, [eis, nos, noz, nus, ois, sos, uns, uso, zen]).
lista_posicoes(puz3, [[4,5,6], [2,3,4,5], [4,5], [1,3], [1,5,6], [1,2,3]]).
num_colunas(puz3, 6).
lista_letras(puz3, [[(1,2), o]]).

%------------------------------- puz4 (8 x 8)--------------------------------------------
% nao fica resolvido so com inicializa
% nao tem solucao unica
% usa retrocesso

lista_palavras(puz4, [ajo, amo, ano, asa,ato, avo, aco, boa, boi, bom,ima,mao,moa,ovo,soa,sob,voo]).
lista_posicoes(puz4, [[2,3,4,5,6,7,8], [4,8],[2,4,6,7],
[1,2,6,7], [2,3,5],
[2,6,8],[4,8],[1,2,6,7,8]]).
num_colunas(puz4, 8).
lista_letras(puz4, []).

%---------------------------------------------------------------------------------------
puzzle(N, [Lst_Pals, Grelha]) :-
    lista_palavras(N, Lst_Pals),
    lista_posicoes(N, Lst_Posicoes),
    num_colunas(N, Num_Colunas),
    gera_grelha(Lst_Posicoes, Num_Colunas, Temp),
    lista_letras(N, Lst_letras),
    findall(Pos, member([Pos, _], Lst_letras), Posicoes),
    findall(Let, member([_, Let], Lst_letras), Letras),
    mat_muda_posicoes(Temp, Posicoes, Letras, Grelha).

%-----------------------------------------------------------------------------
% gera_linha(Lst_Cols, Linha, N_Colunas)
%?- gera_linha([5,7], Linha, 8).
% Linha = [_G417, _G420, _G426, _G435, #, _G462, #, _G501].
%-----------------------------------------------------------------------------

gera_linha(Lst_Cols, Linha, N_Colunas) :-
    gera_linha(Lst_Cols, Linha, N_Colunas, [], 0).

gera_linha(_, Linha, N_Colunas, Linha, N_Colunas) :- !.
gera_linha(Lst_Cols, Linha, N_Colunas, Temp, I) :-
    N_I is I + 1,
    novo_elemento(N_I, Lst_Cols, Novo_El),
    append(Temp, [Novo_El], N_Temp),
    gera_linha(Lst_Cols, Linha, N_Colunas, N_Temp, N_I).

novo_elemento(Col, Lst_Cols, Novo_El) :-
    (member(Col, Lst_Cols), !, Novo_El = #
                    ;
     Novo_El = _).

%-----------------------------------------------------------------------------
% gera_grelha(Lst_Lsts_Cols, N_Colunas, Grelha)
%-----------------------------------------------------------------------------
gera_grelha([], _, []) :- !.

gera_grelha([Lst_Cols | R], N_Colunas, Grelha) :-
    gera_grelha(R, N_Colunas, Grelha_R),
    gera_linha(Lst_Cols, Linha, N_Colunas),
    Grelha = [Linha | Grelha_R].

%-----------------------------------------------------------------------------
% gera_linha_indices(Lst_Cols, Linha, N_Colunas)
%?- gera_linha_indices([5,7], Linha, 8).
% Linha = [_G417, _G420, _G426, _G435, #, _G462, #, _G501].
%-----------------------------------------------------------------------------

gera_linha_indices(Lst_Cols, Linha, N_Colunas, Var_Lin) :-
gera_linha_indices(Lst_Cols, Linha, N_Colunas, [], 0, Var_Lin).

gera_linha_indices(_, Linha, N_Colunas, Linha, N_Colunas, _) :- !.
gera_linha_indices(Lst_Cols, Linha, N_Colunas, Temp, I, Var_Lin) :-
    N_I is I + 1,
    novo_elemento_indices(N_I, Lst_Cols, Novo_El, Var_Lin),
    append(Temp, [Novo_El], N_Temp),
    gera_linha_indices(Lst_Cols, Linha, N_Colunas, N_Temp, N_I, Var_Lin).

novo_elemento_indices(Col, Lst_Cols, Novo_El, Var_Lin) :-
    (member(Col, Lst_Cols), !, Novo_El = #
                        ;
     atom_concat(Var_Lin, Col, Novo_El)).

%-----------------------------------------------------------------------------
% gera_grelha_indices(Lst_Lsts_Cols, N_Colunas, Grelha)
%-----------------------------------------------------------------------------
gera_grelha_indices([Lst_Cols | R], N_Colunas, Grelha) :-
    gera_grelha_indices([Lst_Cols | R], N_Colunas, Grelha, 1).

gera_grelha_indices([], _, [], _) :- !.

gera_grelha_indices([Lst_Cols | R], N_Colunas, Grelha, Num_Lin) :-
    Num_Lin_seguinte is Num_Lin + 1,
    gera_grelha_indices(R, N_Colunas, Grelha_R, Num_Lin_seguinte),
    atom_concat('P', Num_Lin, Var_Lin),
    gera_linha_indices(Lst_Cols, Linha, N_Colunas, Var_Lin),
    Grelha = [Linha | Grelha_R].

%-----------------------------------------------------------------------------
% mat_muda_posicao(Mat, Pos, Cont, N_Mat):
% N_Mat e' o resultado de substituir o conteudo da posicao Pos
% de Mat por Cont.
%-----------------------------------------------------------------------------

mat_muda_posicao(Mat, (L,C), Cont, N_Mat) :-
    nth1(L,Mat,Linha),
    mat_muda_linha(Linha,C,Cont, N_Linha),
    mat_muda_linha(Mat,L,N_Linha, N_Mat),!.

%-----------------------------------------------------------------------------
% mat_muda_posicoes(Mat, Lst_Posicoes, Lst_Cont, N_Mat):
% N_Mat e' o resultado de substituir o conteudo das posicoes de
% Lst_Posicoes de Mat pelo elemento correpondente de Lst_Cont.
%-----------------------------------------------------------------------------
mat_muda_posicoes(Mat, [], _, Mat) :- !.

mat_muda_posicoes(Mat, [Pos | R_Pos], [Cont | R_Cont], N_Mat) :-
    mat_muda_posicao(Mat, Pos, Cont, Temp),
    mat_muda_posicoes(Temp, R_Pos, R_Cont, N_Mat).

%-----------------------------------------------------------------------------
% mat_muda_linha(Mat, L, N_Linha_L, N_Mat):
% N_Mat e' o resultado de substituir a linha L de Mat
% por N_Linha_L.
%-----------------------------------------------------------------------------
mat_muda_linha([_|T], 1, N_Linha_L, [N_Linha_L|T]) :- !.

mat_muda_linha([H|T], L, N_Linha_L, [H|R]):-
    L > 0,
    NL is L-1,
    mat_muda_linha(T, NL, N_Linha_L, R), !.


