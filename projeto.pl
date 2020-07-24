% Nome : Tomas de Araujo Tavares        Numero : 95680

% Importar Codigo comum
:- [codigo_comum].


%       -------- : Funcoes Auxiliares : --------        % 

% -> Auxiliar de espacos_com_posicoes_comuns
% elemento_comum/2 - (Lista1, Lista2)
%   Testa se a Lista1 e a Lista2 tem pelo menos
%   um elemento em comum.

elemento_comum(Lst1, Lst2) :-
    \+ elemento_nao_comum(Lst1, Lst2, Lst2).

% Caso Terminal : Caso tenha verificado todos os elementos
% e nenhum seja comum as duas listas
elemento_nao_comum([], _, _).

% Para todos os elementos da Lst2 foi testada a sua
% igualdade com o primeiro elemento da Lst fazer o mesmo
% para o resto dos elementos da Lst1
elemento_nao_comum([_ | R], [], Lst2) :-
    elemento_nao_comum(R, Lst2, Lst2).

elemento_nao_comum([P | R], [P2 | R2], Lst2) :-
    P \== P2,
    elemento_nao_comum([P | R], R2, Lst2).



% -> Auxiliar de palavra_possivel_esp
% verifica_espacos/2 - (Espacos_Comuns, Letras):
%   Verifica se da para inserir por pelo menos uma das 
%   letras[Letras] nos em cada espaco dos [Espacos_Comuns]. 

verifica_espacos([], _).

verifica_espacos([Esp | R], Letras) :- 
    verifica_espaco(Esp, Letras),
    verifica_espacos(R, Letras).


% -> Auxiliar de palavra_possivel_esp
% verifica_espaco/2 - (Espaco, Letras):
%   Verifica se alguma das letras da lista [Letras]
%   unifica com o espaco dado

% Caso Terminal : A lista de palavras esta vazia logo nehuma palavra
%                 unifica com o espaco dado [false.]
verifica_espaco(_, []) :- fail.

% Caso Terminal : Uma das palavras e unificavel com o espco [true.]
verifica_espaco(Espaco, [Letra | _]) :-
    unifiable(Espaco, Letra, _).

verifica_espaco(Espaco, [_ | R]) :-
    verifica_espaco(Espaco, R).


% -> Auxiliar de letras_comuns
% letra_igual/2 - (Letras, Posicao, Letra):
%   Dada uma lista de letras de palavras uma [Posicao]
%   e uma letra[Letra] verifica para cada palavra na lista
%   se tem a letra[Letra] nao posicao[Posicao].

% Caso Terminal : Nao ha mais palavras para verificar
letra_igual([], _, _).

letra_igual([Palavra | R], Posicao, Letra) :-
    nth1(Posicao, Palavra, Letra_Palavra),
    Letra == Letra_Palavra,
    letra_igual(R, Posicao, Letra).

% -> Auxiliar de atribui_comuns
% substitui_letras_espaco/2 - (Letras, Espaco):
%   Dado um conjunto de letras e as suas posicoes[Letras]
%   substitui no espaco a respetiva letra na sua posicao 
%   respetiva

% Caso Terminal : Todas as letras foram unificadas nas respetivas
%                  posicoes do espaco
substitui_letras_espaco([], _).

substitui_letras_espaco([(Posicao, Letra) | R], Espaco) :-
    nth1(Posicao, Espaco, Local_Letra),
    Local_Letra = Letra,
    substitui_letras_espaco(R, Espaco).

% -> Auxiliar de retira_impossiveis
% letras_unificaveis/3 - (Espaco, Letras, Letras_Unificaveis):
%   Recebe um espaco e uma lista de letras de palavras[Letras] e 
%   devolve em uma lista[Letras_Unificaveis] que apenas contem
%   as palavras que unificam com o espaco dado 

letras_unificaveis(Espaco, Letras, Letras_Unificaveis) :-
    letras_unificaveis(Espaco, Letras, [], Letras_Unificaveis). 

% Caso Terminal : Nao a mais letras para testar se unificam com o espaco
%                 e devolve aquelas que unificam
letras_unificaveis(_, [], Resultado, Resultado).

letras_unificaveis(Espaco, [Palavra | R], Lst_Acumula, Letras_Unificaveis) :-
    unifiable(Espaco, Palavra, _),
    append(Lst_Acumula, [Palavra], Lst_Acumula_),
    letras_unificaveis(Espaco, R, Lst_Acumula_, Letras_Unificaveis).

% A palavra nao unifica nao adicionar a lista final
letras_unificaveis(Espaco, [_ | R], Lst_Acumula, Letras_Unificaveis) :-
    letras_unificaveis(Espaco, R, Lst_Acumula, Letras_Unificaveis). 


% - Auxiliar de retira_unicas
% remove_palavras_unicas/3 - (Palavras, Unicas, Palavras_Sem_Unicas):
%   Remove todas as palavras que pertencem simultaneamente as duas listas
%   [Palavras] e [Unicas].

remove_palavras_unicas(Palavras, Unicas, Palavras_Sem_Unicas) :-
    findall(Palavra_Nao_Unica, 
        (member(Palavra_Nao_Unica, Palavras), not(member(Palavra_Nao_Unica, Unicas))),
        Palavras_Sem_Unicas).

% -> Auxiliar escolhe_menos_alternativas
% uma_palavra/1 - (Espaco_e_Palavras):
%   Verifica se a lista dada tem 1 e uma so palavra

uma_palavra([_, Palavras]) :- 
    length(Palavras, Len_Palavras),
    Len_Palavras == 1.

%         --------------------------------------     %   


%       -------- : Funcoes Pedidas : --------        % 

% obtem_letras_palavras/2 - (Lst_Palavras, Letras):
%   Predicado que recebe uma lista com varias palavras[Lst_Palavras] e 
%   transforma-a em uma lista de varias listas[Letras] onde os seus 
%   elementos sao os caracteres de cada palavra.

obtem_letras_palavras(Lst_Palavras, Letras) :-
    sort(Lst_Palavras, Lst_Palavras_Ord),
    obtem_letras_palavras(Lst_Palavras_Ord, [], Letras).


obtem_letras_palavras([], Lst, Letras) :-
    % Inverte a lista final ja que os elementos vao estar invertidos 
    reverse(Lst, Letras).


obtem_letras_palavras([P | R], Lst, Letras) :-
    % Obter a lista com os caracteres do primeiro elemento da
    % lista de palavras
    atom_chars(P, Lst_Chars),

    % Adicionar a lista de caracteres ao Letras e fazer o 
    % mesmo para o resto da lista de palavras
    obtem_letras_palavras(R, [Lst_Chars | Lst], Letras). 
    


% espaco_fila/2 - (Fila, Espaco):
%   Predicado que recebe uma lista[Fila] que corresponde a uma
%   linha ou coluna e devolve o espaco[Espaco] existente nessa fila,
%   sendo o espaco um espaco valido (Espaco com pelo menos 3 posicoes).

espaco_fila(Fila, Espaco) :-
    espaco_fila(Fila, [], Espaco).

% Caso Terminal : Lista esta vazia
espaco_fila([], Espaco, Espaco) :-
    length(Espaco, Esp_Length),
    Esp_Length >= 3.

% Caso Terminal : Escontrou um asterisco
espaco_fila([P | _], Lst, Lst) :-
    P == # ,
    length(Lst, Lst_Length),
    Lst_Length >= 3.

% Se a o tamanho da lista for menor que 3 reiniciar o acumulador
espaco_fila([P | R], _, Espaco) :-
    P == # ,
    espaco_fila(R, [], Espaco).

espaco_fila([P | R], Lst, Espaco) :-
    P \== #,
    append(Lst, [P], Lst_),
    espaco_fila(R, Lst_, Espaco).



% espacos_fila/2 - (Fila, Espacos):
%   Recebe uma lista[Fila] e obtem todos os espacos dessa
%   lista guardando-os em outra lista[Espacos]

espacos_fila(Fila, Espacos) :- 
    bagof(Espaco, espaco_fila(Fila, Espaco), Espacos),
    !.

% Se nao houver nenhum espaco valido na fila em vez
% de retornar falso devolve a lista vazia
espacos_fila(_, []).



% espacos_puzzle/2 - (Grelha, Espacos):
%   Dado a grelha do puzzle e devolve todos os espacos validos
%   de todas as linhas e de todas as colunas


espacos_puzzle(Grelha, Espacos) :-
    % Obtem todos os espacos possiveis para as linhas
    espacos_puzzle(Grelha, [], Espacos_Linhas),

    % Transpoe a matriz para que agora as colunhas sejam linhas
    mat_transposta(Grelha, Transposta),

    % Obtem todos os espacos possiveis para as colunas
    espacos_puzzle(Transposta, [], Espacos_Colunas),

    % Junta todos os espacos possiveis as colunas e das linhas
    % no resultado final[Espacos]
    append(Espacos_Linhas, Espacos_Colunas, Espacos).

% Caso Terminal : Nao ha mais linhas para obter espacos
espacos_puzzle([], Lst_Acumula, Lst_Acumula).

% Percorre todas as linhas da Grelha obtendo os seus espacos 
% possiveis juntando os a um acumulador
espacos_puzzle([Fila | R], Lst_Acumula, Espaco) :-
    espacos_fila(Fila, Espacos),
    append(Lst_Acumula, Espacos, Lst_Acumula_),
    espacos_puzzle(R, Lst_Acumula_, Espaco).



% espacos_com_posicoes_comuns/3 - (Espacos, Espaco, Espacos_Comuns)
%   Rerebe uma lista com espacos[Espacos] e um espaco[Espaco]
%   e devolve a lista[Espacos_comuns] que e a lista de espacos
%   que tem uma posicao em comum com o espaco[Espaco]

espacos_com_posicoes_comuns(Espacos, Espaco, Espacos_Comuns) :-
    espacos_com_posicoes_comuns(Espacos, Espaco, [],Espacos_Comuns).

% Caso Terminal : Todos os espacos ja foram vistos
espacos_com_posicoes_comuns([], _, Resultado, Resultado).

% Se o primeiro elemento da lista tem um elemento em comum com o espaco
% adicionar o primeiro elemento da a lista acumuladora 
espacos_com_posicoes_comuns([P | R], Espaco, Lst_Acumula, Espacos_Comuns) :-
    % Verificar se o primeiro elemento e o espaco sao diferentes
    P \== Espaco,
    elemento_comum(P, Espaco),
    append(Lst_Acumula, [P], Lst_Acumula_),
    espacos_com_posicoes_comuns(R, Espaco, Lst_Acumula_, Espacos_Comuns),
    !.

% Se o primeiro elemento nao tiver elementos em comum com o espaco continuar
% a verificar o resto dos espacos
espacos_com_posicoes_comuns([_ | R], Espaco, Lst_Acumula, Espacos_Comuns) :-
    espacos_com_posicoes_comuns(R, Espaco, Lst_Acumula, Espacos_Comuns). 



% palavra_possivel_esp/4 - (Palavra, Espaco, Espacos, Letras):
%   Dada uma lista de letras de uma palavra[Palavra] e verifica se a 
%   e possivel inserir a palavra nesse espaco tendo em conta:
%       - Os tamanhos sao iguais [Palavra e Espaco]
%       - Se o espaco ja conter alguma letra ja especificada a palavra
%         deve ter a mesma letra na mesma posicao
%       - Verificar se ao por aquela palavra ali condiciona o uso de 
%         qualquer outra palavra


palavra_possivel_esp(Palavra, Espaco, Espacos, Letras) :-
    Espaco = Palavra,
    espacos_com_posicoes_comuns(Espacos, Espaco, Espacos_Comuns),
    verifica_espacos(Espacos_Comuns, Letras),
    !.


% palavras_possiveis_esp/4 - (Letras, Espacos, Esapco, Palavras_Possiveis):
%   Dada uma lista de todas as palavras[Letras] e todos os espacos de uma 
%   grelha devolve uma lista de todas as palavras[Palavras_Possiveis] que
%   e possivel inserir no espaco[Espaco]


palavras_possiveis_esp(Letras, Espacos, Espaco, Palavras_Possiveis) :-
    findall(Palavra, 
        (member(Palavra, Letras), 
        palavra_possivel_esp(Palavra, Espaco, Espacos, Letras)),
        Palavras_Possiveis).


% palavras_possiveis/3 - (Letras, Espacos, Palavras_Possiveis):
%   Dados todos os espacos[Espacos] possiveis de por palavras
%   e uma lista de todas as palavras possiveis[Letras] devolve
%   uma lista[Palavras_Possiveis] de listas em que cada lista
%   primeiro elemento e o espaco e o segundo sao todas as palavras
%   possiveis para aquele espaco


palavras_possiveis(Letras, Espacos, Palavras_Possiveis) :-
    palavras_possiveis(Letras, Espacos, Espacos, [], Palavras_Possiveis).

% Caso Terminal : Caso nao haja mais espacos para verificar
palavras_possiveis(_, _, [], Resultado, Resultado).

palavras_possiveis(Letras, Espacos_Fixos, [Espaco | R], Lst_Acumula, Palavras_Possiveis) :-
    palavras_possiveis_esp(Letras, Espacos_Fixos, Espaco, Palavras_Espaco),
    append(Lst_Acumula, [[Espaco, Palavras_Espaco]], Lst_Acumula_),
    palavras_possiveis(Letras, Espacos_Fixos, R, Lst_Acumula_, Palavras_Possiveis),
    !.



% letras_comuns/2 - (Letras, Letras_comuns):
%   Dada uma lista de letras de palavras[Letras] e
%   devolve uma lista com as letras comuns a todas as
%   palavras em [Letras] na forma (posicao, letra)

letras_comuns([Palavra | R], Letras_comuns) :-
    length(Palavra, Posicao),
    letras_comuns([Palavra | R], Posicao, [], Letras_comuns),
    !. 

% Caso Terminal : Caso Ja tenha visto todas as posicoes das palavras
letras_comuns(_, 0, Resultado, Resultado).

letras_comuns([Palavra | R], Posicao, Lst_Acumula, Letras_comuns) :-
    nth1(Posicao, Palavra, Letra),
    letra_igual(R, Posicao, Letra),

    % Atualizar a posicao de comparacao
    Posicao_Menos1 is Posicao - 1,
    letras_comuns([Palavra | R], Posicao_Menos1, [(Posicao, Letra) | Lst_Acumula], Letras_comuns).

% Na posicao atual as letras sao diferentes em diferentes palavras passar 
% para a proxima posicao
letras_comuns(Letras, Len_Palavra, Lst_Acumula, Letras_comuns) :-
    Len_Palavra_ is Len_Palavra - 1,
    letras_comuns(Letras, Len_Palavra_, Lst_Acumula, Letras_comuns).


% atribui_comuns/1 - (Palavras_Possiveis):
%   Dado uma lista com os espacos e as palavras possiveis para esse
%   espaco atribui no espaco as letras que:

atribui_comuns([]).

atribui_comuns([Espaco_E_Palavras | R]) :-
    nth1(1, Espaco_E_Palavras, Espaco),
    nth1(2, Espaco_E_Palavras, Palavras),

    % Obter as letras comuns a todas as Palavras 
    letras_comuns(Palavras, Letras_Comuns),

    % Substituir no espaco todas as letras comuns
    substitui_letras_espaco(Letras_Comuns, Espaco),
    atribui_comuns(R).



% retira_impossiveis/2 - (Palavras_Possiveis, Novas_Palavras_Possiveis):
%   Dada uma lista de [Palavras_Possiveis] obtida do perdicado 
%   atribui_comuns este perdicado remove todas as palavras que
%   nao unifiquem com os novos espacos.retira_impossiveis

retira_impossiveis(Palavras_Possiveis, Novas_Palavras_Possiveis) :-
    retira_impossiveis(Palavras_Possiveis, [], Novas_Palavras_Possiveis),
    !.

% Caso Terminal : As linhas de espacos e palavras acabram devolve
% as mesmas linhas so que com as palavras que nao unificam com o
% espaco removidas


retira_impossiveis([], Resultado, Resultado).

retira_impossiveis([[Espaco, Letras] | R], Lst_Acumula, Novas_Palavras_Possiveis) :-
    letras_unificaveis(Espaco, Letras, Letras_Unificaveis),
    append(Lst_Acumula, [[Espaco, Letras_Unificaveis]], Lst_Acumula_),
    retira_impossiveis(R, Lst_Acumula_, Novas_Palavras_Possiveis).


% obtem_unicas/2 - (Palavras_possiveis, Unicas):
%   Dada uma lista de palavras possiveis[Palavras_possiveis], 
%   obtida do perdicado atribui_comuns e retira_impossiveis
%   e devolve uma lista[Unicas] que contem todas as palavras
%   que so podem ser aquelas para um dado espaco


obtem_unicas(Palavras_Possiveis, Unicas) :-
    obtem_unicas(Palavras_Possiveis, [], Unicas),
    !.

% Caso Terminal : Todos os espacos e posicoes foram verificados logo
% deve ser retornada a lista de palavras unicas
obtem_unicas([], Resultado, Resultado).

obtem_unicas([[_, Palavras] | R], Lst_Acumula, Unicas) :-
    length(Palavras, Num_Palavras),
    Num_Palavras == 1,
    append(Lst_Acumula, Palavras, Lst_Acumula_), 
    obtem_unicas(R, Lst_Acumula_, Unicas).

% O numero de Palavras possiveis para o espaco nao e so uma por isso
% nao adiciona a lista de palavras unicas
obtem_unicas([_ | R], Lst_Acumula, Unicas) :-
    obtem_unicas(R, Lst_Acumula, Unicas). 


% retira_unicas/2 - (Palavras_Possiveis, Novas_Palavras_Possiveis):
%   Dada uma lista de palavras possiveis[Palavras_Possiveis] este 
%   perdicado altera esta lista de forma a retirar todas as palavras
%   que sao unicas possiveis para um certo espaco

retira_unicas(Palavras_Possiveis, Novas_Palavras_Possiveis) :-
    obtem_unicas(Palavras_Possiveis, Unicas),
    retira_unicas(Palavras_Possiveis, Unicas, [], Novas_Palavras_Possiveis),
    !.

% Caso Terminal : De todos os espacos e posicoes foram retiradas as palavras
% que sao unicas a outro espaco e devolve a nova lista de palavras atualizada
retira_unicas([], _, Resultado, Resultado).

retira_unicas([[Espaco, Palavras] | R], Unicas, Lst_Acumula, Novas_Palavras_Possiveis) :-
    % Verificar se a palavra ja nao e unica
    length(Palavras, Num_Palavras),
    Num_Palavras > 1,
    remove_palavras_unicas(Palavras, Unicas , Palavras_Sem_Unicas),
    append(Lst_Acumula, [[Espaco, Palavras_Sem_Unicas]], Lst_Acumula_), 
    retira_unicas(R , Unicas, Lst_Acumula_, Novas_Palavras_Possiveis).

% O primeiro elemento tem como palavra uma palavra unica logo nao e alterado
retira_unicas([[Espaco, Palavras] | R], Unicas, Lst_Acumula, Novas_Palavras_Possiveis) :-
    append(Lst_Acumula, [[Espaco, Palavras]], Lst_Acumula_),
    retira_unicas(R, Unicas, Lst_Acumula_, Novas_Palavras_Possiveis). 



% simplifica/2 - (Palavras_Possiveis, Novas_Palavras_Possiveis):
%   E a aplicacao dos perdicados atibui_comuns, retira_impossiveis e
%   retira_unicas sucessivamente ate que nao existam mais alteracoes

simplifica(Palavras_Possiveis, Novas_Palavras_Possiveis) :-
    simplifica(Palavras_Possiveis, Palavras_Possiveis, Novas_Palavras_Possiveis). 

simplifica(Palavras_Possiveis, Palavras_Possiveis_Ant, Novas_Palavras_Possiveis) :-
    atribui_comuns(Palavras_Possiveis),
    retira_impossiveis(Palavras_Possiveis, Palavras_Possiveis_),
    retira_unicas(Palavras_Possiveis_, Palavras_Possiveis__),
    Palavras_Possiveis__ \== Palavras_Possiveis_Ant,

    % Se a lista anterior diferente da atual continuar a chamar o 
    %perdicado ate que sejam iguais
    simplifica(Palavras_Possiveis__, Palavras_Possiveis__, Novas_Palavras_Possiveis),
    !.

    
% Caso Terminal : Quando nao a alteracoes entre o lista anterior a aplicacao
% dos perdicados e a seguinte deve retornar essa lista
simplifica(Palavras_Possiveis, Palavras_Possiveis, Palavras_Possiveis) :- 
    atribui_comuns(Palavras_Possiveis).


% inicializa/2 - (Puzzle, Palavras_Possiveis):
%   Recebe uma lista de listas[Puzzle] e aplica os perdicados
%   feitos ate aqui ate ter as listas simplificadas

inicializa([Lst_Palavras, Grelha], Palavras_Possiveis) :-
    obtem_letras_palavras(Lst_Palavras, Letras),
    espacos_puzzle(Grelha, Espacos),
    palavras_possiveis(Letras, Espacos, Pals_Possiveis),
    simplifica(Pals_Possiveis, Palavras_Possiveis).



% escolhe_menos_alternativas/2 - (Palavras_Possiveis, Escolha):
%   Escolhe o espaco que tem menos palavras que unificam tirando
%   aqueles que so tem uma palavra

escolhe_menos_alternativas(Palavras_Possiveis, Escolha) :-
    % Remove todos os elementos que so contem 1 palavra 
    exclude(uma_palavra, Palavras_Possiveis, Palavras_Possiveis_),

    % Verifica se a lista nao e vazia(todos os elementos so tinham 1 palavra)
    Palavras_Possiveis_ \== [],
    nth1(1, Palavras_Possiveis_, Espaco_e_Palavra),
    escolhe_menos_alternativas(Palavras_Possiveis_, Espaco_e_Palavra, Escolha),
    !.


% Caso Terminal : A lista de Palavras_Possiveis acabou
escolhe_menos_alternativas([], Resultado, Resultado).

escolhe_menos_alternativas([[Espaco, Palavras] | Palavras_Possiveis], [_ , Palavras_Menor], Escolha) :-
    length(Palavras, Len_Palavras),
    length(Palavras_Menor, Len_Palavras_Menor),
    Len_Palavras_Menor > Len_Palavras,
    escolhe_menos_alternativas(Palavras_Possiveis, [Espaco, Palavras], Escolha).

% Caso a palavra seja do mesmo tamanho ou maior que a menor retirar essa palavra
escolhe_menos_alternativas([_ | Palavras_Possiveis], Menor, Escolha) :-
    escolhe_menos_alternativas(Palavras_Possiveis, Menor, Escolha).


% experimenta_pal/3 - (Escolha, Palavras_Possiveis, Novas_Palavras_Possiveis):
%   Dada uma escolha[Escolha] que e constituida por um espaco e pelas palavras
%   possiveis para esse espaco e escolhe uma palavra dessa lista unificando a
%   com o espaco 

experimenta_pal([Espaco, Palavras], Palavras_Possiveis, Novas_Palavras_Possiveis) :-
    member(Palavra, Palavras), 
    experimenta_pal(Palavra, [Espaco, Palavras], Palavras_Possiveis, [], Novas_Palavras_Possiveis).

% Caso Terminal : Caso nao haja mais linhas para percorrer
experimenta_pal(_, _, [], Resultado, Resultado).

experimenta_pal(Palavra, Escolha, [Espaco_e_Palavra | R], Lst_Acumula, Novas_Palavras_Possiveis) :-
    Escolha == Espaco_e_Palavra, !, 
    nth1(1, Escolha, Espaco),

    % Unificar o espaco com a palavra
    Espaco = Palavra,

    % Substituir o espaco por apenas pelos elementos unificados
    append(Lst_Acumula, [[Espaco, [Palavra]]], Lst_Acumula_),  
    experimenta_pal(Palavra, Escolha, R, Lst_Acumula_, Novas_Palavras_Possiveis). 

% Caso o espaco e a primera posicao das palavras possiveis nao sejam iguais basta 
% adicionar o primeiro elemento a lista final
experimenta_pal(Palavra , Escolha, [Espaco_e_Palavra | R], Lst_Acumula, Novas_Palavras_Possiveis) :-
    append(Lst_Acumula, [Espaco_e_Palavra], Lst_Acumula_),
    experimenta_pal(Palavra, Escolha, R, Lst_Acumula_, Novas_Palavras_Possiveis). 
    

% resolve_aux/2 - (Palavras_Possiveis, Novas_Palavras_Possiveis):
%   Recebe uma lista de palavras possiveis e so termina quando todos
%   os espacos so contem uma palavra possivel para esse dado espaco
    
resolve_aux(Palavras_Possiveis, Novas_Palavras_Possiveis) :-
    escolhe_menos_alternativas(Palavras_Possiveis, Escolha), !,
    experimenta_pal(Escolha, Palavras_Possiveis, Palavras_Possiveis_),
    simplifica(Palavras_Possiveis_, Palavras_Possiveis__),
    resolve_aux(Palavras_Possiveis__, Novas_Palavras_Possiveis),
    !.

% Caso Terminal : So ha espacos com 1 elemento entao escolhe_menos_alternativas
% da false entao devolve essa lista
resolve_aux(Palavras_Possiveis, Palavras_Possiveis).
    

% resolve/1 - (Puzzle):
%   perdicado que resolve o puzzle

resolve(Puzzle) :-
    inicializa(Puzzle, Palavras_Possiveis),
    resolve_aux(Palavras_Possiveis, _).

