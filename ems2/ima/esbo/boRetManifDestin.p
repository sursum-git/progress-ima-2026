/*****************************************************************************
**
**     Programa: boRetManifDestin.P
**     Data....: Agosto de 2022
**     Objetivo: Manter a tabela ret-manif-destin
**               Esta tabela controlar  a data de entrada do resumo, 
                os eventos de manifesta‡Æo  e a altera‡Æo da situa‡Æo da 
                nota fiscal, no caso de haver cancelamento
*****************************************************************************/


DEFINE VARIABLE cChaveNFe       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCodUsuario     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iNumTipo        AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDescricao      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iSituacao       AS INTEGER     NO-UNDO.
DEFINE BUFFER bf FOR ret-manif-destin .



/*
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 movto_resumo_nfe_id              int6        i
   11 cod-chave-nfe                    char        i
   20 dt_hora_registro                 datetm      i
   30 num_tipo                         inte        i
   40 cod_usuario                      char
   50 num_situacao                     inte        i
   60 qt_tentativas                    inte
   70 dt_hr_ult_busca                  datetm
   80 descricao                        char




*/

                  
PROCEDURE setChaveNfe:

    DEFINE INPUT  PARAMETER pChave AS CHARACTER   NO-UNDO.
    ASSIGN cChaveNfe = pChave.

END PROCEDURE.

PROCEDURE setCodUsuario:

    DEFINE INPUT  PARAMETER pUsuario AS CHARACTER   NO-UNDO.
    ASSIGN cCodUsuario = pUsuario.

END PROCEDURE.

PROCEDURE setNumTipo:

    DEFINE INPUT  PARAMETER pNumTipo AS INTEGER     NO-UNDO.
    ASSIGN iNumTipo = pNumTipo.

END PROCEDURE.


PROCEDURE setSituacao:

    DEFINE INPUT  PARAMETER pSituacao AS INTEGER     NO-UNDO.
    ASSIGN iSituacao = pSituacao.

END PROCEDURE.



PROCEDURE setDescricao:

    DEFINE INPUT  PARAMETER pDescricao AS CHARACTER   NO-UNDO.
    ASSIGN cDescricao = pDescricao .

END PROCEDURE.



PROCEDURE inserir:
    CREATE bf .
    ASSIGN bf.cod-chave-nfe = cChaveNfe
           bf.cod-msg  = string(iNumTipo)
           bf.des-msg  = cDescricao 
           bf.num-livre-4 = iSituacao
           bf.cod-livre-4 = cCodUsuario
           bf.dat-ret     = TODAY
           bf.hra-ret     = string(TIME,'hh:mm:ss')
           //bf.num-livre-5 = qtTentativa
        .



END PROCEDURE.


//nos procedimento abaixo sempre ‚ necessario antes chavar o setChaveNfe e setCodUsuario

PROCEDURE criarRetRecbtoResumo:

    RUN setNumTipo(9501).
    RUN setDescricao('Resumo Recebido').
    RUN setSituacao(4).
    RUN inserir .

END PROCEDURE.


PROCEDURE criarRetCiencia:

    RUN setNumTipo(9502).
    RUN setDescricao('Ciˆncia da Opera‡Æo').
    RUN setSituacao(1).
    RUN inserir .

END PROCEDURE.

PROCEDURE criarRetOperConf:

    RUN setNumTipo(9503).
    RUN setDescricao('Opera‡Æo Confirmada').
    RUN setSituacao(1).
    RUN inserir .

END PROCEDURE.

PROCEDURE criarRetNFCancelada:

    RUN setNumTipo(9504).
    RUN setDescricao('Nota Fiscal Cancelada').
    RUN setSituacao(1). //verificar se a nota fiscal foi lan‡ada j  que foi cancelada
    RUN inserir .

END PROCEDURE.


PROCEDURE criarRetOperNaoRealiz:

    RUN setNumTipo(9505).
    RUN setDescricao('Opera‡Æo NÆo Realizada').
    RUN setSituacao(1). //verificar se a nota fiscal foi lan‡ada j  que foi cancelada
    RUN inserir .

END PROCEDURE.


PROCEDURE criarRetDescOper:

    RUN setNumTipo(9506).
    RUN setDescricao('Opera‡Æo Desconhecida').
    RUN setSituacao(1). //verificar se a nota fiscal foi lan‡ada j  que foi cancelada
    RUN inserir .

END PROCEDURE.






/*
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 dat-ret                          date        im
   20 hra-ret                          char        im
   30 cod-chave-nfe                    char        im
   40 cod-msg                          char        im
   50 des-msg                          char
   60 idi-orig-solicit                 inte        m
   70 log-ativ                         logi
   80 num-seq-msg-manif                inte        im
   90 num-tip-msg                      inte
  100 cod-livre-1                      char
  110 cod-livre-2                      char
  120 cod-livre-3                      char
  130 cod-livre-4                      char
  140 cod-livre-5                      char
  150 dat-livre-1                      date
  160 dat-livre-2                      date
  170 dat-livre-3                      date
  180 dat-livre-4                      date
  190 dat-livre-5                      date
  200 val-livre-1                      deci-10
  210 val-livre-2                      deci-10
  220 val-livre-3                      deci-8
  230 val-livre-4                      deci-8
  240 val-livre-5                      deci-8
  250 log-livre-1                      logi
  260 log-livre-2                      logi
  270 log-livre-3                      logi
  280 log-livre-4                      logi
  290 log-livre-5                      logi
  300 num-livre-1                      inte
  310 num-livre-2                      inte
  320 num-livre-3                      inte
  330 num-livre-4                      inte
  340 num-livre-5                      inte


*/
