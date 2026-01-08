/*
programa: esbo/boTransacoes.p
Objetivo: Manter a tabela de transa‡äes a ser utilizada por diversos programas
data: 10/2021
*/
DEFINE NEW GLOBAL SHARED VAR transGlobCorrente AS INT64 .

DEFINE VARIABLE idTransacao AS int64     NO-UNDO.
DEFINE VARIABLE cChave      LIKE transacoes.chave NO-UNDO.
DEFINE VARIABLE codPrograma AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLogin      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCalculo    AS INT64     NO-UNDO.
{utp/ut-glob.i}
{esp/util.i}
DEFINE TEMP-TABLE ttTrans LIKE transacoes.

PROCEDURE setChave:
 DEFINE INPUT  PARAMETER pChave LIKE transacoes.chave   NO-UNDO.
 ASSIGN cChave = pChave.

 /*
    IF c-seg-usuario = 'super' THEN
    MESSAGE idTransacao
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

 IF idTransacao <> 0 THEN DO:
    FIND transacoes
         WHERE transacoes.transacao_id = idTransacao
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL transacoes THEN DO:
       ASSIGN transacoes.chave = pChave.
       FIND CURRENT transacoes NO-LOCK NO-ERROR.
    END.

 END.
END PROCEDURE.

PROCEDURE setCodPrograma:
 DEFINE INPUT  PARAMETER pProg LIKE transacoes.cod_programa   NO-UNDO.
 ASSIGN codPrograma = pProg.
 FIND transacoes
      WHERE transacoes.transacao_id = idTransacao
      EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL transacoes THEN DO:
       ASSIGN transacoes.cod_programa = pProg.
       FIND CURRENT transacoes NO-LOCK NO-ERROR.
    END.

END PROCEDURE.


PROCEDURE setLogin:

    DEFINE INPUT  PARAMETER pLogin AS CHARACTER   NO-UNDO.
    ASSIGN cLogin = pLogin .

END PROCEDURE.

PROCEDURE setCalculo:

    DEFINE INPUT  PARAMETER pCalculo AS INT64     NO-UNDO.
    ASSIGN iCalculo = pCalculo.

END PROCEDURE.


PROCEDURE iniciarTransacao:

    CREATE transacoes.
    ASSIGN transacoes.transacao_id  = NEXT-VALUE(seq_transacao)
           transacoes.dt_hr_ini     = NOW 
           transacoes.login         = IF cLogin <> '' THEN cLogin ELSE c-seg-usuario
           transacoes.chave         = cChave
           transacoes.cod_programa  = codPrograma 
           idTransacao              = transacoes.transacao_id 
           transGlobCorrente        = idTransacao
           transacoes.calculo_id    = iCalculo.


END PROCEDURE.

PROCEDURE finalizarTransacao:
    DEFINE INPUT  PARAMETER iSit AS INTEGER     NO-UNDO.
    /*IF c-seg-usuario = 'super' THEN
    MESSAGE idTransacao
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    FIND transacoes EXCLUSIVE-LOCK
        WHERE transacoes.transacao_id = idTransacao NO-ERROR.
    IF AVAIL transacoes THEN
       ASSIGN transacoes.dt_hr_fim = NOW
              transacoes.ind_sit_transacao = iSit
              idTransacao          = 0
              transGlobCorrente    = 0.
    FIND CURRENT transacoes NO-LOCK NO-ERROR.
END PROCEDURE.


PROCEDURE getIDTransCorrente:

    DEFINE OUTPUT PARAMETER iTrans AS INTEGER     NO-UNDO.
    ASSIGN iTrans = idTransacao.

END PROCEDURE.

PROCEDURE getDadosTransPorId:
    DEFINE INPUT  PARAMETER idTrans   AS INT64     NO-UNDO.
    DEFINE OUTPUT PARAMETER dtHrIni   AS DATETIME                    NO-UNDO.
    DEFINE OUTPUT PARAMETER dtHrFim   AS DATETIME                    NO-UNDO.
    DEFINE OUTPUT PARAMETER cPrograma LIKE transacoes.cod_programa   NO-UNDO.
    DEFINE OUTPUT PARAMETER cChave    LIKE transacoes.chave          NO-UNDO.
    DEFINE OUTPUT PARAMETER cLogin    LIKE transacoes.login          NO-UNDO.
    DEFINE OUTPUT PARAMETER iSit      AS INTEGER     NO-UNDO.

    FIND transacoes NO-LOCK
        WHERE transacoes.transacao_id = idTransacao
        NO-ERROR.
    IF AVAIL transacoes THEN DO:
       ASSIGN dtHrIni       = transacoes.dt_hr_ini
              dtHrFim       = transacoes.dt_hr_fim
              cPrograma     = transacoes.cod_programa
              cChave        = transacoes.chave
              cLogin        = transacoes.login
              iSit          = transacoes.ind_sit_transacao .
    END.



END PROCEDURE.

PROCEDURE getTtTransacaoCorrente:
    DEFINE INPUT  PARAMETER idTrans AS INT64     NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR  ttTrans .
    
    EMPTY TEMP-TABLE ttTrans.
    FIND transacoes NO-LOCK
        WHERE transacoes.transacao_id = idTrans
        NO-ERROR.
    IF AVAIL transacoes THEN DO:
        CREATE ttTrans.
        BUFFER-COPY transacoes TO ttTrans.
    END.




END PROCEDURE.

PROCEDURE getUltIDPorPrograma.
    
    DEFINE OUTPUT PARAMETER iTransacao LIKE transacoes.transacao_id  NO-UNDO.
    FIND LAST transacoes
        WHERE transacoes.cod_programa = codPrograma
        AND   transacoes.ind_sit_transacao = 1 // concluida
        USE-INDEX ind_prog_transacao NO-LOCK NO-ERROR.


END PROCEDURE.

PROCEDURE getUltIDporChave.

    DEFINE OUTPUT PARAMETER pTransacao LIKE transacoes.transacao_id  NO-UNDO.
    
    FIND LAST transacoes
        WHERE transacoes.chave = cChave
        AND   transacoes.ind_sit_transacao = 1 // concluida
         USE-INDEX ind_chave NO-LOCK NO-ERROR.
    IF AVAIL transacoes THEN
        ASSIGN pTransacao = transacoes.transacao_id .
END PROCEDURE.


PROCEDURE getIdsPorChave:
    DEFINE INPUT  PARAMETER pSitTransacao AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER cListaIds     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE sitIni AS INTEGER     NO-UNDO INIT 0.
    DEFINE VARIABLE sitFim AS INTEGER     NO-UNDO INIT 99.
    
    IF pSitTransacao > 0 THEN
       ASSIGN sitIni = pSitTransacao
              sitFim = pSitTransacao .

    FOR EACH  transacoes
        WHERE transacoes.chave = cChave
        AND   transacoes.ind_sit_transacao >= sitIni
        AND   transacoes.ind_sit_transacao <= sitFim
        USE-INDEX ind_chave NO-LOCK.
        RUN incrValor(INPUT-OUTPUT cListaIds, transacoes.transacao_id,",").
    END.  
    
END PROCEDURE.

PROCEDURE gerarTransacao:
    /*gera uma nova transa‡Æo com base em parametros*/
    DEFINE INPUT  PARAMETER pCodPrograma AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCodUsuario  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pNumCalculo  AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pChave       AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER idTrans      AS INT64       NO-UNDO.
    DEFINE VARIABLE cSufixo AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE hBoCalculos AS HANDLE      NO-UNDO.

    RUN esbo/boCalculos.p PERSIST SET hBoCalculos.
    /*RUN SETCalculo IN hBoCalculos(pNumCalculo).
    RUN getSufixo IN hBoCalculos(OUTPUT cSufixo).*/
    RUN setChave(pChave).
    RUN setCalculo(pNumCalculo).
    RUN setCodPrograma(pCodPrograma).
    IF pCodUsuario = '' THEN
       ASSIGN cLogin = c-seg-usuario.
    RUN setLogin(cLogin).
    RUN iniciarTransacao.
    DELETE PROCEDURE hBoCalculos.
    RUN getIDTransCorrente(OUTPUT idTrans).


   



END PROCEDURE.








