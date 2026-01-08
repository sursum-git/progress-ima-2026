/*******************************************************
programa: esbo/boExpDadosPostGresql.p
objetivo: extrair os dados conforme parametros
e criar comandos de insert a serem executados
posteriormente
data:10/2024
autor:tadeu silva parreiras
******************************************************/
{esbo/boMetaDados.i}

DEFINE VARIABLE hBoMsg AS HANDLE      NO-UNDO.
DEFINE TEMP-TABLE ttTabela
    FIELD banco    AS CHAR
    FIELD tabela   AS CHAR
    FIELD condicao AS CHAR
    FIELD campos   AS CHAR
    .
DEFINE VARIABLE cNomeArquivo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iQtRegs      AS INTEGER     NO-UNDO.


    
FUNCTION convDado RETURNS CHAR(dado AS CHAR, tipo AS CHAR):

    //todo

END FUNCTION.


PROCEDURE iniciar:

    //colocar a bo de msg
    RUN esbo/boMsg.p PERSIST SET hBoMsg.


END.

PROCEDURE finalizar:

    IF VALID-HANDLE(hBoMSg) THEN DO:
       DELETE PROCEDURE hBoMsg.
    END.

    DELETE PROCEDURE THIS-PROCEDURE .

END PROCEDURE.


PROCEDURE incluirTabela:

    DEFINE INPUT  PARAMETER pBanco      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pTabela     AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCondicao   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCampo      AS CHARACTER   NO-UNDO.

    CREATE ttTabela.
    ASSIGN ttTabela.banco       = pBanco
           ttTabela.tabela      = pTabela
           ttTabela.condicao    = pCondicao
           ttTabela.campos      = pCampo
           .



END PROCEDURE.




PROCEDURE setNomeArq:

    DEFINE INPUT  PARAMETER pNomeArquivo AS CHARACTER   NO-UNDO.
    ASSIGN cNomeArquivo = pNomeArquivo.


END PROCEDURE.


PROCEDURE setQtRegsPorInsert:

    DEFINE INPUT  PARAMETER pQt AS INTEGER     NO-UNDO.
    ASSIGN iQtRegs = pQt.

END PROCEDURE.

PROCEDURE sethBoMsg:

    DEFINE INPUT  PARAMETER pH AS HANDLE      NO-UNDO.

    ASSIGN hBoMsg = pH .


END PROCEDURE.


PROCEDURE exec:

    

    FOR each ttCampos:
       DISP ttCampos WITH 1 COL WIDTH 550.
    END.



END PROCEDURE.


PROCEDURE _getPropsCpsTabela:

    DEFINE INPUT  PARAMETER pBanco  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pTabela AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCampos AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE hBoMd AS HANDLE      NO-UNDO.

    RUN esbo/boMetaDados.p PERSIST SET hBoMd.

    RUN setBanco    IN hBoMd(pBanco).
    RUN setTabela   IN hBoMd(pTabela).
    RUN setCampos   IN hBoMd(pCampos).
    RUN getCpsTb    IN hBoMd.
    RUN getTTCps    IN hBoMd(OUTPUT TABLE ttCampos).

END PROCEDURE.







