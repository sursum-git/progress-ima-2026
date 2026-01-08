                                                                            
{esp/utilAPi.i}
{utp/ut-api-action.i    piPrinc  GET ~*}
{utp/ut-api-notfound.i}

//paramatros acao log_todos_limites log_limite_atual
DEFINE TEMP-TABLE tt LIKE hist_limit_cred_cli.
PROCEDURE piPrinc:

    DEF INPUT PARAM jsonInput         AS JsonObject NO-UNDO.
    DEF OUTPUT PARAM jsonOutput       AS JsonObject NO-UNDO. 

    DEFINE VARIABLE cRetorno          AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cIndLimite        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cCliente          AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE codUsuario        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cDtValidade       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE hBoHistLimCredCli AS HANDLE      NO-UNDO.
    DEFINE VARIABLE cVlCredito        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cHistorico        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE deCredito         AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE idCorrente        AS INTEGER     NO-UNDO.
    DEFINE VARIABLE hRetorno          AS HANDLE      NO-UNDO.
    jsonOutput = NEW JsonObject().

    RUN setJsonInput(JsonInput).
    RUN getQueryParam('ind_limite',OUTPUT cIndLimite).
    RUN getQueryParam('cod_emitente',OUTPUT cCliente).
    RUN getQueryParam('cod_usuario',OUTPUT codUsuario).
    RUN getQueryParam('dt_validade',OUTPUT cDtValidade).
    RUN getQueryParam('vl_credito',OUTPUT cVlCredito).
    RUN getQueryParam('historico',OUTPUT chistorico).

    RUN esbo/boHistLimCredCli.p PERSISTENT SET hBoHistLimCredCli.
    RUN setProp IN hBoHistLimCredCli('cod_emitente',cCliente).


    CASE cIndLimite:
        WHEN 'incluir' THEN DO:
            RUN setProp IN hBoHistLimCredCli('cod_usuario',codUsuario).
            RUN setProp IN hBoHistLimCredCli('dt_validade',cDtValidade).
            RUN setProp IN hBoHistLimCredCli('vl_credito',cVlCredito).
            RUN setProp IN hBoHistLimCredCli('historico',cHistorico ).
            RUN incluirLimite IN hBoHistLimCredCli.
            RUN getIdCorrente IN hBoHistLimCredCli(OUTPUT idCorrente).
            RUN criarTTChave('id',STRING(idCorrente)).
            ASSIGN hRetorno = TEMP-TABLE ttChave:HANDLE.


        END.
        WHEN 'limite_atual' THEN DO:
           RUN getLimiteAtual IN hBoHistLimCredCli(OUTPUT deCredito).
           RUN criarTTChave('vl_limite',STRING(deCredito)).
           ASSIGN hRetorno = TEMP-TABLE ttChave:HANDLE.

        END.

        WHEN 'todos_limites' THEN DO:
            RUN getLimitesCliente IN hBoHistLimCredCli(OUTPUT TABLE tt ).
            ASSIGN hRetorno = TEMP-TABLE tt:HANDLE.
        END.


    END CASE.
   RUN retJsontt(INPUT-OUTPUT jsonOutput, hRetorno ).
/*
    RUN criarTTRetorno('chave:' + cChave + " - tipo:" + cRetorno,1).
    ASSIGN iCalculo = INT(cRetorno).
    IF cChave <> '' AND iCalculo <> 0 THEN DO:
        RUN esbo/boLogsCalculos.p PERSISTENT SET hBoLogsCalculos.
        RUN iniciarBos IN hBoLogsCalculos.
        RUN setCalculo IN hBoLogsCalculos(iCalculo).
        RUN getLogsUltTransPorChave IN hBoLogsCalculos(cChave, OUTPUT TABLE ttTrans, OUTPUT TABLE ttLog).
        RUN finalizarBos IN hBoLogsCalculos.
        IF VALID-HANDLE(hBoLogsCalculos) THEN
           DELETE PROCEDURE hBoLogsCalculos.
        RUN retJsontt(INPUT-OUTPUT jsonOutput, TEMP-TABLE ttLog:HANDLE ).
    END.
    ELSE  DO:
        RUN criarTTRetorno('‚ obrigatorio passar a chave e o tipo',1).
        RUN retJsontt(INPUT-OUTPUT jsonOutput, TEMP-TABLE ttRetorno:HANDLE ).
    END.
    
    //RUN retJsontt(INPUT-OUTPUT jsonOutput, TEMP-TABLE ttRetorno:HANDLE ).


   */


END PROCEDURE.









