DEF VAR h-acomp       AS HANDLE NO-UNDO.

/* Variaveis do Recebimento Fiscal ---                                  */
DEF TEMP-TABLE tt-docum-est NO-UNDO LIKE docum-est 
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-item-doc-est NO-UNDO LIKE item-doc-est
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE wt-item-doc-est NO-UNDO LIKE item-doc-est
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-dupli-apagar NO-UNDO LIKE dupli-apagar
    FIELD r-rowid AS ROWID.

/* Temp-Table tt-erro Definitions ---*/
{method/dbotterr.i}

DEF VAR h-boin090      AS HANDLE.
DEF VAR h-boin176      AS HANDLE.
    
DEF VAR c-cod-estabel     LIKE estabelec.cod-estabel.
DEF VAR c-cod-estabel-ori LIKE estabelec.cod-estabel.
DEF VAR c-serie-ori       LIKE nota-fiscal.serie.
DEF VAR c-nr-nota-fis-ori LIKE nota-fiscal.nr-nota-fis.
DEF VAR c-natur-oper-ori  LIKE natur-oper.nat-operacao.

DEF TEMP-TABLE tt-itens 
    FIELD nr-sequencia AS INT
    FIELD data         AS DATE FORMAT "99/99/9999" 
    FIELD item-ori     LIKE ITEM.it-codigo
    FIELD nr-lote      AS CHAR FORMAT "x(2)"
    FIELD un-ori       LIKE ITEM.un
    FIELD qtde-ori     AS DEC
    FIELD it-codigo    LIKE ITEM.it-codigo
    FIELD un           LIKE ITEM.un
    FIELD quantidade   AS DEC
    FIELD valor        AS DEC
    FIELD vlr-tot-item AS DEC
    INDEX indice1 IS PRIMARY nr-sequencia.


    RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
    {utp/ut-liter.i Criando_Recebimento *}
    RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

    RUN pi-acompanhar IN h-acomp (INPUT "Aguarde").
 
    IF NOT VALID-HANDLE(h-boin090) OR
       h-boin090:TYPE      <> "PROCEDURE":U OR
       h-boin090:FILE-NAME <> "inbo/boin090.p":U THEN
       RUN inbo/boin090.p PERSISTENT SET h-boin090.
    
    IF NOT VALID-HANDLE(h-boin176) OR 
       h-boin176:TYPE      <> "PROCEDURE":U OR
       h-boin176:FILE-NAME <> "inbo/boin176.p":U THEN
       RUN inbo/boin176.p PERSISTENT SET h-boin176.

    EMPTY TEMP-TABLE tt-docum-est.
    EMPTY TEMP-TABLE tt-item-doc-est. 
    EMPTY TEMP-TABLE tt-dupli-apagar. 

    FIND estabelec WHERE
         estabelec.cod-estabel = c-cod-estabel-ori NO-LOCK NO-ERROR.

    FIND emitente WHERE
         emitente.cgc = estabelec.cgc NO-LOCK NO-ERROR.

    /* Cria Temp-Table para o Recebimento*/
    CREATE tt-docum-est.
    ASSIGN tt-docum-est.cod-emit = emitente.cod-emit
           tt-docum-est.serie-docto = c-serie-ori
           tt-docum-est.nro-docto = c-nr-nota-fis-ori
           tt-docum-est.nat-operacao = c-natur-oper-ori
           tt-docum-est.cod-estabel = c-cod-estabel
           tt-docum-est.declaracao-import = c-nr-nota-fis-ori
           tt-docum-est.dt-emissao = TODAY
           tt-docum-est.dt-trans = TODAY
           tt-docum-est.char-1 = STRING(TODAY,"99/99/9999") + STRING(TIME,"HH:MM") + 'DIGITA€ÇO RµPIDA'.

    FOR EACH tt-itens NO-LOCK
             BREAK BY tt-itens.it-codigo.

        RUN pi-acompanhar IN h-acomp (INPUT "Criando Itens: " + tt-itens.it-codigo).
        FIND item WHERE
             item.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.

        /* Cria os Itens do Recebimento */
        CREATE tt-item-doc-est.
        ASSIGN tt-item-doc-est.nro-docto      = tt-docum-est.nro-docto
               tt-item-doc-est.serie-docto    = tt-docum-est.serie-docto
               tt-item-doc-est.cod-emitente   = tt-docum-est.cod-emit
               tt-item-doc-est.nat-operacao   = tt-docum-est.nat-operacao
               tt-item-doc-est.sequencia      = tt-itens.nr-sequencia
               tt-item-doc-est.it-codigo      = tt-itens.it-codigo
               tt-item-doc-est.qt-do-forn     = tt-itens.quantidade
               tt-item-doc-est.preco-total[1] = tt-itens.vlr-tot-item
               tt-item-doc-est.preco-unit[1]  = tt-itens.valor
               tt-item-doc-est.peso           = tt-itens.quantidade
               tt-item-doc-est.base-icm       = tt-itens.vlr-tot-item
               tt-item-doc-est.base-ipi       = tt-itens.vlr-tot-item
               tt-item-doc-est.cod-depos      = item.deposito-pad
               tt-item-doc-est.dt-vali-lote   = 12.31.9999
               tt-item-doc-est.narrativa      = "Recebimento Autom tico".
    END.
    ASSIGN tt-docum-est.tot-valor = 1000
           tt-docum-est.tot-peso = 10
           tt-docum-est.valor-mercad = 1000
           tt-docum-est.base-icm = 1000
           tt-docum-est.base-ipi = 1000
           tt-docum-est.despesa-nota = 20.

    RUN pi-acompanhar IN h-acomp (INPUT "Criando Recebimento").

    /* Cria Recebimento Fiscal */
    RUN openQueryStatic IN h-boin090 (INPUT "Main":U).
    RUN emptyRowErrors IN h-boin090.
    RUN setRecord IN h-boin090 (INPUT TABLE tt-docum-est).
    RUN createRecord IN h-boin090.
    RUN getRowErrors IN h-boin090 (OUTPUT TABLE RowErrors).
    IF CAN-FIND(FIRST RowErrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:

       RUN pi-finalizar IN h-acomp.

       FOR EACH rowerrors WHERE
                RowErrors.ErrorSubType = "ERROR":U:
           MESSAGE "Erro ao Gerar o Recebimento" SKIP
                    rowerrors.errordescription 
                   VIEW-AS ALERT-BOX.
       END.
       IF VALID-HANDLE(h-boin090) THEN
          DELETE PROCEDURE h-boin090.

       IF VALID-HANDLE(h-boin176) THEN
          DELETE PROCEDURE h-boin176.

       RETURN 'ADM-ERROR'.
    END.

    RUN pi-acompanhar IN h-acomp (INPUT "Criando Itens Recebimento").

    /* Cria Itens do Recebimento*/
    RUN openQueryStatic IN h-boin176 (INPUT "Main":U).
    RUN emptyRowErrors IN h-boin176.

    FOR EACH tt-item-doc-est:
        EMPTY TEMP-TABLE wt-item-doc-est.
        CREATE wt-item-doc-est.
        BUFFER-COPY tt-item-doc-est TO wt-item-doc-est.

        RUN setRecord IN h-boin176 (INPUT TABLE wt-item-doc-est).
        RUN createRecord IN h-boin176.
    END.
    RUN getRowErrors IN h-boin176 (OUTPUT TABLE RowErrors).
    IF CAN-FIND(FIRST RowErrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:

       RUN pi-finalizar IN h-acomp.

       FOR EACH rowerrors WHERE
                RowErrors.ErrorSubType = "ERROR":U:
           MESSAGE "Erro ao Gerar os Itens do Recebimento" SKIP
                    rowerrors.errordescription 
                   VIEW-AS ALERT-BOX.
       END.
       IF VALID-HANDLE(h-boin090) THEN
          DELETE PROCEDURE h-boin090.

       IF VALID-HANDLE(h-boin176) THEN
          DELETE PROCEDURE h-boin176.

       RETURN 'ADM-ERROR'.
    END.

    RUN pi-finalizar IN h-acomp.

    IF VALID-HANDLE(h-boin090) THEN
       DELETE PROCEDURE h-boin090.
    
    IF VALID-HANDLE(h-boin176) THEN
       DELETE PROCEDURE h-boin176.
