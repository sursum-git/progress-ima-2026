/* Temp Tables Recebidas DO Receiver */

DEF TEMP-TABLE rowErrors NO-UNDO    
         FIELD errorsequence    AS INT
         FIELD errornumber      AS INT
         FIELD errordescription AS CHAR FORMAT "x(150)"
         FIELD errorparameters  AS CHAR
         FIELD errortype        AS CHAR
         FIELD errorhelp        AS CHAR FORMAT "x(150)"
         FIELD errorsubtype     AS CHAR.

DEF TEMP-TABLE  tt-ordem-compra LIKE ordem-compra
        FIELD r-rowid AS ROWID.

DEF TEMP-TABLE  tt-ordem-compra-bo LIKE ordem-compra
        FIELD r-rowid AS ROWID.

DEF TEMP-TABLE  tt-ordem-compra-aux LIKE ordem-compra
        FIELD r-rowid AS ROWID.

DEF TEMP-TABLE  tt-prazo-compra LIKE prazo-compra
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-prazo-compra-aux LIKE prazo-compra
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE  tt-prazo-compra-bo LIKE prazo-compra
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-cotacao-item-bo LIKE cotacao-item
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-cotacao-item LIKE cotacao-item
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-cond-especif-bo LIKE cond-especif
         FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-cond-especif LIKE cond-especif
         FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE tt-bo-erro NO-UNDO
            FIELD i-sequen     AS INT
            FIELD cd-erro      AS INT
            FIELD mensagem     AS CHAR FORMAT "x(255)"
            FIELD parametros   AS CHAR FORMAT "x(255)" INIT ""
            FIELD errortype    AS CHAR FORMAT "x(20)"
            FIELD errorhelp    AS CHAR FORMAT "x(20)"
            FIELD errorsubtype AS character.


{inbo/boin295.i tt-pedido-compr}
{inbo/boin295.i tt-pedido-compr-bo}

DEF INPUT PARAM TABLE FOR tt-pedido-compr.
DEF INPUT PARAM TABLE FOR tt-ordem-compra.
DEF INPUT PARAM TABLE FOR tt-prazo-compra.
DEF INPUT PARAM TABLE FOR tt-cotacao-item.
DEF INPUT PARAM TABLE FOR tt-cond-especif.

DEF VAR h-boin082    AS HANDLE NO-UNDO.
DEF VAR h-boin082vl  AS HANDLE NO-UNDO.
DEF VAR h-boin274    AS HANDLE NO-UNDO.
DEF VAR h-boin274vl  AS HANDLE NO-UNDO.
DEF VAR h-boin295    AS HANDLE NO-UNDO.
DEF VAR h-boin356    AS HANDLE NO-UNDO.
DEF VAR h-boin356vl  AS HANDLE NO-UNDO.
DEF VAR h-boin057    AS HANDLE NO-UNDO.
DEF VAR h-boin356aux AS HANDLE NO-UNDO.

/* Chamada e abertura das query‹s das BO‹s */
RUN inbo/boin082.p      PERSISTENT SET h-boin082.
RUN inbo/boin082.p      PERSISTENT SET h-boin082vl.
RUN inbo/boin274.p      PERSISTENT SET h-boin274.
RUN inbo/boin274vl.p    PERSISTENT SET h-boin274vl.
RUN inbo/boin295.p      PERSISTENT SET h-boin295.
RUN inbo/boin356.p      PERSISTENT SET h-boin356.
RUN inbo/boin356vl.p    PERSISTENT SET h-boin356vl.
RUN inbo/boin057.p      PERSISTENT SET h-boin057.

RUN openquery IN h-boin082 (INPUT 1).
RUN openQuery IN h-boin274 (INPUT 1).
RUN openQuery IN h-boin295 (INPUT 1).
RUN openQuery IN h-boin356 (INPUT 1).
RUN openQueryMaIN IN h-boin057.

DEF VAR i-nro-rows                  AS INTEGER NO-UNDO.                         

                                    
/*  Atualizar Pedido Compra    */

FOR EACH tt-pedido-compr:
    RUN goToKey IN h-boin295 (INPUT tt-pedido-compr.num-pedido).
    RUN GetRecord IN h-boin295 (OUTPUT TABLE tt-pedido-compr-bo).
    FIND FIRST tt-pedido-compr-bo NO-ERROR.
    RUN emptyRowObject  IN h-boin295.
    RUN setRecord IN h-boin295 (INPUT TABLE tt-pedido-compr-bo).

    /* Atualizar Ordem Compra */

    FOR EACH tt-ordem-compra:
        RUN goToKey IN h-boin274 (INPUT tt-ordem-compra.numero-ordem).
        RUN GetRecord IN h-boin274 (OUTPUT TABLE tt-ordem-compra-bo).
        FIND FIRST tt-ordem-compra-bo NO-ERROR.
        ASSIGN tt-ordem-compra-bo.preco-unit   = tt-ordem-compra.preco-unit
               tt-ordem-compra-bo.pre-unit-for = tt-ordem-compra.pre-unit-for
               tt-ordem-compra-bo.preco-fornec = tt-ordem-compra.preco-fornec.
        RUN setRecord IN h-boin274 (INPUT TABLE tt-ordem-compra-bo).
        RUN validateUpdatePedEmerg IN h-boin274vl (input TABLE tt-ordem-compra-bo,
                                                   INPUT tt-ordem-compra-bo.r-rowid,
                                                   OUTPUT TABLE tt-bo-erro).
        RUN executeUpdatePedEmerg IN h-boin274vl.
        RUN afterUpdateRecord IN h-boin295.
        RUN getRowErrors IN h-boin274 (OUTPUT TABLE RowErrors).
        IF CAN-FIND(FIRST RowErrors 
                    WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
           FOR EACH rowerrors WHERE
                    RowErrors.ErrorSubType = "ERROR":U:
               MESSAGE "Erro ao Gravar Altera‡Æo Ordem de Compra" SKIP
                        rowerrors.errordescription 
                       VIEW-AS ALERT-BOX.
           END.
           RETURN 'ADM-ERROR'.
        END.
    END.
    /*  Atualiza‡Æo Prazo Compra */

    FOR EACH tt-prazo-compra:
        RUN goToKey IN h-boin356 (INPUT tt-prazo-compra.numero-ordem, INPUT tt-prazo-compra.parcela).
        RUN GetRecord IN h-boin356 (OUTPUT TABLE tt-prazo-compra-bo).
        FIND FIRST tt-prazo-compra-bo no-error.
        ASSIGN tt-prazo-compra-bo.data-entrega  = tt-prazo-compra.data-entrega
               tt-prazo-compra-bo.qtd-sal-forn  = tt-prazo-compra.qtd-sal-forn.  
    
        RUN emptyRowObject  IN h-boin356.
        RUN setRecord IN h-boin356 (INPUT TABLE tt-prazo-compra-bo).
        FOR EACH tt-ordem-compra-aux:
            DELETE tt-ordem-compra-aux.
        END.
        FIND FIRST tt-ordem-compra WHERE
                   tt-ordem-compra.numero-ordem = tt-prazo-compra.numero-ordem
                   NO-LOCK no-error.
        IF  AVAIL tt-ordem-compra THEN do:
            CREATE tt-ordem-compra-aux.
            BUFFER-COPY tt-ordem-compra TO tt-ordem-compra-aux.
            RUN INputTABLEOrdemCompra in h-boin356vl (INPUT TABLE tt-ordem-compra-aux).
            RUN inbo/boin356.p PERSISTENT SET h-boin356aux.
            RUN setConstraINtNumOrdem in h-boin356aux (INPUT tt-ordem-compra.numero-ordem).
            RUN openQueryStatic       IN h-boin356aux (INPUT "byNumOrdembyParcela").
            RUN getFirst              IN h-boin356aux.
            RUN getBatchRecords       IN h-boin356aux (?, 
                                                       NO, 
                                                       ?, 
                                                       OUTPUT i-nro-rows,
                                                       OUTPUT TABLE tt-prazo-compra-aux).
            DELETE PROCEDURE h-boin356aux.
            RUN validateUpdateManutOrdCompra IN h-boin356vl (INPUT  TABLE tt-prazo-compra-bo,
                                                             INPUT  TABLE tt-prazo-compra-aux,
                                                             "mod",
                                                             INPUT  tt-prazo-compra-bo.r-rowid,
                                                             INPUT  STRING(TODAY, "99/99/9999"),
                                                             "31/12/9999",
                                                             "", /* usuÿrio corrente serÿ pego na BO */
                                                             YES,
                                                             NO,
                                                             YES,
                                                             YES,
                                                             OUTPUT TABLE rowerrors).
            RUN executeUpdateManutOrdCompra IN h-boin356vl (INPUT tt-prazo-compra-bo.r-rowid).
            RUN getRowErrors IN h-boin356 (OUTPUT TABLE RowErrors).
        END.
        IF CAN-FIND(FIRST RowErrors 
                    WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
           FOR EACH rowerrors WHERE
                    RowErrors.ErrorSubType = "ERROR":U:
               MESSAGE "Erro ao Gravar Altera‡Æo Ordem de Compra" SKIP
                        rowerrors.errordescription 
                       VIEW-AS ALERT-BOX.
           END.
           RETURN 'ADM-ERROR'.
        END.
    END.
    FOR EACH tt-cotacao-item:
        RUN goToCotacaoAprovada IN h-boin082 (INPUT tt-cotacao-item.numero-ordem).
        IF  RETURN-VALUE = "OK":U THEN DO:
            RUN GetRecord IN h-boin082 (OUTPUT TABLE tt-cotacao-item-bo).
            FIND FIRST tt-cotacao-item-bo NO-ERROR.
            ASSIGN tt-cotacao-item-bo.preco-unit       = tt-cotacao-item.preco-unit   
                   tt-cotacao-item-bo.preco-fornec     = tt-cotacao-item.preco-fornec.
            RUN emptyRowObject  IN h-boin082.
            RUN setRecord IN h-boin082 (INPUT TABLE tt-cotacao-item-bo).
            RUN updateRecord IN h-boin082.
            RUN getRowErrors IN h-boin082 (OUTPUT TABLE RowErrors).
            RUN atualizaOrdensCompraPedidoCompra IN h-boin295.
            RUN aprovacaoEletronica IN h-boin295 (INPUT tt-ordem-compra-bo.r-rowid).
            RUN atualizaSituacaoPedidoCompra IN h-boin295.  
        END.
        IF CAN-FIND(FIRST RowErrors 
                    WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
           FOR EACH rowerrors WHERE
                    RowErrors.ErrorSubType = "ERROR":U:
               MESSAGE "Erro ao Gravar Altera‡Æo Prazo de Compra" SKIP
                        rowerrors.errordescription 
                       VIEW-AS ALERT-BOX.
           END.
           RETURN 'ADM-ERROR'.
        END.
    END.
    CREATE alt-ped.
    ASSIGN alt-ped.data         = TODAY
           alt-ped.hora         = STRING(TIME,"HH:MM:SS")
           alt-ped.usuario      = tt-ordem-compra-bo.cod-comprado
           alt-ped.num-pedido   = tt-ordem-compra-bo.num-pedido
           alt-ped.numero-ordem = tt-ordem-compra-bo.numero-ordem
           alt-ped.parcela      = tt-prazo-compra-bo.parcela
           alt-ped.cod-cond-pag = tt-pedido-compr-bo.cod-cond-pag
           alt-ped.preco        = tt-prazo-compra-bo.qtd-sal-forn 
           alt-ped.quantidade   = tt-cotacao-item-bo.preco-fornec 
           alt-ped.data-entrega = tt-prazo-compra-bo.data-entrega
           alt-ped.char-1       = STRING(tt-cotacao-item-bo.preco-fornec) + "|" + 
                                  STRING(tt-prazo-compra-bo.qtd-sal-forn) + "|" + 
                                  STRING(tt-prazo-compra-bo.data-entrega) + "|" + 
                                  STRING(tt-pedido-compr-bo.cod-cond-pag)
           alt-ped.observacao   = "Alterado a Quantidade Automaticamente".
END.

DELETE PROCEDURE h-boin082.
DELETE PROCEDURE h-boin082vl.
DELETE PROCEDURE h-boin274.
DELETE PROCEDURE h-boin274vl.
DELETE PROCEDURE h-boin295.
DELETE PROCEDURE h-boin356.
DELETE PROCEDURE h-boin356vl.
DELETE PROCEDURE h-boin057.

