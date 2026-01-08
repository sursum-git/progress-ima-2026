DEF INPUT PARAMETER p-num-pedido LIKE pedido-compr.num-pedido.
DEF INPUT PARAMETER p-it-codigo  LIKE ordem-compra.it-codigo.
DEF INPUT PARAMETER p-cod-refer  LIKE ordem-compra.cod-refer.
DEF INPUT PARAMETER p-qt-solic   LIKE ordem-compra.qt-solic.
DEF INPUT PARAMETER p-preco-forn LIKE ordem-compra.preco-forn.

DEFINE TEMP-TABLE tt-cotacao-item NO-UNDO LIKE cotacao-item
       FIELD r-Rowid AS ROWID.
DEFINE TEMP-TABLE tt-ordem-compra NO-UNDO LIKE ordem-compra
       FIELD r-Rowid AS ROWID.
DEFINE TEMP-TABLE tt-prazo-compra LIKE prazo-compra
       FIELD r-Rowid AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.
    
DEF VAR h-boin274    AS HANDLE NO-UNDO.
DEF VAR h-boin274sd  AS HANDLE NO-UNDO.
DEF VAR h-boin356    AS HANDLE NO-UNDO.
DEF VAR h-boin082     AS HANDLE NO-UNDO.

DEF VAR i-num-ordem LIKE ordem-compra.numero-ordem.
DEF VAR da-dt-cotacao  LIKE pedido-compr.data-pedido.
DEF VAR c-return AS CHAR.

IF NOT VALID-HANDLE(h-boin274) or
   h-boin274:TYPE      <> "PROCEDURE":U OR
   h-boin274:FILE-NAME <> "inbo/boin274.p":U THEN
   RUN inbo/boin274.p PERSISTENT SET h-boin274.

IF NOT VALID-HANDLE(h-boin274sd) or
   h-boin274sd:TYPE      <> "PROCEDURE":U OR
   h-boin274sd:FILE-NAME <> "inbo/boin274sd.p":U THEN
   RUN inbo/boin274sd.p PERSISTENT SET h-boin274sd.

IF NOT VALID-HANDLE(h-boin356) or
   h-boin356:TYPE      <> "PROCEDURE":U OR
   h-boin356:FILE-NAME <> "inbo/boin356.p":U THEN
   RUN inbo/boin356.p PERSISTENT SET h-boin356.

IF NOT VALID-HANDLE(h-boin082) or
  h-boin082:TYPE      <> "PROCEDURE":U OR
  h-boin082:FILE-NAME <> "inbo/boin082.p":U THEN
   RUN inbo/boin082.p PERSISTENT SET h-boin082.

FOR EACH tt-ordem-compra.
    DELETE tt-ordem-compra.
END.

FIND pedido-compr WHERE
     pedido-compr.num-pedido = p-num-pedido NO-ERROR.

RUN geraNumeroOrdemPedEmerg IN h-boin274sd (OUTPUT i-num-ordem,
                                            OUTPUT TABLE RowErrors).

CREATE tt-ordem-compra.
ASSIGN tt-ordem-compra.num-pedido = pedido-compr.num-pedido
       tt-ordem-compra.numero-ordem = i-num-ordem
       tt-ordem-compra.cod-emit = pedido-compr.cod-emitente
       tt-ordem-compra.cod-estabel = pedido-compr.cod-estabel
       tt-ordem-compra.cod-comprado = 'emoura'
       tt-ordem-compra.requisitante = 'edmar'
       tt-ordem-compra.it-codigo = p-it-codigo
       tt-ordem-compra.cod-refer = p-cod-refer
       tt-ordem-compra.qt-solic = p-qt-solic
       tt-ordem-compra.conta-contab = '412106801537'
       tt-ordem-compra.preco-forn = p-preco-forn
       tt-ordem-compra.impr-ficha = NO
       tt-ordem-compra.dep-almoxar = 'ALM'
       tt-ordem-compra.tp-despesa = 5
       tt-ordem-compra.cod-transp = 99999
       tt-ordem-compra.cod-entrega = "Padr∆o".
       
RUN openQueryStatic IN h-boin274(INPUT "Main":U).
RUN emptyRowErrors IN h-boin274.
RUN setRecord IN h-boin274(INPUT TABLE tt-ordem-compra).
RUN createRecord IN h-boin274.
RUN getRowErrors IN h-boin274(OUTPUT TABLE RowErrors).
IF CAN-FIND(FIRST RowErrors 
            WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:

   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
       MESSAGE "Erro ao Criar o Item no Pedido" SKIP
               " Pedido:" pedido-compr.num-pedido
               " Cliente:" tt-ordem-compra.cod-emit
               " Item:" tt-ordem-compra.it-codigo
               " Refer:" tt-ordem-compra.cod-refer
               " Qtd:" tt-ordem-compra.qt-solic SKIP
               "Erro:" rowerrors.errornumber " - " SKIP
                rowerrors.errordescription 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
   RETURN 'ADM-ERROR'.
END.

FIND ITEM WHERE
     ITEM.it-codigo = tt-ordem-compra.it-codigo NO-LOCK NO-ERROR.

CREATE tt-prazo-compra.
ASSIGN tt-prazo-compra.numero-ordem = tt-ordem-compra.numero-ordem
       tt-prazo-compra.parcela      = 1
       tt-prazo-compra.un           = ITEM.un
       tt-prazo-compra.quantidade   = tt-ordem-compra.qt-solic
       tt-prazo-compra.qtd-sal-forn = tt-ordem-compra.qt-solic
       tt-prazo-compra.data-entrega = TODAY
       tt-prazo-compra.situacao     = 2.

RUN openQueryStatic IN h-boin356(INPUT "Main":U).
RUN emptyRowErrors IN h-boin356.
RUN setRecord IN h-boin356 (INPUT TABLE tt-prazo-compra).
RUN createRecord IN h-boin356.
RUN getRowErrors IN h-boin356 (OUTPUT TABLE RowErrors).
IF CAN-FIND(FIRST RowErrors 
            WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:

   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
       MESSAGE "Erro ao Criar a Parcela da Ordem" SKIP
               " Pedido:" pedido-compr.num-pedido
               " Cliente:" tt-ordem-compra.cod-emit
               " Item:" tt-ordem-compra.it-codigo
               " Refer:" tt-ordem-compra.cod-refer
               " Qtd:" tt-ordem-compra.qt-solic SKIP
               "Erro:" rowerrors.errornumber " - " SKIP
                rowerrors.errordescription 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
   RETURN 'ADM-ERROR'.
END.

IF VALID-HANDLE(h-boin274) THEN
   DELETE PROCEDURE h-boin274.

IF VALID-HANDLE(h-boin274sd) THEN
   DELETE PROCEDURE h-boin274sd.

IF VALID-HANDLE(h-boin356) THEN
   DELETE PROCEDURE h-boin356.

IF VALID-HANDLE(h-boin082) THEN
   DELETE PROCEDURE h-boin082.

