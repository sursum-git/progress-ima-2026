DEF TEMP-TABLE tt-ped-item LIKE ped-item 
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE wt-ped-item NO-UNDO LIKE ped-item
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF VAR h-bodi154    AS HANDLE.

DEF VAR i-sit-item LIKE ped-item.cod-sit-item.
DEF VAR i-sit-aval LIKE ped-venda.cod-sit-aval.
DEF VAR i-cod-mess LIKE ped-venda.cod-message-alert.      
DEF VAR da-dt-mess LIKE ped-venda.dt-mensagem.
DEF VAR c-desc-for LIKE ped-venda.desc-forc-cr.           
DEF VAR l-dsp-fat  LIKE ped-venda.dsp-pre-fat.

DEF VAR i-sit-ped LIKE ped-venda.cod-sit-ped.
DEF VAR l-erro    AS LOG.

DEF INPUT PARAMETER TABLE FOR tt-ped-item.

FIND FIRST tt-ped-item NO-ERROR.

IF NOT AVAIL tt-ped-item THEN RETURN 'NOK'.

FIND ped-venda WHERE
     ped-venda.nr-pedcli = tt-ped-item.nr-pedcli NO-ERROR.

FIND ped-item OF ped-venda WHERE 
     ped-item.nr-sequencia = tt-ped-item.nr-sequencia NO-LOCK NO-ERROR.

ASSIGN i-sit-item = ped-item.cod-sit-item
       i-sit-aval = ped-venda.cod-sit-aval
       i-cod-mess = ped-venda.cod-message-alert
       da-dt-mess = ped-venda.dt-mensagem
       c-desc-for = ped-venda.desc-forc-cr
       l-dsp-fat  = ped-venda.dsp-pre-fat. 

ASSIGN i-sit-ped = 0.
IF ped-venda.cod-sit-ped = 4 THEN
   ASSIGN i-sit-ped = ped-venda.cod-sit-ped
          ped-venda.cod-sit-ped = 1.
    
IF i-sit-item = 5 THEN
   RUN esapi/reativa-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                 INPUT '1').

IF NOT VALID-HANDLE(h-bodi154) or
   h-bodi154:TYPE      <> "PROCEDURE":U OR
   h-bodi154:FILE-NAME <> "dibo/bodi154.p":U THEN
   RUN dibo/bodi154.p PERSISTENT SET h-bodi154.

ASSIGN tt-ped-item.vl-liq-abe = tt-ped-item.qt-pedida * tt-ped-item.vl-preori
       tt-ped-item.vl-preuni = tt-ped-item.vl-preori. 

RUN setConstraintKey IN h-bodi154 (INPUT tt-ped-item.nome-abrev,
                                   INPUT tt-ped-item.nr-pedcli,
                                   INPUT tt-ped-item.nr-sequencia,
                                   INPUT tt-ped-item.it-codigo,
                                   INPUT tt-ped-item.cod-refer).

RUN openQueryStatic in h-bodi154 (input "Key":U).        
RUN emptyRowErrors IN h-bodi154.
RUN setRecord IN h-bodi154(INPUT TABLE tt-ped-item).
RUN updateRecord IN h-bodi154.
RUN getRowErrors IN h-bodi154(OUTPUT TABLE RowErrors).
    
ASSIGN l-erro = NO.
IF CAN-FIND(FIRST RowErrors 
            WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:

   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
       MESSAGE "Erro ao Gravar o Item no Pedido" SKIP
               " Pedido:" ped-venda.nr-pedcli
               " Cliente:" ped-venda.nome-abrev
               " Item:" tt-ped-item.it-codigo
               " Refer:" tt-ped-item.cod-refer
               " Qtd:" tt-ped-item.qt-pedida SKIP
               "Erro:" rowerrors.errornumber " - " SKIP
                rowerrors.errordescription 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.

       ASSIGN l-erro = YES.
   END.
END.

FIND ped-item OF tt-ped-item NO-ERROR.
IF NOT AVAIL ped-item THEN NEXT.

IF ped-item.vl-preori = 0 THEN
   ASSIGN ped-item.vl-preori = tt-ped-item.vl-preori
          ped-item.vl-preuni = tt-ped-item.vl-preuni.

FIND ped-venda OF ped-item USE-INDEX ch-pedido NO-ERROR.

FIND emitente WHERE
     emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.

/* Avalia‡Æo de Cr‚dito Autom tico */
IF emitente.ind-cre-cli = 2 THEN
   ASSIGN i-sit-aval = 3.

IF i-sit-item = 5 THEN
   RUN esapi/suspende-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                  INPUT '1').

ASSIGN ped-venda.cod-sit-aval = i-sit-aval
       ped-venda.cod-message-alert = i-cod-mess 
       ped-venda.dt-mensagem = da-dt-mess 
       ped-venda.desc-forc-cr = c-desc-for 
       ped-venda.dsp-pre-fat = l-dsp-fat.

IF i-sit-ped <> 0 THEN
   ASSIGN ped-venda.cod-sit-ped = i-sit-ped.

IF VALID-HANDLE(h-bodi154) THEN
   DELETE PROCEDURE h-bodi154.

IF l-erro THEN
   RETURN 'NOK'.

