DEF INPUT PARAMETER p-nr-pedcli LIKE ped-venda.nr-pedcli.
DEF INPUT PARAMETER p-nr-seq    LIKE ped-item.nr-sequencia.
DEF INPUT PARAMETER p-it-codigo LIKE ped-item.it-codigo.
DEF INPUT PARAMETER p-cod-refer LIKE ped-item.cod-refer.
DEF INPUT PARAMETER p-qt-pedida LIKE ped-item.qt-pedida.
DEF INPUT PARAMETER p-vl-preuni LIKE ped-item.vl-preori.

DEF TEMP-TABLE tt-ped-item LIKE ped-item 
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
DEF VAR h-bodi154sdf AS HANDLE.

DEF VAR i-sit-aval LIKE ped-venda.cod-sit-aval.
DEF VAR i-cod-mess LIKE ped-venda.cod-message-alert.      
DEF VAR da-dt-mess LIKE ped-venda.dt-mensagem.
DEF VAR c-desc-for LIKE ped-venda.desc-forc-cr.           
DEF VAR l-dsp-fat  LIKE ped-venda.dsp-pre-fat.
DEF VAR i-cod-sit-ped LIKE ped-venda.cod-sit-ped.

IF NOT VALID-HANDLE(h-bodi154) or
   h-bodi154:TYPE      <> "PROCEDURE":U OR
   h-bodi154:FILE-NAME <> "dibo/bodi154.p":U THEN
   RUN dibo/bodi154.p PERSISTENT SET h-bodi154.

IF NOT VALID-HANDLE(h-bodi154sdf) OR 
   h-bodi154sdf:TYPE      <> "PROCEDURE":U OR
   h-bodi154sdf:FILE-NAME <> "dibo/bodi154sdf.p":U THEN
   RUN dibo/bodi154sdf.p PERSISTENT SET h-bodi154sdf.

FOR EACH tt-ped-item.
    DELETE tt-ped-item.
END.

FIND ped-venda WHERE
     ped-venda.nr-pedcli = p-nr-pedcli NO-ERROR.

ASSIGN i-sit-aval = ped-venda.cod-sit-aval
       i-cod-mess = ped-venda.cod-message-alert
       da-dt-mess = ped-venda.dt-mensagem
       c-desc-for = ped-venda.desc-forc-cr
       l-dsp-fat  = ped-venda.dsp-pre-fat
       i-cod-sit-ped = ped-venda.cod-sit-ped. 

ASSIGN ped-venda.cod-sit-ped = 1.

CREATE tt-ped-item.
ASSIGN tt-ped-item.nome-abrev = ped-venda.nome-abrev
       tt-ped-item.nr-pedcli = ped-venda.nr-pedcli
       tt-ped-item.nr-sequencia = p-nr-seq
       tt-ped-item.it-codigo = p-it-codigo
       tt-ped-item.cod-refer = p-cod-refer
       tt-ped-item.qt-pedida = p-qt-pedida.

FIND preco-item WHERE
     preco-item.nr-tabpre = ped-venda.nr-tabpre AND
     preco-item.it-codigo = p-it-codigo AND
     preco-item.cod-refer = p-cod-refer
     NO-LOCK NO-ERROR.

ASSIGN tt-ped-item.vl-preuni = IF AVAIL preco-item
                               THEN preco-item.preco-venda
                               ELSE p-vl-preuni
       tt-ped-item.vl-preori = tt-ped-item.vl-preuni
       tt-ped-item.qt-un-fat = tt-ped-item.qt-pedida
       tt-ped-item.vl-liq-abe = p-qt-pedida * tt-ped-item.vl-preuni
       tt-ped-item.nat-operacao = ped-venda.nat-operacao
       tt-ped-item.cod-entrega = ped-venda.cod-entrega
       tt-ped-item.nr-tabpre = ped-venda.nr-tabpre
       tt-ped-item.tp-preco = ped-venda.tp-preco
       tt-ped-item.dt-entrega = ped-venda.dt-entrega
       tt-ped-item.char-1 = 'DIGITA€ÇO RµPIDA'
       tt-ped-item.dec-2 = 0.
       
FIND item-dist WHERE 
     item-dist.it-codigo = tt-ped-item.it-codigo NO-LOCK NO-ERROR.
IF NOT AVAIL item-dist THEN DO:
   CREATE item-dist.
   ASSIGN item-dist.it-codigo = tt-ped-item.it-codigo.
END.

RUN openQueryStatic IN h-bodi154(INPUT "Main":U).
RUN emptyRowErrors IN h-bodi154.

RUN inputTable     IN h-bodi154sdf (INPUT TABLE tt-ped-item).
RUN setDefaultItem IN h-bodi154sdf.
RUN outputTable    IN h-bodi154sdf (OUTPUT TABLE tt-ped-item). 

RUN setRecord IN h-bodi154(INPUT TABLE tt-ped-item).
RUN createRecord IN h-bodi154.

RUN getRowErrors IN h-bodi154(OUTPUT TABLE RowErrors).
IF CAN-FIND(FIRST RowErrors 
            WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:

   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
       MESSAGE "Erro ao Criar o Item no Pedido" SKIP
               " Pedido:" ped-venda.nr-pedcli
               " Cliente:" ped-venda.nome-abrev
               " Item:" p-it-codigo
               " Refer:" p-cod-refer
               " Qtd:" p-qt-pedida SKIP
               "Erro:" rowerrors.errornumber " - " SKIP
                rowerrors.errordescription 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
   RETURN 'ADM-ERROR'.
END.

FIND emitente WHERE
     emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.

ASSIGN ped-venda.cod-sit-aval = i-sit-aval
       ped-venda.cod-message-alert = i-cod-mess 
       ped-venda.dt-mensagem = da-dt-mess 
       ped-venda.desc-forc-cr = c-desc-for 
       ped-venda.dsp-pre-fat = l-dsp-fat.

ASSIGN ped-venda.cod-sit-ped = i-cod-sit-ped.

RELEASE ped-venda.
RELEASE ped-item.

IF VALID-HANDLE(h-bodi154) THEN
   DELETE PROCEDURE h-bodi154.

IF VALID-HANDLE(h-bodi154sdf) THEN
   DELETE PROCEDURE h-bodi154sdf.
