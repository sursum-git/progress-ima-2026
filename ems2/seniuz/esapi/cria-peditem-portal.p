DEF INPUT  PARAMETER p-nr-pedcli LIKE ped-venda.nr-pedcli.
DEF INPUT  PARAMETER p-nr-seq    LIKE ped-item.nr-sequencia.
DEF INPUT  PARAMETER p-it-codigo LIKE ped-item.it-codigo.
DEF INPUT  PARAMETER p-cod-refer LIKE ped-item.cod-refer.
DEF INPUT  PARAMETER p-qt-pedida LIKE ped-item.qt-pedida.
DEF INPUT  PARAMETER p-vl-preuni LIKE ped-item.vl-preori.
DEF OUTPUT PARAMETER c-erro-peditem AS CHAR.

DEF TEMP-TABLE tt-ped-item NO-UNDO LIKE ped-item 
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.
    
DEF NEW GLOBAL SHARED VAR c-msg-erro-di154 AS CHAR NO-UNDO.

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

IF NOT VALID-HANDLE(h-bodi154sdf) or
   h-bodi154sdf:TYPE      <> "PROCEDURE":U OR
   h-bodi154sdf:FILE-NAME <> "dibo/bodi154sdf.p":U THEN
   RUN dibo/bodi154sdf.p PERSISTENT SET h-bodi154sdf.

ASSIGN c-msg-erro-di154 = "".

FOR EACH tt-ped-item.
    DELETE tt-ped-item.
END.

FIND ped-venda WHERE
     ped-venda.nr-pedcli = p-nr-pedcli NO-ERROR.

IF NOT AVAIL ped-venda THEN DO:
   ASSIGN c-erro-peditem = "(ESAPI/CRIA-PED-ITEM.P) Pedido de Venda " + p-nr-pedcli + " N∆o Cadastrado...".
   RETURN 'ADM-ERROR'.
END.

IF p-nr-seq = 0 THEN DO.
   ASSIGN c-erro-peditem = "(ESAPI/CRIA-PED-ITEM.P) Sequencia est† Zerada...".
   RETURN 'ADM-ERROR'.
END.

IF p-qt-pedida = 0 THEN DO:
    ASSIGN c-erro-peditem = "(ESAPI/CRIA-PED-ITEM.P) Quantidade do Item Pedida est† Zerada...".
    RETURN 'ADM-ERROR'.
END.

IF p-vl-preuni = 0 THEN DO:
   ASSIGN c-erro-peditem = "(ESAPI/CRIA-PED-ITEM.P) Preco Unitario do Item est† Zerado...".
   RETURN 'ADM-ERROR'.
END.


IF p-it-codigo = "" THEN DO:
   ASSIGN c-erro-peditem = "(ESAPI/CRIA-PED-ITEM.P) Item N∆o Informado...".
   RETURN 'ADM-ERROR'.
END.

FIND ITEM WHERE
     ITEM.it-codigo = p-it-codigo NO-LOCK NO-ERROR.
IF NOT AVAIL ITEM THEN DO.
   ASSIGN c-erro-peditem = "(ESAPI/CRIA-PED-ITEM.P) Item N∆o Cadastrado...".
   RETURN 'ADM-ERROR'.
END.


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
       tt-ped-item.qt-pedida = p-qt-pedida
       tt-ped-item.vl-preori = p-vl-preuni
       tt-ped-item.vl-liq-abe = p-qt-pedida * p-vl-preuni
       tt-ped-item.nat-operacao = ped-venda.nat-operacao
       tt-ped-item.cod-entrega = "Padr∆o"
       tt-ped-item.dt-entrega = ped-venda.dt-entrega.

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

   FIND FIRST tt-ped-item NO-LOCK NO-ERROR.

   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
       ASSIGN c-erro-peditem = "Erro ao Criar o Item no Pedido " + ped-venda.nr-pedcli + " Sequencia " + STRING(p-nr-seq) +
                               " Item " + tt-ped-item.it-codigo + " Refer: " + tt-ped-item.cod-refer + CHR(10) + 
                               rowerrors.errordescription.
   END.  

   ASSIGN c-erro-peditem = IF c-erro-peditem = ''
                           THEN c-msg-erro-di154
                           ELSE c-erro-peditem + CHR(10) + c-msg-erro-di154.




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
