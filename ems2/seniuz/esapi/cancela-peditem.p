DEF INPUT PARAMETER p-nr-pedcli LIKE ped-venda.nr-pedcli.
DEF INPUT PARAMETER p-nr-seq    LIKE ped-item.nr-sequencia.
DEF INPUT PARAMETER p-motivo    AS   CHAR. 

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF VAR h-bodi154    AS HANDLE.
DEF VAR h-bodi154can AS HANDLE.

DEF VAR i-sit-aval LIKE ped-venda.cod-sit-aval.
DEF VAR i-cod-mess LIKE ped-venda.cod-message-alert.      
DEF VAR da-dt-mess LIKE ped-venda.dt-mensagem.
DEF VAR c-desc-for LIKE ped-venda.desc-forc-cr.           
DEF VAR l-dsp-fat  LIKE ped-venda.dsp-pre-fat.

IF NOT VALID-HANDLE(h-bodi154) or
   h-bodi154:TYPE      <> "PROCEDURE":U OR
   h-bodi154:FILE-NAME <> "dibo/bodi154.p":U THEN
   RUN dibo/bodi154.p PERSISTENT SET h-bodi154.

IF NOT VALID-HANDLE(h-bodi154can) OR
   h-bodi154can:TYPE      <> "PROCEDURE":U OR
   h-bodi154can:FILE-NAME <> "dibo/bodi154can.p":U THEN
   RUN dibo/bodi154can.p PERSISTENT SET h-bodi154can.

FIND ped-venda WHERE
     ped-venda.nr-pedcli = p-nr-pedcli NO-LOCK.

FIND ped-item OF ped-venda WHERE 
     ped-item.nr-sequencia = p-nr-seq NO-LOCK NO-ERROR.

ASSIGN i-sit-aval = ped-venda.cod-sit-aval
       i-cod-mess = ped-venda.cod-message-alert
       da-dt-mess = ped-venda.dt-mensagem
       c-desc-for = ped-venda.desc-forc-cr
        l-dsp-fat = ped-venda.dsp-pre-fat. 

RUN ValidateCancelation IN h-bodi154can (INPUT ROWID(ped-item),
                                         INPUT p-motivo,
                                         INPUT-OUTPUT TABLE RowErrors).

IF CAN-FIND(FIRST RowErrors
            WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
       MESSAGE "Erro ao Cancelar o Item no Pedido" SKIP
               " Pedido:" ped-venda.nr-pedcli
               " Cliente:" ped-venda.nome-abrev
               " Sequencia:" ped-item.nr-sequencia SKIP 
               "Erro:" rowerrors.errornumber " - " 
               rowerrors.errordescription
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
   RETURN 'ADM-ERROR'.
END.
ELSE
   RUN Updatecancelation IN h-bodi154can (INPUT ROWID(ped-item),
                                          INPUT p-motivo,
                                          INPUT TODAY,
                                          INPUT '').

RUN esapi/cria-log-pedvenda.p (INPUT ped-item.nr-pedcli,
                               INPUT ped-item.nome-abrev,
                               INPUT TRIM(STRING(ped-item.nr-sequencia,">>>9")) + 
                                     " Item: " + TRIM(ped-item.it-codigo) + " Refer: " + TRIM(ped-item.cod-refer) + 
                                     " Qtde: " + TRIM(STRING(ped-item.qt-pedida,">>>,>>9.99")) + ": Cancelada por " + p-motivo,
                               INPUT YES).

FIND ped-venda where
     ped-venda.nr-pedcli = p-nr-pedcli NO-ERROR.

IF ped-venda.cod-sit-ped = 6 THEN 
   RUN esapi/cria-log-pedvenda.p (INPUT ped-venda.nr-pedcli,
                                  INPUT ped-venda.nome-abrev,
                                  INPUT "Pedido Cancelado. " + p-motivo,
                                  INPUT YES).

/* Completa o Pedido */
IF ped-venda.cod-sit-aval <> 1 AND NOT ped-venda.completo THEN
   RUN esapi/completa-pedvenda.p (INPUT ped-venda.nr-pedido).

FIND emitente WHERE
     emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.

/* Avalia‡Æo de Cr‚dito Autom tico */
IF emitente.ind-cre-cli = 2 THEN
   ASSIGN i-sit-aval = 3.

ASSIGN ped-venda.cod-sit-aval = i-sit-aval
       ped-venda.cod-message-alert = i-cod-mess 
       ped-venda.dt-mensagem = da-dt-mess 
       ped-venda.desc-forc-cr = c-desc-for 
       ped-venda.dsp-pre-fat = l-dsp-fat.

FIND CURRENT ped-venda NO-LOCK NO-ERROR.
FIND CURRENT ped-venda NO-LOCK NO-ERROR.

IF VALID-HANDLE(h-bodi154) THEN
   DELETE PROCEDURE h-bodi154.

IF VALID-HANDLE(h-bodi154can) THEN
   DELETE PROCEDURE h-bodi154can.

