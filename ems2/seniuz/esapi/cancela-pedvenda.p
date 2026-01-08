DEF INPUT PARAMETER p-nr-pedcli LIKE ped-venda.nr-pedcli.
DEF INPUT PARAMETER p-motivo    AS   CHAR.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF VAR h-bodi159can AS HANDLE.

FIND ped-venda WHERE
     ped-venda.nr-pedcli = p-nr-pedcli NO-ERROR.

IF ped-venda.cod-sit-ped = 4 THEN
   ASSIGN ped-venda.cod-sit-ped = 1.

IF NOT VALID-HANDLE(h-bodi159can) OR
   h-bodi159can:TYPE      <> "PROCEDURE":U OR
   h-bodi159can:FILE-NAME <> "dibo/bodi159can.p":U THEN
   RUN dibo/bodi159can.p PERSISTENT SET h-bodi159can.

RUN emptyRowErrors IN h-bodi159can.
/*
RUN ValidateCancelation IN h-bodi159can (INPUT ROWID(ped-venda),
                                         OUTPUT TABLE RowErrors).
IF CAN-FIND(FIRST RowErrors
            WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
       MESSAGE "Erro ao Cancelar o Pedido" SKIP
               " Pedido:" ped-venda.nr-pedcli
               " Cliente:" ped-venda.nome-abrev SKIP
               "Erro:" rowerrors.errornumber " - " 
               rowerrors.errordescription
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
   RETURN 'ADM-ERROR'.
END.
ELSE */
   RUN Updatecancelation IN h-bodi159can (INPUT ROWID(ped-venda),
                                          INPUT p-motivo,
                                          INPUT TODAY,
                                          INPUT '').

IF VALID-HANDLE(h-bodi159can) THEN
   DELETE PROCEDURE h-bodi159can.

