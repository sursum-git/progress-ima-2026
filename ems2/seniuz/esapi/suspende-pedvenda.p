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

DEF VAR h-bodi159sus AS HANDLE.

FIND ped-venda WHERE
     ped-venda.nr-pedcli = p-nr-pedcli NO-ERROR.

IF NOT VALID-HANDLE(h-bodi159sus) OR
   h-bodi159sus:TYPE      <> "PROCEDURE":U OR
   h-bodi159sus:FILE-NAME <> "dibo/bodi159sus.p":U THEN
   RUN dibo/bodi159sus.p PERSISTENT SET h-bodi159sus.

RUN emptyRowErrors IN h-bodi159sus.

RUN ValidateSuspension IN h-bodi159sus (INPUT ROWID(ped-venda),
                                        OUTPUT TABLE RowErrors).
IF CAN-FIND(FIRST RowErrors 
            WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
       MESSAGE "Erro ao Suspender o Pedido" SKIP
               " Pedido:" ped-venda.nr-pedcli
               " Cliente:" ped-venda.nome-abrev SKIP
               "Erro:" rowerrors.errornumber " - " 
               rowerrors.errordescription 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
END.
ELSE
   RUN UpdateSuspension IN h-bodi159sus (INPUT ROWID(ped-venda),
                                         INPUT p-motivo).

IF VALID-HANDLE(h-bodi159sus) THEN
   DELETE PROCEDURE h-bodi159sus.

