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

DEF VAR h-bodi159rct AS HANDLE.

FIND ped-venda WHERE
     ped-venda.nr-pedcli = p-nr-pedcli NO-ERROR.

IF NOT VALID-HANDLE(h-bodi159rct) OR
   h-bodi159rct:TYPE      <> "PROCEDURE":U OR
   h-bodi159rct:FILE-NAME <> "dibo/bodi159rct.p":U THEN
   RUN dibo/bodi159rct.p PERSISTENT SET h-bodi159rct.

RUN emptyRowErrors IN h-bodi159rct.

RUN ValidateReactivation IN h-bodi159rct (INPUT ROWID(ped-venda),
                                          OUTPUT TABLE RowErrors).
IF CAN-FIND(FIRST RowErrors 
            WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
       MESSAGE "Erro ao Reativar o Pedido" SKIP
               " Pedido:" ped-venda.nr-pedcli
               " Cliente:" ped-venda.nome-abrev SKIP
               "Erro:" rowerrors.errornumber " - " 
               rowerrors.errordescription 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
END.
ELSE
   RUN UpdateReactivation IN h-bodi159rct (INPUT ROWID(ped-venda),
                                           INPUT p-motivo).

IF VALID-HANDLE(h-bodi159rct) THEN
   DELETE PROCEDURE h-bodi159rct.

