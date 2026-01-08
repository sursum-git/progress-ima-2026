DEF INPUT PARAMETER p-nr-pedido LIKE ped-venda.nr-pedido.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF VAR h-bodi159del AS HANDLE.

FIND ped-venda WHERE
     ped-venda.nr-pedido = p-nr-pedido NO-ERROR.

IF NOT VALID-HANDLE(h-bodi159del) OR
   h-bodi159del:TYPE      <> "PROCEDURE":U OR
   h-bodi159del:FILE-NAME <> "dibo/bodi159del.p":U THEN
   RUN dibo/bodi159del.p PERSISTENT SET h-bodi159del.

RUN emptyRowErrors IN h-bodi159del.
RUN ValidateDelete IN h-bodi159del (INPUT ROWID(ped-venda),
                                    OUTPUT TABLE RowErrors).
IF CAN-FIND(FIRST RowErrors 
            WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:

       MESSAGE "Erro ao Eliminar o Pedido" SKIP
               " Pedido:" ped-venda.nr-pedcli
               " Cliente:" ped-venda.nome-abrev
               "Erro:" rowerrors.errornumber " - " 
               rowerrors.errordescription 
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
END.
ELSE
   RUN UpdateDelete IN h-bodi159del (INPUT ROWID(ped-venda)).

IF VALID-HANDLE(h-bodi159del) THEN
   DELETE PROCEDURE h-bodi159del.

