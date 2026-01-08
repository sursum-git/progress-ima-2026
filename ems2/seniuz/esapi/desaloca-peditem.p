DEF INPUT PARAMETER p-row-ped-item AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF VAR h-bodi154 AS HANDLE.

IF NOT VALID-HANDLE(h-bodi154) or
   h-bodi154:TYPE      <> "PROCEDURE":U OR
   h-bodi154:FILE-NAME <> "dibo/bodi154.p":U THEN
   RUN dibo/bodi154.p PERSISTENT SET h-bodi154.

FIND ped-item WHERE
     ROWID(ped-item) = p-row-ped-item NO-LOCK.

RUN unallocateItem IN h-bodi154 (INPUT ROWID(ped-item)).

RUN getRowErrors IN h-bodi154 (OUTPUT TABLE RowErrors).

IF CAN-FIND(FIRST RowErrors 
           WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
       MESSAGE "Erro ao Desalocar Itens do Pedido" SKIP
                rowerrors.errordescription 
               VIEW-AS ALERT-BOX.
   END.
   RETURN 'NOK'.
END.

FIND CURRENT ped-item SHARE-LOCK NO-ERROR.
ASSIGN ped-item.qt-log-aloca = 0
       ped-item.qt-alocada = 0.
FIND CURRENT ped-item NO-LOCK NO-ERROR.

IF VALID-HANDLE(h-bodi154) THEN
   DELETE PROCEDURE h-bodi154.

