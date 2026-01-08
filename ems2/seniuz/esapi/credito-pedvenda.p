
DEF VAR h-bodi159 AS HANDLE.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

IF NOT VALID-HANDLE(h-bodi159) OR
   h-bodi159:TYPE      <> "PROCEDURE":U OR
   h-bodi159:FILE-NAME <> "dibo/bodi159com.p":U THEN
   RUN dibo/bodi159.p PERSISTENT SET h-bodi159.

RUN emptyRowErrors IN h-bodi159.
RUN validaCredito IN h-bodi159 (INPUT YES,
                                INPUT TODAY,
                                INPUT "",
                                INPUT 'super',
                                INPUT "apr").

IF CAN-FIND(FIRST RowErrors 
           WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:

  FOR EACH rowerrors WHERE
           RowErrors.ErrorSubType = "ERROR":U:
      MESSAGE "Erro ao Validar Credito do Pedido" SKIP
              "Erro:" rowerrors.errornumber " - " SKIP
               rowerrors.errordescription 
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  RETURN 'ADM-ERROR'.
END.

IF VALID-HANDLE(h-bodi159) THEN
   DELETE PROCEDURE h-bodi159.

