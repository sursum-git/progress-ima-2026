DEF TEMP-TABLE tt-ped-venda LIKE ped-venda
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
    FIELD ErrorSequence    AS INTEGER
    FIELD ErrorNumber      AS INTEGER
    FIELD ErrorDescription AS CHARACTER
    FIELD ErrorParameters  AS CHARACTER
    FIELD ErrorType        AS CHARACTER
    FIELD ErrorHelp        AS CHARACTER
    FIELD ErrorSubType     AS CHARACTER.

DEF VAR h-bodi159 AS HANDLE.
DEF VAR h-bodi159com AS HANDLE.

FOR EACH tt-ped-venda.
    DELETE tt-ped-venda.
END.
FIND emitente 9493.

FIND ped-venda WHERE
     ped-venda.nome-abrev = emitente.nome-abrev AND
     ped-venda.nr-pedcli = '64009' NO-LOCK NO-ERROR.

BUFFER-COPY ped-venda TO tt-ped-venda.

IF NOT VALID-HANDLE(h-bodi159) or
   h-bodi159:TYPE      <> "PROCEDURE":U OR
   h-bodi159:FILE-NAME <> "dibo/bodi159.p":U THEN
   RUN dibo/bodi159.p PERSISTENT SET h-bodi159.

RUN SetConstraintNrPedido IN h-bodi159 (INPUT tt-ped-venda.nr-pedcli).

RUN openQueryNrPedido in h-bodi159.        

/*
RUN completeOrder IN h-bodi159com (INPUT ROWID(tt-ped-venda),
                                   OUTPUT TABLE RowErrors).
*/

RUN emptyRowErrors IN h-bodi159.
RUN setRecord IN h-bodi159(INPUT TABLE tt-ped-venda).
RUN updateRecord        in h-bodi159.
RUN getRowErrors IN h-bodi159com(OUTPUT TABLE RowErrors).

IF CAN-FIND(FIRST RowErrors 
            WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
   FOR each rowerrors WHERE RowErrors.ErrorSubType = "ERROR":U:
       MESSAGE  rowerrors.errornumber
                rowerrors.errordescription VIEW-AS ALERT-BOX.
   END.
END.

IF VALID-HANDLE(h-bodi159) THEN
   DELETE PROCEDURE h-bodi159.

IF VALID-HANDLE(h-bodi159com) THEN
   DELETE PROCEDURE h-bodi159com.
