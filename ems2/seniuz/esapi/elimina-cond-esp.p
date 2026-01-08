DEF INPUT PARAMETER p-nr-pedido LIKE cond-ped.nr-pedido.
DEF INPUT PARAMETER p-nr-sequencia LIKE cond-ped.nr-sequencia.

DEF TEMP-TABLE tt-cond-ped LIKE cond-ped 
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF VAR h-bodi018    AS HANDLE.
DEF VAR h-bodi018com AS HANDLE.

IF NOT VALID-HANDLE(h-bodi018) or
   h-bodi018:TYPE      <> "PROCEDURE":U OR
   h-bodi018:FILE-NAME <> "dibo/bodi018.p":U THEN
   RUN dibo/bodi018.p PERSISTENT SET h-bodi018.

FIND cond-ped WHERE
     cond-ped.nr-pedido = p-nr-pedido AND
     cond-ped.nr-sequencia = p-nr-sequencia
     NO-LOCK NO-ERROR.

CREATE tt-cond-ped.
BUFFER-COPY cond-ped TO tt-cond-ped
       ASSIGN tt-cond-ped.r-rowid = ROWID(cond-ped). 

RUN setConstraintRowid IN h-bodi018 (INPUT ROWID(cond-ped)).
RUN openQueryStatic IN h-bodi018 (INPUT "rowid":U).        
RUN emptyRowErrors IN h-bodi018.
RUN setRecord IN h-bodi018(INPUT TABLE tt-cond-ped).
RUN deleteRecord IN h-bodi018.
RUN getRowErrors IN h-bodi018(OUTPUT TABLE RowErrors).
    
IF CAN-FIND(FIRST RowErrors 
            WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:

       IF rowerrors.errornumber = 3 OR
          rowerrors.errornumber = 8 OR
          rowerrors.errornumber = 10
          THEN NEXT.

       MESSAGE "Erro ao Eliminar a Condi‡Æo de Pagamento Especial" SKIP
               "Erro:" rowerrors.errornumber " - " 
               rowerrors.errordescription 
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
   END.
END.

IF VALID-HANDLE(h-bodi018) THEN
   DELETE PROCEDURE h-bodi018.


