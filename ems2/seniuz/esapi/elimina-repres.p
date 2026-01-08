DEF INPUT PARAMETER p-nr-pedido LIKE ped-repre.nr-pedido.
DEF INPUT PARAMETER p-nome-ab-rep LIKE ped-repre.nome-ab-rep.

DEF TEMP-TABLE tt-ped-repre LIKE ped-repre 
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF VAR h-bodi157    AS HANDLE.
DEF VAR l-erro       AS LOG.
    
IF NOT VALID-HANDLE(h-bodi157) OR 
   h-bodi157:TYPE      <> "PROCEDURE":U OR
   h-bodi157:FILE-NAME <> "dibo/bodi157.p":U THEN
   RUN dibo/bodi157.p PERSISTENT SET h-bodi157.

FIND ped-repre WHERE
     ped-repre.nr-pedido = p-nr-pedido AND
     ped-repre.nome-ab-rep = p-nome-ab-rep
     NO-LOCK NO-ERROR.

IF AVAIL ped-repre THEN DO.
   CREATE tt-ped-repre.
   BUFFER-COPY ped-repre TO tt-ped-repre
        ASSIGN tt-ped-repre.r-rowid = ROWID(ped-repre).

   RUN setConstraintRowid IN h-bodi157 (INPUT ROWID(ped-repre)).

   RUN openQueryStatic IN h-bodi157 (INPUT "rowid":U).        
   RUN setRecord IN h-bodi157 (INPUT TABLE tt-ped-repre).
    
   RUN emptyRowErrors IN h-bodi157.
   RUN DeleteRecord IN h-bodi157.
   RUN getRowErrors IN h-bodi157 (OUTPUT TABLE RowErrors).
        
   IF CAN-FIND(FIRST RowErrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
      ASSIGN l-erro = NO.  
      FOR EACH rowerrors WHERE
               RowErrors.ErrorSubType = "ERROR":U:
    
          IF rowerrors.errornumber = 3 OR
             rowerrors.errornumber = 8 OR
             rowerrors.errornumber = 10
             THEN NEXT.
            
          MESSAGE "Erro ao Eliminar o Represente " p-nome-ab-rep SKIP
                  "Erro:" rowerrors.errornumber " - " 
                   rowerrors.errordescription 
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.
          ASSIGN l-erro = YES.   
      END.
      IF l-erro THEN
         RETURN 'ADM-ERROR'.
   END.
    
END.

IF VALID-HANDLE(h-bodi157) THEN
   DELETE PROCEDURE h-bodi157.


