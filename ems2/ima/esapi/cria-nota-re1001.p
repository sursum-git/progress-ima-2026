/* Variaveis do Recebimento Fiscal ---                                  */
DEF TEMP-TABLE tt-docum-est NO-UNDO LIKE docum-est 
    FIELD r-rowid AS ROWID.
 
DEF TEMP-TABLE tt-item-doc-est NO-UNDO LIKE item-doc-est
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE wt-item-doc-est NO-UNDO LIKE item-doc-est
    FIELD r-rowid AS ROWID.

/* Temp-Table tt-erro Definitions ---*/
{method/dbotterr.i}

DEF VAR h-boin090      AS HANDLE.
DEF VAR h-boin176      AS HANDLE.

DEF INPUT PARAMETER TABLE FOR tt-docum-est.
DEF INPUT PARAMETER TABLE FOR tt-item-doc-est.

IF NOT VALID-HANDLE(h-boin090) OR
   h-boin090:TYPE      <> "PROCEDURE":U OR
   h-boin090:FILE-NAME <> "inbo/boin090.p":U THEN
   RUN inbo/boin090.p PERSISTENT SET h-boin090.

IF NOT VALID-HANDLE(h-boin176) OR 
   h-boin176:TYPE      <> "PROCEDURE":U OR
   h-boin176:FILE-NAME <> "inbo/boin176.p":U THEN
   RUN inbo/boin176.p PERSISTENT SET h-boin176.

/* Cria Recebimento Fiscal */
RUN openQueryStatic IN h-boin090 (INPUT "Main":U).
RUN emptyRowErrors IN h-boin090.
RUN setRecord IN h-boin090 (INPUT TABLE tt-docum-est).
RUN createRecord IN h-boin090.
RUN getRowErrors IN h-boin090 (OUTPUT TABLE RowErrors).

IF CAN-FIND(FIRST RowErrors WHERE 
                  RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:

   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
       MESSAGE "Erro ao Gerar o Recebimento" SKIP
                rowerrors.errordescription 
               VIEW-AS ALERT-BOX.
   END.
   IF VALID-HANDLE(h-boin090) THEN
      DELETE PROCEDURE h-boin090.

   IF VALID-HANDLE(h-boin176) THEN
      DELETE PROCEDURE h-boin176.

   RETURN 'ADM-ERROR'.
END.


/* Cria Itens do Recebimento*/
RUN openQueryStatic IN h-boin176 (INPUT "Main":U).
RUN emptyRowErrors IN h-boin176.

FOR EACH tt-item-doc-est:
    EMPTY TEMP-TABLE wt-item-doc-est.
    CREATE wt-item-doc-est.
    BUFFER-COPY tt-item-doc-est TO wt-item-doc-est.

    RUN setRecord IN h-boin176 (INPUT TABLE wt-item-doc-est).
    RUN createRecord IN h-boin176.
END.
RUN getRowErrors IN h-boin176 (OUTPUT TABLE RowErrors).
IF CAN-FIND(FIRST RowErrors 
           WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:

   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
       MESSAGE "Erro ao Gerar os Itens do Recebimento" SKIP
                rowerrors.errordescription 
               VIEW-AS ALERT-BOX.
   END.
   IF VALID-HANDLE(h-boin090) THEN
      DELETE PROCEDURE h-boin090.

   IF VALID-HANDLE(h-boin176) THEN
      DELETE PROCEDURE h-boin176.

   RETURN 'ADM-ERROR'.
END.


IF VALID-HANDLE(h-boin090) THEN
   DELETE PROCEDURE h-boin090.

IF VALID-HANDLE(h-boin176) THEN
   DELETE PROCEDURE h-boin176.
          

