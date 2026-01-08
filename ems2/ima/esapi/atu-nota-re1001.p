/* Variaveis do Recebimento Fiscal ---                                  */
DEF TEMP-TABLE tt-docum-est NO-UNDO LIKE docum-est 
    FIELD r-rowid AS ROWID.
 
/* Temp-Table tt-erro Definitions ---*/
{method/dbotterr.i}

DEF VAR h-boin090 AS HANDLE.

IF NOT VALID-HANDLE(h-boin090) OR
   h-boin090:TYPE      <> "PROCEDURE":U OR
   h-boin090:FILE-NAME <> "inbo/boin090.p":U THEN
   RUN inbo/boin090.p PERSISTENT SET h-boin090.

DEF INPUT PARAMETER TABLE FOR tt-docum-est.

FIND FIRST tt-docum-est SHARE-LOCK NO-ERROR.

FIND docum-est WHERE
     docum-est.serie-docto = tt-docum-est.serie-docto AND
     docum-est.nro-docto = tt-docum-est.nro-docto AND
     docum-est.cod-emit = tt-docum-est.cod-emit AND
     docum-est.nat-operacao = tt-docum-est.nat-operacao
     SHARE-LOCK NO-ERROR.

/* Cria Temp-Table para o Recebimento*/
ASSIGN tt-docum-est.r-rowid = ROWID(docum-est).

/* Atualiza Documento */
RUN openQueryStatic IN h-boin090 (INPUT "Main":U).
RUN emptyRowErrors IN h-boin090.
RUN repositionRecord IN h-boin090 (input tt-docum-est.r-rowid).
RUN AtualizaDocumento IN h-boin090.
RUN getRowErrors IN h-boin090 (OUTPUT TABLE RowErrors).
IF CAN-FIND(FIRST RowErrors 
           WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:

   FOR EACH rowerrors WHERE
            RowErrors.ErrorSubType = "ERROR":U:
       MESSAGE "Erro ao Atualizar o Documento" SKIP
                rowerrors.errordescription 
               VIEW-AS ALERT-BOX.
   END.
   IF VALID-HANDLE(h-boin090) THEN
      DELETE PROCEDURE h-boin090.

   RETURN 'ADM-ERROR'.
END.

IF VALID-HANDLE(h-boin090) THEN
   DELETE PROCEDURE h-boin090.


