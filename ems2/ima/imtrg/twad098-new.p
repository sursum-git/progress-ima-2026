/****************************************************************************
** Programa : TWAD098 - trigger de Write para a tabela b-emit-new  - TONINHO
** Data     : Novmembro 2014
** Objetivo : trigger de Write para a tabela b-emit-new 
** Empresa  : IMA 
** Vers∆o   : 2.04.001
** Alterado : 
** Fluxo    : Integrar b-emit-new com as Demais Bases
*****************************************************************************/

DEFINE PARAMETER BUFFER b-emit-new FOR emitente.
DEFINE PARAMETER BUFFER b-emit-old FOR emitente.  

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR c-results  AS CHAR.
DEF VAR i-ct       AS INT.

IF AVAIL b-emit-new THEN DO:
   FIND ext-emitente WHERE
        ext-emitente.cod-emit = b-emit-new.cod-emit SHARE-LOCK NO-ERROR.
   IF NOT AVAIL ext-emitente THEN DO.
      CREATE ext-emitente.
      ASSIGN ext-emitente.cod-emit = b-emit-new.cod-emit.
   END.
   ASSIGN ext-emitente.log-integr-bkp = NO.


   BUFFER-COMPARE b-emit-new TO b-emit-old SAVE RESULT IN c-results.
   DO i-ct = 1 TO NUM-ENTRIES(c-results).
      CREATE his-emit.
      ASSIGN his-emit.cod-emitente = b-emit-new.cod-emitente
             his-emit.dt-his-emit  = TODAY
             his-emit.horario = STRING(TIME,"HH:MM").

      FIND mgcad._file WHERE
           mgcad._file._file-name = 'emitente' NO-LOCK NO-ERROR.

      FIND mgcad._field OF mgcad._file WHERE
           mgcad._field._field-name = ENTRY(i-ct,c-results) NO-LOCK NO-ERROR.

      ASSIGN his-emit.historico = "Usu†rio: " + c-seg-usuario + " Alterou " + mgcad._field._label + " de " + STRING(b-emit-old.cod-rep) + " para: " + STRING(b-emit-new.cod-rep).
   END.
END.


