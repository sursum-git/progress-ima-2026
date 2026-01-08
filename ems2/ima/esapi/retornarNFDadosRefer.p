/*retornar o documento referenciado da nota fiscal seja de 
entrada ou de saida*/
DEFINE INPUT  PARAMETER pEstabel     AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pDocumento   AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
DEFINE INPUT  PARAMETER pSerie       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNatOperacao AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
DEFINE INPUT  PARAMETER pEmitente    AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER cNota        AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
DEFINE OUTPUT PARAMETER daData       AS DATE  NO-UNDO .
DEFINE OUTPUT PARAMETER cChave       AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cSerie       AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER iEmitente    AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER cDescEmitente AS CHARACTER   NO-UNDO FORMAT 'x(30)'.



FIND FIRST nota-fisc-adc WHERE 
           nota-fisc-adc.cod-estab          = pEstabel          AND
           nota-fisc-adc.cod-serie          = pSerie            AND
           nota-fisc-adc.cod-nota-fisc      = pDocumento        AND
           nota-fisc-adc.cod-natur-operac   = pNatOperacao      AND
           nota-fisc-adc.cdn-emitente       = pEmitente         AND
           nota-fisc-adc.idi-tip-dado       = 3 NO-LOCK NO-ERROR .
IF AVAIL nota-fisc-adc THEN DO:
   FIND emitente
       WHERE emitente.cod-emitente = nota-fisc-adc.cdn-emitente
       NO-LOCK NO-ERROR.
   ASSIGN cNota        = nota-fisc-adc.cod-docto-referado
          daData       = nota-fisc-adc.dat-docto-referado 
          cSerie       = nota-fisc-adc.cod-ser-docto-referado
          iEmitente    = nota-fisc-adc.cdn-emitente
          cDescEmitente = IF AVAIL emitente THEN emitente.nome-emit ELSE ''
          cChave      = substr(nota-fisc-adc.cod-livre-2,1,44). 
END.
ELSE DO:
   ASSIGN cNota      = ''
          daData     = ?
          cSerie     = ''
          iEmitente  = 0
          cDescEmitente = ''
          cChave = ''. 

END.

