
DEFINE INPUT  PARAMETER pSerie      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pDocto      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNatOper    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSeq        AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER dTotal      AS DECIMAL     NO-UNDO.



FOR EACH componente NO-LOCK
    WHERE componente.serie-docto    = pSerie
    AND   componente.nro-docto      = pDocto
    AND   componente.nat-operacao   = pNatOper
    AND   componente.sequencia      = pSeq
    AND   componente.componente     = 2
    :
    ASSIGN dTotal = dTotal + componente.quantidade.
    
END.
