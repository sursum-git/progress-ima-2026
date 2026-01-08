
{esp/params.i}

PROCEDURE verifIntegrLisa:
    DEFINE INPUT  PARAMETER pEstab      AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER lIntegra    AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE cEstabsIntegraLISA  AS CHARACTER   NO-UNDO.
    
    
    ASSIGN cEstabsIntegraLISA = getEstabsIntegraLISA().
           lIntegra           = LOOKUP(pEstab,cEstabsIntegraLISA,",") > 0 
           .
    

END PROCEDURE.
