PROCEDURE extrairInfCp5ConfEtq:

    DEFINE INPUT  PARAMETER pTexto      AS CHARACTER        NO-UNDO.
    DEFINE OUTPUT PARAMETER nrContainer AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER idEtqLisa   AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER localiz     AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.

    REPEAT iCont = 1 TO NUM-ENTRIES(pTexto,"|"):
        CASE iCont:
            WHEN 1  THEN
                ASSIGN nrContainer = INT(ENTRY(iCont,pTexto,"|")).
            WHEN 2 THEN
                ASSIGN idEtqLisa   = ENTRY(iCont,pTexto,"|").
            WHEN 3 THEN
                ASSIGN localiz    = ENTRY(iCont,pTexto,"|").
        END CASE.                                           
    END.


END PROCEDURE.
