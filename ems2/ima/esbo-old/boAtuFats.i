DEFINE TEMP-TABLE ttContainerPend NO-UNDO
    FIELD nrContainer  AS INT
    FIELD itCodigo     AS CHAR
    FIELD codRefer     AS CHAR  
    FIELD qtFaturada   AS DECIMAL FORMAT '->>>,>>>,>>>,>>9.99'
    INDEX ind-unico    IS PRIMARY  nrContainer itCodigo codRefer .

DEFINE TEMP-TABLE ttDatas NO-UNDO LIKE fats_99.
DEFINE TEMP-TABLE ttTransacao NO-UNDO
    FIELD idTransacao AS INT64
    .

DEFINE TEMP-TABLE ttAnoMes NO-UNDO
    FIELD ano AS INT
    FIELD mes AS INT
    INDEX unico IS PRIMARY ano mes . 


PROCEDURE _calcTtAnoMes:

    DEFINE INPUT  PARAMETER pDtIni  AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pDtFim  AS DATE        NO-UNDO.

    DEFINE VARIABLE iCont       AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iInterval   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE data        AS DATE        NO-UNDO.

    ASSIGN iInterval = pDtFim - pDtIni .
    IF iInterval = 0 THEN
       ASSIGN iInterval = 1.
    REPEAT icont = 1 TO iInterval : 
        IF iCont > 1 THEN
           ASSIGN data = data + 1.
        ELSE
           ASSIGN data = pDtIni. 
        FIND ttAnomes
            WHERE ttAnoMes.ano = YEAR(data)
            AND   ttAnoMes.mes = MONTH(data) NO-ERROR.
        IF NOT AVAIL ttAnoMes THEN DO:
           CREATE ttAnoMes.
           ASSIGN 
           ttAnoMes.ano = YEAR(data)  
           ttAnoMes.mes = MONTH(data) .
        END.                           
    END.                               


END PROCEDURE.
