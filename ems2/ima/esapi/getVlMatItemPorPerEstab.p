DEFINE INPUT  PARAMETER pEstab  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pDtEmis AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER pItem   AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER vlMat   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE periodo         AS DATE        NO-UNDO.
DEFINE VARIABLE periodoPesq     AS DATE        NO-UNDO.
DEFINE VARIABLE iCont           AS INTEGER     NO-UNDO.
//primeiro dia no mes da emiss∆o da nota fiscal
ASSIGN periodo = DATE(MONTH(pDtEmis),1,YEAR(pDtEmis)).

/*volto para o primeiro dia do proximo màs posterior*/
ASSIGN periodo     = DATE(MONTH(periodo),1,YEAR(periodo)) .


REPEAT:
    ASSIGN iCont = iCont + 1.
    ASSIGN periodoPesq =   date(ADD-INTERVAL(datetime(periodo), iCont * -1 ,'months')).
    /*MESSAGE 'periodo que ser† pesquisado' SKIP
        periodoPEsq - 1 SKIP
        'contador:' iCont
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    FIND pr-it-per NO-LOCK
        WHERE pr-it-per.cod-estabel = pEstab
        AND   pr-it-per.it-codigo   = pItem
        AND   pr-it-per.periodo     = periodoPesq - 1
        NO-ERROR.
    IF AVAIL pr-it-per THEN DO:
       ASSIGN vlMat = pr-it-per.val-unit-mat-m[1].
       LEAVE.
    END.
    //evitar loop infinito
    IF iCont = 50 THEN LEAVE.
    
END.














