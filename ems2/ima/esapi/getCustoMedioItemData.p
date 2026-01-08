DEFINE INPUT  PARAMETER pDtRefer    AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER pItem       AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER vlMedio     AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER dataValor   AS DATETIME    NO-UNDO.

DEFINE VARIABLE periodo             AS DATE        NO-UNDO.
DEFINE VARIABLE periodoPesq         AS DATE        NO-UNDO.
DEFINE VARIABLE iCont               AS INTEGER     NO-UNDO.

DEFINE VARIABLE lEncontrou  AS LOGICAL     NO-UNDO.


//primeiro dia no mes da data de referencia
ASSIGN periodo = DATE(MONTH(pDtRefer),1,YEAR(pDtRefer))
       periodo = date(ADD-INTERVAL(datetime(periodo), 1 ,'months')) .

REPEAT:
    IF iCont  > 0 THEN
       ASSIGN periodoPesq =  date(ADD-INTERVAL(datetime(periodo), iCont * -1 ,'months')).
    ELSE
       ASSIGN periodoPesq =  periodo.
    FIND FIRST pr-it-per NO-LOCK
        WHERE   pr-it-per.it-codigo   = pItem
        AND     pr-it-per.periodo     = periodoPesq - 1 NO-ERROR .
    IF AVAIL pr-it-per THEN DO:
        ASSIGN vlMedio      = pr-it-per.val-unit-mat-m[1] + pr-it-per.val-unit-mat-m[2] + pr-it-per.val-unit-mat-m[3] +
               pr-it-per.val-unit-mob-m[1] + pr-it-per.val-unit-mob-m[2] + pr-it-per.val-unit-mob-m[3] +
               pr-it-per.val-unit-ggf-m[1] + pr-it-per.val-unit-ggf-m[2] + pr-it-per.val-unit-ggf-m[3] +
               pr-it-per.val-unit-mat-p[1] + pr-it-per.val-unit-mat-p[2] + pr-it-per.val-unit-mat-p[3] +
               pr-it-per.val-unit-mob-p[1] + pr-it-per.val-unit-mob-p[2] + pr-it-per.val-unit-mob-p[3] +
               pr-it-per.val-unit-ggf-p[1] + pr-it-per.val-unit-ggf-p[2] + pr-it-per.val-unit-ggf-p[3] +
               pr-it-per.val-unit-mat-o[1] + pr-it-per.val-unit-mat-o[2] + pr-it-per.val-unit-mat-o[3] +
               pr-it-per.val-unit-mob-o[1] + pr-it-per.val-unit-mob-o[2] + pr-it-per.val-unit-mob-o[3] +
               pr-it-per.val-unit-ggf-o[1] + pr-it-per.val-unit-ggf-o[2] + pr-it-per.val-unit-ggf-o[3] 
               lEncontrou   = YES
               dataValor    = pr-it-per.periodo .
               LEAVE.
    END.
    
       
    //evitar loop infinito      
    IF iCont = 50 THEN LEAVE.

    ASSIGN iCont = iCont + 1.
    
END.














