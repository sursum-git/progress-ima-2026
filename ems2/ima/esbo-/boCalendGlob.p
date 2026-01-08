/*
programa:esbo/boCalendGlob.p
Objetivo: Buscas as tabelas calend_glob e dia_calend_glob
*/

DEFINE VARIABLE dData   AS DATE        NO-UNDO.
DEFINE VARIABLE cCalend AS CHARACTER     NO-UNDO.

PROCEDURE setCalend:
    DEFINE INPUT  PARAMETER pCalend AS CHARACTER   NO-UNDO.
    ASSIGN cCalend = pCalend.

END PROCEDURE.

PROCEDURE setData:

    DEFINE INPUT PARAMETER pData AS DATE   NO-UNDO.
    ASSIGN dData = pData.

END PROCEDURE.

PROCEDURE verifDiaUtil:

    DEFINE OUTPUT PARAMETER lDiaUtil AS LOGICAL     NO-UNDO.
    FIND FIRST dia_calend_glob NO-LOCK
        WHERE dia_calend_glob.cod_calend = cCalend
        AND   dia_calend_glob.dat_calend = dData NO-ERROR.
    IF AVAIL dia_calend_glob THEN
       ASSIGN lDiaUtil = dia_calend_glob.LOG_dia_util .

END PROCEDURE.
