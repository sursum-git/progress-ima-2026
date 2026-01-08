/*************************************
Programa:boConsParam
Objetivo: Consulta de parametros 
Autor: Tadeu Silva Parreiras
Data: Outubro/2020
**************************************/
DEFINE VARIABLE cCodParam AS CHARACTER   NO-UNDO.

PROCEDURE setCodParam:

    DEFINE INPUT  PARAMETER pCodParam AS CHARACTER   NO-UNDO.
    ASSIGN cCodParam = pCodParam.
    FIND im-param
        WHERE im-param.cod-param = cCodParam
        NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE getVlParam:
    DEFINE OUTPUT PARAMETER vlParam AS CHARACTER   NO-UNDO.
    FIND CURRENT im-param NO-LOCK NO-ERROR.
    IF AVAIL im-param THEN
       ASSIGN vlParam = im-param.val-param.

END PROCEDURE.

PROCEDURE getQtMinMtBook:
    DEFINE OUTPUT PARAMETER vlParam AS CHARACTER   NO-UNDO.
    

    RUN setCodParam('qt_minima_mt_book').
    RUN getVlParam(OUTPUT vlParam).
    IF vlParam = '' THEN
       ASSIGN vlParam = '5'.

END PROCEDURE.


PROCEDURE getQtMinKgBook:
    DEFINE OUTPUT PARAMETER vlParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('qt_minima_kg_book').
    RUN getVlParam(OUTPUT vlParam).
    IF vlParam = '' THEN
       ASSIGN vlParam = '2'.

END PROCEDURE.

PROCEDURE getEmailsErrosArqDesign:
    DEFINE OUTPUT PARAMETER vlParam AS CHARACTER   NO-UNDO.

    RUN setCodParam('emails_erros_arq_design').
    RUN getVlParam(OUTPUT vlParam).
    

END PROCEDURE.

PROCEDURE getCalendAprovGer:
    DEFINE OUTPUT PARAMETER vlParam AS CHARACTER   NO-UNDO.
    RUN setCodParam('calend_aprov_ger').
    RUN getVlParam(OUTPUT vlParam).
    IF vlParam = '' THEN
       ASSIGN vlParam = 'FISCAL'.

END PROCEDURE.

