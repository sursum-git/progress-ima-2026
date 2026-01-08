DEFINE VARIABLE cDoc         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEstab       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSerie       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNatOperacao AS CHARACTER   NO-UNDO.
UPDATE
    cDoc
    cEstab
    cSerie
    cNatOperacao WITH 1 COL.

FOR EACH  doc-fiscal
    WHERE doc-fiscal.nr-doc-fis = cDoc
    AND   doc-fiscal.cod-estabel = cEstab
    AND   doc-fiscal.serie = cSerie
    AND   doc-fiscal.nat-operacao = cNatOperacao NO-LOCK:
    FOR EACH it-doc-fisc OF doc-fiscal 
        WHERE VL-ICMSOU-IT = 0 EXCLUSIVE-LOCK.
        ASSIGN VL-ICMSOU-IT = VL-MERC-LIQ.
        DISP it-doc-fisc.it-codigo it-doc-fisc.vl-merc-liq it-doc-fisc.vl-icmsou-it.
    END.
    MESSAGE 'CONCLUIDO'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

END.
