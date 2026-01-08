DEFINE VARIABLE lAchou AS LOGICAL     NO-UNDO.
OUTPUT TO c:\temp\documentos.txt.
FOR EACH docum-est NO-LOCK
    WHERE docum-est.nat-operacao = '19207i'
    AND docum-est.cod-estabel = '505':
    ASSIGN lAchou = NO.
    FOR EACH item-doc-est OF docum-est EXCLUSIVE-LOCK
        WHERE item-doc-est.ct-codigo = '' .
        ASSIGN  lAchou = YES
                 item-doc-est.ct-codigo = '19000017' .

    END.

    IF lAchou THEN
       DISP docum-est.serie docum-est.nro-docto docum-est.cod-emitente .
END.
