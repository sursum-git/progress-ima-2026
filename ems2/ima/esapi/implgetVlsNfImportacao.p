DEFINE VARIABLE cDescricao AS CHARACTER   NO-UNDO.
OUTPUT TO c:\temp\dados_imp_med_es.txt.
{esapi/getVlsNfImportacao.i ttDanfeAux}
FOR EACH nota-fiscal NO-LOCK
WHERE nota-fiscal.cod-estabel = '5'
/*
AND nota-fiscal.serie = '3'
AND nota-fiscal.nr-nota-fis = '0189244'
*/
AND nota-fiscal.dt-emis-nota >= 01.01.2023
AND nota-fiscal.nat-operacao  = '31201M' .

DISP nr-nota-fis.

    /*RUN esapi/getVlsNfImportacao.p(ROWID(nota-fiscal),OUTPUT TABLE ttDanfeAUX).
    FOR EACH ttDanfeAux:
        DISP ttDanfeAux WITH 1 COL.
    END.*/
    
    RUN esapi/getDescrNFImportacao.p(ROWID(nota-fiscal),OUTPUT cDescricao).
    DISP cDescricao FORMAT 'x(200)' WITH WIDTH 550  .
    


END.
