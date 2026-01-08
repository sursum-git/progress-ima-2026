DEFINE TEMP-TABLE tt
    FIELD itCodigo      AS CHAR
    FIELD codRefer      AS CHAR
    FIELD nroDocto      AS CHAR
    FIELD serie         AS CHAR
    FIELD dtTransacao   AS DATE .

OUTPUT TO c:\temp\refers_subs.txt.
FOR EACH docum-est NO-LOCK
    WHERE docum-est.dt-trans >= 12.07.2022 .
    FOR EACH item-doc-est OF docum-est NO-LOCK.
        FIND referencias_subst 
            WHERE referencias_subst.cod_refer  = item-doc-est.cod-refer
            NO-LOCK NO-ERROR  .
        IF AVAIL referencias_subst THEN DO:
           CREATE tt.
           ASSIGN tt.itCodigo     = item-doc-est.it-codigo
                  tt.codRefer     = item-doc-est.cod-refer
                  tt.nroDocto     = item-doc-est.nr-docto
                  tt.serie        = item-doc-est.serie-docto
                  tt.dtTransacao  = docum-est.dt-trans .
        END.
    END.    
END.
OUTPUT CLOSE.
