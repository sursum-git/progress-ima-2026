OUTPUT TO "c:\temp\titulo.txt".
FOR EACH titulo WHERE titulo.vl-saldo <> 0 
                  AND titulo.vl-liquido <> titulo.vl-original NO-LOCK.
    put titulo.nr-docto ";"
        titulo.parcela ";"
        titulo.dt-emissao ";"
        titulo.vl-original ";"
        titulo.vl-saldo ";"
        titulo.vl-liquido
        SKIP.
END.
OUTPUT CLOSE.
