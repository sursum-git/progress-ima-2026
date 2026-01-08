FOR EACH titulo WHERE titulo.cod-estabel = '2'
                  AND titulo.nr-docto    = '0005226'
                NO-LOCK.
    DISP titulo.cod-estabel
         titulo.cod-esp
         titulo.nr-docto
         titulo.parcela
         titulo.dt-emissao
         titulo.dt-venc.
END.
