DEF VAR i-ct    AS INT.
DEF VAR de-peso AS DEC.
FOR EACH mp-entr-mat WHERE
         mp-entr-mat.nro-docto = 17371  AND
         mp-entr-mat.cod-emit  = 14177  SHARE-LOCK.
    DISP mp-entr-mat
          WITH WIDTH 550 SIDE-LABELS 1 COLUMN.
    
    FOR EACH mp-fardo WHERE
             mp-fardo.nr-cdr = mp-entr-mat.nr-cdr SHARE-LOCK.
             DISP mp-fardo.nr-fardo
                  mp-fardo.situacao VIEW-AS FILL-IN.
             ASSIGN i-ct = i-ct + 1
                    de-peso = de-peso + mp-fardo.peso.
             ASSIGN mp-fardo.situacao = 4. 
    END.
    
END.
DISP i-ct
     de-peso.
