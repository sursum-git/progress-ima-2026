FOR EACH mp-entr-mat WHERE
         mp-entr-mat.nro-docto = 610  AND
         mp-entr-mat.cod-emit  = 19354  SHARE-LOCK.
    DISP mp-entr-mat
          WITH WIDTH 550 SIDE-LABELS 1 COLUMN.
    
    FOR EACH mp-fardo WHERE
             mp-fardo.nr-cdr = mp-entr-mat.nr-cdr SHARE-LOCK.
        ASSIGN mp-fardo.padrao = "MG AGRICOLA XINGU".
        ASSIGN mp-entr-mat.padrao[1] = "MG AGRICOLA XINGU". 
        DISP mp-fardo.nr-cdr
             mp-fardo.padrao
             MP-ENTR-MAT.PADRAO[1]
             mp-fardo.peso(TOTAL) FORMAT ">>>,>>9.99"
             mp-fardo.situacao VIEW-AS FILL-IN.
    END.
    
END.
