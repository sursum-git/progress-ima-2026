FOR EACH mp-entr-mat WHERE mp-entr-mat.nro-docto = 949.
    FOR EACH mp-fardo WHERE mp-fardo.nr-cdr = mp-entr-mat.nr-cdr.
     /*   ASSIGN mp-fardo.padrao = REPLACE(mp-fardo.padrao,"GO","BA"). */
        DISP mp-fardo.padrao. 
        
    END.
   /* ASSIGN mp-entr-mat.procedencia = "BA". */
    DISP mp-entr-mat.nro-docto
         mp-entr-mat.cod-emit
         mp-entr-mat.procedencia
         mp-entr-mat.dt-recebimento.
    
END.
