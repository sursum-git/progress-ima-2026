
FOR EACH nota-fiscal WHERE
         nota-fiscal.dt-emis >= 07.01.2023 AND 
         nota-fiscal.dt-emis <= 07.31.2023 NO-LOCK. 

    DISP nota-fiscal.nr-nota-fis.
    PAUSE 0.

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
    
        FIND ITEM WHERE
             ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
    
        IF NOT it-nota-fisc.it-codigo BEGINS 'DD' THEN DO.
           FIND nar-it-nota WHERE 
                nar-it-nota.cod-estabel  = it-nota-fisc.cod-estabel AND
                nar-it-nota.serie        = it-nota-fisc.serie AND
                nar-it-nota.nr-nota-fis  = it-nota-fisc.nr-nota-fis AND
                nar-it-nota.nr-sequencia = it-nota-fisc.nr-seq-fat AND
                nar-it-nota.it-codigo    = it-nota-fisc.it-codigo
                SHARE-LOCK NO-ERROR.
        
           IF NOT avail nar-it-nota then DO:
              CREATE nar-it-nota.
              ASSIGN nar-it-nota.cod-estabel  = it-nota-fisc.cod-estabel
                     nar-it-nota.serie        = it-nota-fisc.serie      
                     nar-it-nota.nr-nota-fis  = it-nota-fisc.nr-nota-fis
                     nar-it-nota.nr-sequencia = it-nota-fisc.nr-seq-fat 
                     nar-it-nota.it-codigo    = it-nota-fisc.it-codigo.
           END.
           ASSIGN nar-it-nota.narrativa = ENTRY(1,ITEM.narrativa,CHR(10)).
    
           IF it-nota-fisc.cod-refer <> "" THEN DO:
              ASSIGN nar-it-nota.narrativa = nar-it-nota.narrativa + " REF: " + it-nota-fisc.cod-refer. /* Adiciona Referencia do item*/
    
              FIND item-ext WHERE 
                   item-ext.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR. /* Adiciona Gramatura do item*/
    
              IF AVAIL item-ext AND item-ext.gramatura > 0 THEN
                 ASSIGN nar-it-nota.narrativa = nar-it-nota.narrativa + "  G/M: " + STRING(item-ext.gramatura).
              IF ITEM.cod-imagem <> "" THEN /* Adiciona Regra de lavagem do item*/
                 ASSIGN nar-it-nota.narrativa = nar-it-nota.narrativa + "  RL: " + ITEM.cod-imagem. 
           END.
        END.
    END.
END.


