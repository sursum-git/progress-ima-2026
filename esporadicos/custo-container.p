OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "custo-container.csv.").
PUT "DATA;CONTAINER;PRODUTO;DESCRI€ÇO;REF.;CUSTO" SKIP.
FOR EACH pp-container WHERE pp-container.dt-recebimento >= 01.01.2024
                        AND pp-container.dt-recebimento <= TODAY :
                        
    FOR EACH pp-it-container NO-LOCK 
        WHERE pp-it-container.nr-container = pp-container.nr-container:
        
        
            
        FOR LAST item_custos  NO-LOCK
            WHERE item_custos.it_codigo = pp-it-container.it-codigo,
            EACH item_container_custos OF item_custos NO-LOCK,
            LAST  versoes_item_custo_container OF item_container_custos NO-LOCK
            WHERE versoes_item_custo_container.container_id = pp-container.nr-container  :
            FOR LAST ITEM 
                WHERE ITEM.it-codigo = item_custos.it_codigo
                NO-LOCK.
                
                
            END.
            EXPORT DELIMITER ";" pp-container.dt-recebimento pp-container.nr-container 
            pp-it-container.it-codigo
            IF AVAIL ITEM THEN ITEM.desc-item ELSE ""
            pp-it-container.cod-refer
            item_custos.vl_unit_novo * pp-it-container.qt-pedida.
            
            
        END.
           
            
    END.
    
END.
OUTPUT CLOSE.
