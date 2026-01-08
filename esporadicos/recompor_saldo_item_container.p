DEFINE VARIABLE dTotal AS DECIMAL     NO-UNDO.
DEFINE VARIABLE i-container AS INTEGER     NO-UNDO.
DEFINE VARIABLE cPedidos AS CHARACTER      NO-UNDO FORMAT 'x(500)'.
UPDATE i-container.

OUTPUT TO c:\temp\recomp_saldo_item_container.csv.
PUT "container;item;refer;qt.vendida; qt.calc; qt.dif; pedidos" SKIP.

FOR EACH pp-container  WHERE 
    pp-container.nr-container = i-container NO-LOCK:
    FOR EACH pp-it-container OF pp-container:
        
        ASSIGN dTotal = 0.
        ASSIGN cpedidos = ''.
        FOR EACH ped-venda-ext NO-LOCK
            WHERE ped-venda-ext.nr-container = pp-container.nr-container.
            /*MESSAGE 'encontrei o pedido :' ped-venda-ext.nr-pedido
                VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
            FIND FIRST ped-venda
                WHERE ped-venda.nr-pedido   = ped-venda-ext.nr-pedido
                /*AND ped-venda.nome-abrev    = ped-venda-ext.nome-abrev*/
                /*AND (ped-venda.cod-sit-ped = 1 OR ped-venda.cod-sit-ped = 5)*/
                AND ped-venda.cod-sit-ped <> 3 
                NO-LOCK NO-ERROR.
            IF AVAIL ped-venda THEN DO:
               /*DISP ped-venda.nr-pedido.*/
                
               FOR EACH ped-item OF ped-venda 
                   WHERE ped-item.cod-sit-item <> 3 
                   AND  ped-item.it-codigo     = pp-it-container.it-codigo
                   AND   ped-item.cod-refer       = pp-it-container.cod-refer NO-LOCK.
                   ASSIGN dTotal = dTotal + ped-item.qt-pedida.
                   ASSIGN cPedidos = cPedidos + "," + STRING(ped-venda.nr-pedido).
               END.
            END.
            
        END.
        EXPORT DELIMITER ";" i-container pp-it-container.it-codigo pp-it-container.cod-refer pp-it-container.qt-vendida dtotal COLUMN-LABEL "calc." pp-it-container.qt-vendida - dtotal COLUMN-LABEL  "dif" cpedidos.


    END.
END.
