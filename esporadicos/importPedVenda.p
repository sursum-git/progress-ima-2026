DEFINE TEMP-TABLE ttPedVenda LIKE ped-venda.
DEFINE TEMP-TABLE ttPedItem  LIKE ped-item.
DEFINE TEMP-TABLE ttCondPed  LIKE ped-item.


INPUT FROM c:\temp\ped-venda.txt.
   REPEAT:
       CREATE ttPedVenda.
       IMPORT DELIMITER "|" ttPedVenda.
   END.
INPUT CLOSE.


FOR EACH ttPedVenda:
    FIND FIRST ped-venda 
        WHERE ped-venda.cod-estabel =  ttPedVenda.cod-estabel
        AND   ped-venda.nr-pedcli   =  ttPedvenda.nr-pedcli
        AND   ped-venda.nome-abrev  =  ttPedVenda.nome-abrev    
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-venda THEN DO:
       CREATE ped-venda.
       BUFFER-COPY ttPedVenda TO ped-venda.
    END.
END.

INPUT FROM c:\temp\ped-item.txt.
   REPEAT:
       CREATE ttPedItem.
       IMPORT DELIMITER "|" ttPedItem.
   END.
INPUT CLOSE.


FOR EACH ttPedItem:
    FIND FIRST ped-item 
        WHERE ped-item.nr-pedcli   =  ttPeditem.nr-pedcli
        AND   ped-item.nome-abrev  =  ttPeditem.nome-abrev    
        AND   ped-item.it-codigo   =  ttPeditem.it-codigo
        AND   ped-item.cod-refer   =  ttPeditem.cod-refer
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-item THEN DO:
       CREATE ped-item.
       BUFFER-COPY ttPeditem TO ped-item.
    END.
END.


