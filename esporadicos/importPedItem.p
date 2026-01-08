DEFINE TEMP-TABLE ttPedItem  LIKE ped-item.
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


