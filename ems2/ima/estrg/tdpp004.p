TRIGGER PROCEDURE FOR DELETE OF pp-ped-item.

FIND pp-it-container  WHERE 
     pp-it-container.nr-container = pp-ped-item.nr-container    AND
     pp-it-container.it-comprado  = pp-ped-item.it-codigo       AND
     pp-it-container.ref-comprada = pp-ped-item.cod-refer
     EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL pp-it-container THEN
   ASSIGN pp-it-container.qt-vendida = pp-it-container.qt-vendida - pp-ped-item.qt-pedida. 


                               
