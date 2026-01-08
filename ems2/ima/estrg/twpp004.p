TRIGGER PROCEDURE FOR WRITE OF pp-ped-item OLD BUFFER b-pp-ped-item.

FIND pp-it-container  WHERE 
         pp-it-container.nr-container = pp-ped-item.nr-container    AND
         pp-it-container.it-comprado  = pp-ped-item.it-codigo       AND
         pp-it-container.ref-comprada = pp-ped-item.cod-refer
         EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL pp-it-container THEN DO:
         /* MESSAGE 'qt-vendida' pp-it-container.qt-vendida '=' 'qt-vendida' pp-it-container.qt-vendida '-' SKIP
               "( qt-pedida" pp-ped-item.qt-pedida "- qt-pedida-old"  b-pp-ped-item.qt-pedida ")"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
       ASSIGN pp-it-container.qt-vendida = pp-it-container.qt-vendida + (pp-ped-item.qt-pedida - b-pp-ped-item.qt-pedida). 
       /*MESSAGE 'novo valor' pp-it-container.qt-vendida 
           VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    END.
    ELSE
        /*MESSAGE "NÆo encontrou"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
