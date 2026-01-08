OUTPUT TO c:\temp\descr_alteradas.txt.
FOR EACH pp-container
    WHERE pp-container.situacao = 1.
    FOR EACH pp-it-container OF pp-container:
        FIND FIRST ITEM NO-LOCK
            WHERE pp-it-container.it-comprado = ITEM.it-codigo
            NO-ERROR.
        IF AVAIL ITEM THEN DO:
           ASSIGN pp-it-container.desc-item = ITEM.desc-item.
           DISP pp-container.nr-container pp-it-container.it-codigo 
               ITEM.desc-item.
        END.
    END.
END.
OUTPUT CLOSE.
OS-COMMAND SILENT VALUE('start notepad c:\temp\descr_alteradas.txt').
