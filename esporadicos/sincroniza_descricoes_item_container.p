FOR EACH pp-container
    WHERE pp-container.situacao = 1.
    FOR EACH pp-it-container OF pp-container :
        FIND FIRST ITEM 
            WHERE pp-it-container.it-codigo = ITEM.it-codigo
            NO-LOCK NO-ERROR.
        IF AVAIL ITEM THEN DO:
            ASSIGN pp-it-container.desc-item = ITEM.desc-item.
        END.
       
    END.

END.
