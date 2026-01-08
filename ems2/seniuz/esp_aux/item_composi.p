DEF TEMP-TABLE tt-work 
    FIELD cod-composi LIKE composi.cod-composi
    FIELD it-codigo   LIKE ITEM.it-codigo
    INDEX ch-work cod-composi
                  it-codigo.

FOR EACH ITEM WHERE ITEM.it-codigo BEGINS "5" NO-LOCK:
    FIND item-ext WHERE item-ext.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.
       
    FIND tt-work WHERE tt-work.cod-composi = ITEM-ext.cod-composi
                   AND tt-work.it-codigo   = ITEM.it-codigo
                 NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-work THEN DO:
       CREATE tt-work.
       ASSIGN tt-work.cod-composi = IF AVAIL item-ext THEN item-ext.cod-composi
                                                      ELSE ""
              tt-work.it-codigo   = ITEM.it-codigo.
    END.
END.
OUTPUT TO "c:/lixo/item-composi.txt".
FOR EACH tt-work BREAK BY tt-work.cod-composi:
    FIND composi WHERE composi.cod-composi = tt-work.cod-composi NO-LOCK NO-ERROR.
    FIND ITEM WHERE ITEM.it-codigo = tt-work.it-codigo NO-LOCK.
    DISP tt-work.cod-composi WHEN FIRST-OF(tt-work.cod-composi)
         composi.descricao   WHEN FIRST-OF(tt-work.cod-composi) AND AVAIL composi
         tt-work.it-codigo
         ITEM.desc-item FORMAT "x(36)"
         WITH WIDTH 132.
END.
OUTPUT CLOSE.
