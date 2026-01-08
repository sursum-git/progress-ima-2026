/*OUTPUT TO "c:/lixo/lixo.txt".*/

FOR EACH ITEM WHERE ITEM.it-codigo BEGINS "50" 
                AND substr(item.it-codigo,6,1) <> "0"
              NO-LOCK,
    EACH item-uni-estab WHERE item-uni-estab.it-codigo    = ITEM.it-codigo
                          AND item-uni-estab.cod-estabel  = "2"
                          AND item-uni-estab.ind-item-fat = NO
                        NO-LOCK:
    DISPLAY ITEM-uni-estab.it-codigo
            ITEM.desc-item FORMAT "x(36)"
            item-uni-estab.cod-estabel. 
END.

/*OUTPUT CLOSE.*/

