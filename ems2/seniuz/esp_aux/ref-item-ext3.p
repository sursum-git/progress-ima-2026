DEF VAR c-ok AS CHAR.

OUTPUT TO "c:/temp/obsoleto_2.csv".
PUT "Item;Refer;Descricao;C-Obsol;C-Obsol-Item" SKIP.

FOR EACH ref-item-ext WHERE ref-item-ext.cod-obsoleto = "2" NO-LOCK.
    FIND ITEM WHERE ITEM.it-codigo = ref-item-ext.it-codigo NO-LOCK.
    FIND FIRST item-ext WHERE item-ext.it-codigo = ITEM.it-codigo NO-LOCK.
    IF item-ext.cod-obsoleto <> ref-item-ext.cod-obsoleto THEN
       ASSIGN c-ok = item-ext.cod-obsoleto.
    ELSE
       ASSIGN c-ok = "".

    PUT ref-item-ext.it-codigo ";"
        ref-item-ext.cod-refer FORMAT "99 9999 9" ";"
        ITEM.desc-item ";"
        ref-item-ext.cod-obsoleto ";"
        c-ok
        SKIP.
END.
OUTPUT CLOSE.

