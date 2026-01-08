DEF VAR c-aux AS CHAR INIT "00030,01030,01020,01040,01050,01010". /*,09010".*/
DEF VAR c-item LIKE ITEM.it-codigo.

OUTPUT TO "c:/temp/ncm-erro.csv".
PUT "Item;Descricao;Refer;Descricao;Class-Fiscal;Cod-NCM" SKIP.

FOR EACH ref-item-ext WHERE INDEX(c-aux,SUBSTR(ref-item-ext.cod-refer,3,5)) = 0
                      NO-LOCK:
    FIND ITEM WHERE ITEM.it-codigo = ref-item-ext.it-codigo NO-LOCK.
    FIND referencia WHERE referencia.cod-refer = ref-item-ext.cod-refer NO-LOCK.

    IF ref-item-ext.cod-ncm <> ITEM.class-fiscal THEN DO.
       IF c-item <> "" AND c-item <> ITEM.it-codigo THEN
          PUT SKIP(1).
       PUT ref-item-ext.it-codigo ";"
           ITEM.desc-item ";"
           ref-item-ext.cod-refer FORMAT "99 9999 9" ";"
           referencia.descricao ";"
           ITEM.class-fiscal ";"
           ref-item-ext.cod-ncm
           SKIP.
       ASSIGN c-item = ITEM.it-codigo.
    END.
END.
OUTPUT CLOSE.
