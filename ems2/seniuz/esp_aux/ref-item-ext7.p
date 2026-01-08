OUTPUT TO "c:/temp/ncm-geral.csv" CONVERT SOURCE "ibm850".
PUT "Item;Descricao;Refer;Descricao;Familia;Descricao;GE;Descricao;Class-Fiscal;Cod-NCM;Diferen‡a?" SKIP.

FOR EACH ref-item-ext WHERE ref-item-ext.it-codigo BEGINS "5" NO-LOCK:
    FIND ITEM WHERE ITEM.it-codigo = ref-item-ext.it-codigo NO-LOCK.
    FIND referencia WHERE referencia.cod-refer = ref-item-ext.cod-refer
                    NO-LOCK NO-ERROR.
    FIND familia WHERE familia.fm-codigo = ITEM.fm-codigo NO-LOCK.
    FIND grup-estoq WHERE grup-estoq.ge-codigo = ITEM.ge-codigo NO-LOCK.

    PUT UNFORMAT 
        ref-item-ext.it-codigo ";"
        IF AVAIL ITEM THEN ITEM.desc-item ELSE ""
        ";"
        ref-item-ext.cod-refer FORMAT "99 9999 9" ";"
        IF AVAIL referencia THEN referencia.descricao ELSE ""
        ";"
        IF AVAIL familia THEN familia.fm-codigo ELSE ""
        ";"
        IF AVAIL familia THEN familia.descricao ELSE ""
        ";"
        IF AVAIL grup-estoq THEN grup-estoq.ge-codigo ELSE 0
        ";"
        IF AVAIL grup-estoq THEN grup-estoq.descricao ELSE ""
        ";" 
        ITEM.class-fiscal ";"
        ref-item-ext.cod-ncm ";"
        IF ref-item-ext.cod-ncm <> ITEM.class-fiscal THEN "Sim" ELSE "NÆo"
        SKIP.
END.
OUTPUT CLOSE.
