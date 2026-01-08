OUTPUT TO c:/temp/ITEM.csv CONVERT SOURCE "ibm850".
PUT "C‡DIGO;DESCRIÄ«O;UN" SKIP.
FOR EACH ITEM WHERE it-codigo BEGINS '7' NO-LOCK.
    PUT ITEM.it-codigo ";"
        ITEM.desc-item ";"
        ITEM.un
        SKIP.
END.
OUTPUT CLOSE.
