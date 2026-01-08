def temp-table tt-fatorconv
    FIELD ITEM  AS char
    FIELD fator AS char.
    
input from "c:/temp/fatorconv.csv".
SET ^.

repeat:
   create tt-fatorconv.
   import delimiter ";" tt-fatorconv.
end.
input close.

FOR EACH tt-fatorconv.
    IF tt-fatorconv.ITEM = "" THEN NEXT.
    FIND item-ext WHERE item-ext.it-codigo = tt-fatorconv.ITEM NO-ERROR.
    FIND ITEM WHERE ITEM.it-codigo = tt-fatorconv.ITEM NO-LOCK.
    IF NOT AVAIL ITEM THEN DO:
       MESSAGE "Item " tt-fatorconv.ITEM "NÆo existe!"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       NEXT.
    END.
    IF NOT AVAIL item-ext THEN DO.
       CREATE item-ext.
       ASSIGN item-ext.it-codigo = tt-fatorconv.ITEM.
    END.
    ASSIGN item-ext.fator-conv = DEC(tt-fatorconv.fator).
END.
