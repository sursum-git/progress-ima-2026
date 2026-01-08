FOR EACH item-ext WHERE ITEM-ext.cod-obsoleto = "2":
    ACCUMULATE item-ext.it-codigo(COUNT).
END.
DISPLAY (ACCUM COUNT item-ext.it-codigo).
