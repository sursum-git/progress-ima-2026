DEF VAR c-item LIKE ref-item-ext.it-codigo.
DEF VAR c-aux  AS CHAR FORMAT "x(2)".

REPEAT:
   ASSIGN c-aux = "".
   UPDATE c-item
          c-aux.
   FOR EACH ref-item-ext WHERE ref-item-ext.it-codigo = c-item 
                           AND (ref-item-ext.cod-refer BEGINS c-aux OR
                                c-aux = "")
                           AND ref-item-ext.cod-obsoleto = "2".
       ACCUMULATE ref-item-ext.cod-obsoleto(COUNT).
       
       ASSIGN ref-item-ext.cod-obsoleto = "1".
   END.
   DISP (ACCUM COUNT ref-item-ext.cod-obsoleto).
END.
