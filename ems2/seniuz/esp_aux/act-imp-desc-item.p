FOR EACH ITEM WHERE
         ITEM.it-codigo BEGINS '5' AND
         ITEM.ge-codigo >= 50 AND
         ITEM.ge-codigo <= 59.

     ASSIGN ITEM.int-2 = ITEM.ind-imp-desc
            ITEM.ind-imp-desc = 4.
END.

