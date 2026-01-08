FOR EACH ITEM WHERE
         ITEM.ge-codigo >= 50 AND
         ITEM.ge-codigo <= 60 NO-LOCK.

    DISP ITEM.it-codigo.
    PAUSE 0.

    FOR EACH item-uni-estab WHERE
             item-uni-estab.it-codigo = ITEM.it-codigo SHARE-LOCK.
        ASSIGN SUBSTR(item-uni-estab.char-1,133,1) = '0'.
    END.

END.

