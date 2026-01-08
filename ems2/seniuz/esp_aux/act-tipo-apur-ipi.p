FOR EACH ITEM.

    DISP ITEM.it-codigo.
    PAUSE 0.

    ASSIGN SUBSTR(ITEM.char-1,191,1) = '2'.

    FIND classif-fisc WHERE 
         classif-fisc.class-fiscal = item.class-fiscal EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL classif-fisc THEN
       ASSIGN SUBSTR(classif-fisc.char-2,1,1) = "2".
END.

