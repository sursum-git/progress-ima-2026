DEF VAR c-aux     LIKE ITEM.it-codigo.

DEF TEMP-TABLE tt-sintegra
    FIELD registro AS CHAR.

DEF TEMP-TABLE tt-codigos
    FIELD item-ant AS CHAR
    FIELD item-atu AS CHAR
    INDEX ch-codigo item-ant.
    
input from "c:/temp/SINTEGRA.txt".
repeat:
   create tt-sintegra.
   IMPORT DELIMITER "#&@" tt-sintegra.
end.
input close.

input from "c:/temp/conv_cod_loja_ind.csv".
repeat:
   create tt-codigos.
   IMPORT DELIMITER ";" tt-codigos.
end.
input close.

FOR EACH tt-sintegra.
    IF tt-sintegra.registro = "" THEN NEXT.

    IF tt-sintegra.registro BEGINS "60D" THEN DO:
       ASSIGN c-aux = TRIM(STRING(INT(SUBSTR(tt-sintegra.registro,32,14)))).
       FIND FIRST tt-codigos WHERE tt-codigos.item-ant = c-aux NO-LOCK.
       OVERLAY(tt-sintegra.registro,32,14) = tt-codigos.item-atu + FILL(" ",14 - LENGTH(c-aux)).
    END.
    IF tt-sintegra.registro BEGINS "60I" THEN DO:
       ASSIGN c-aux = TRIM(STRING(INT(SUBSTR(tt-sintegra.registro,43,14)))).
       FIND FIRST tt-codigos WHERE tt-codigos.item-ant = c-aux NO-LOCK.
       OVERLAY(tt-sintegra.registro,43,14) = tt-codigos.item-atu + FILL(" ",14 - LENGTH(c-aux)).
    END.
    IF tt-sintegra.registro BEGINS "60R" THEN DO:
       ASSIGN c-aux = TRIM(STRING(INT(SUBSTR(tt-sintegra.registro,10,14)))).
       FIND FIRST tt-codigos WHERE tt-codigos.item-ant = c-aux NO-LOCK.
       OVERLAY(tt-sintegra.registro,10,14) = tt-codigos.item-atu + FILL(" ",14 - LENGTH(c-aux)).
    END.
END.
OUTPUT TO c:/temp/sintegra1.txt.
FOR EACH tt-sintegra.
    PUT UNFORMAT
        tt-sintegra.registro
        SKIP.
END.
OUTPUT CLOSE.
