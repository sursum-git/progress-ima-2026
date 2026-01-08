 DEFINE TEMP-TABLE tt
     FIELD cod_classe AS CHAR.

 DEFINE VARIABLE i AS INTEGER     NO-UNDO.
FOR EACH classes /*WHERE
         cod_classe = "11130"*/ .
/*     IF LENGTH (cod_classe) = 4 THEN           */
/*                                               */
/*         ASSIGN cod_classe = "0" + cod_classe. */
    DISP classes.
END.
    

FOR EACH cnaes.
    FIND FIRST classes 
        WHERE classes.cod_classe = cnaes.cod_classe NO-LOCK NO-ERROR.
    IF NOT AVAIL classes THEN DO:
       FIND FIRST tt
           WHERE tt.cod_classe = cnaes.cod_classe
           NO-ERROR.
       IF NOT AVAIL tt THEN DO:
          CREATE tt.
          ASSIGN tt.cod_classe = cnaes.cod_classe.
       END.
    END.   
END.
ASSIGN i = 0.
FOR EACH tt:
    ASSIGN i = i + 1 .
    DISP tt.
END.
DISP i.
