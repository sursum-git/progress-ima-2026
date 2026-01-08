/* imp-referencia-ext.p
*/

DEF TEMP-TABLE tt-work
    FIELD cod-refer      AS CHAR
    FIELD cod-obsoleto   AS CHAR
    FIELD colecao        AS CHAR
    FIELD fundo          AS CHAR
    FIELD cor            AS CHAR
    FIELD estilo-desenho AS CHAR
    INDEX ch-work cod-refer.
    
input from "c:/temp/referencia-ext.csv".
SET ^.

repeat:
   create tt-work.
   import delimiter ";" tt-work.
end.
input close.

FOR EACH tt-work.
    IF tt-work.cod-refer <> "" THEN DO:
       FIND referencia-ext WHERE referencia-ext.cod-refer = tt-work.cod-refer.
       IF AVAIL referencia-ext THEN DO:
          ASSIGN referencia-ext.cod-obsoleto   = tt-work.cod-obsoleto  
                 referencia-ext.colecao        = tt-work.colecao       
                 referencia-ext.cod-fundo      = tt-work.fundo         
                 referencia-ext.cor            = tt-work.cor           
                 referencia-ext.estilo-desenho = tt-work.estilo-desenho.

       END.
    END.
END.
