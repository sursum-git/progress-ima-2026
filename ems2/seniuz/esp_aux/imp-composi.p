/* imp-composi.p
*/

DEF TEMP-TABLE tt-work
    FIELD cod-composi AS CHAR
    FIELD preimpresso AS LOG
    FIELD descricao   AS CHAR
    FIELD descricao1  AS CHAR
    FIELD descricao2  AS CHAR
    INDEX ch-work cod-composi.
    
input from "c:/temp/composi.csv".
SET ^.

repeat:
   create tt-work.
   import delimiter ";" tt-work.
end.
input close.

FOR EACH tt-work.
    IF tt-work.cod-composi <> "" THEN DO:
       FIND composi WHERE composi.cod-composi = STRING(int(tt-work.cod-composi),"99").
       IF AVAIL composi THEN DO:
          ASSIGN composi.descricao  = tt-work.descricao
                 composi.descricao1 = tt-work.descricao1
                 composi.descricao2 = tt-work.descricao2.
       END.
    END.
END.
