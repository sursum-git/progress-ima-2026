/* Programa: imp-access.p
** Objetivo: Importar o log de acesso … internet e exportar para excel. 
*/

DEF TEMP-TABLE tt-ccpg
    FIELD campo1 AS CHAR FORMAT "x(240)".

DEF TEMP-TABLE tt-cc13
    FIELD campo1 AS CHAR FORMAT "x(240)".

DEF VAR i-vlr-lanc1 AS DEC.

input from "c:/Junta/ccpg.lst".
repeat:
   create tt-ccpg.
   import delimiter "#$%" tt-ccpg.
end.
input close.

input from "c:/Junta/cc13.lst".
repeat:
   create tt-cc13.
   import delimiter "#$%" tt-cc13.
end.
input close.

FOR EACH tt-ccpg:
    IF tt-ccpg.campo1 = "" THEN NEXT.
    DISP substr(tt-ccpg.campo1,8,1)
         substr(tt-ccpg.campo1,81,7).
END.
   
FOR EACH tt-cc13:
    IF tt-cc13.campo1 = "" THEN NEXT.
    DISP substr(tt-cc13.campo1,8,1)
         substr(tt-cc13.campo1,81,7).
END.
