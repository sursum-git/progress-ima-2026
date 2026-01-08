/* Programa: exp-imp-item.p
**           Exporta e importa itens.
*/

DEF VAR i-cont AS INT.
DEF VAR c-arquivo AS CHAR.

ASSIGN c-arquivo = "c:\temp\item.d".

/*----- Exporta‡Æo -----*/
OUTPUT TO VALUE(c-arquivo).

FOR EACH ITEM WHERE item.it-codigo >= "0000001" AND
                    ITEM.it-codigo <= "0000070" NO-LOCK:
    PUT SCREEN ROW 20 COLUMN 10 "Item: " + STRING(item.it-codigo) + " - " + STRING(i-cont).
    EXPORT item.
    ASSIGN i-cont = i-cont + 1.
END.

OUTPUT CLOSE.

/*

/*----- Importa‡Æo -----*/
INPUT FROM VALUE(c-arquivo).

REPEAT:
   CREATE ITEM.
   IMPORT ITEM.
END.
INPUT CLOSE.

*/
