/* Programa: imp-preco-fiscal.p
** Objetivo: Importar pre‡os fiscais lan‡ados em uma planilha,
**           para a correta valoriza‡Æo das sobras.
*/

DEF VAR da-dt-pr-fisc LIKE ITEM.dt-pr-fisc.
DEF VAR i-cont AS INT.
ASSIGN da-dt-pr-fisc = DATE(04,30,YEAR(TODAY)).
UPDATE da-dt-pr-fisc.

DEF TEMP-TABLE tt-preco-fiscal
    FIELD it-codigo    LIKE item.it-codigo
    FIELD preco-fiscal LIKE item.preco-fiscal.
    
INPUT FROM "c:/lixo/preco_fiscal2.csv".
/*SET ^. quando tiver cabecalho */

REPEAT:
   CREATE tt-preco-fiscal.
   IMPORT DELIMITER ";" tt-preco-fiscal.
END.
INPUT CLOSE.

FOR EACH tt-preco-fiscal:
    IF tt-preco-fiscal.it-codigo = "" THEN NEXT.
    ASSIGN i-cont = i-cont + 1.
    PUT SCREEN ROW 10 COLUMN 10 "Item: " + tt-preco-fiscal.it-codigo.
    PUT SCREEN ROW 11 COLUMN 11 "Cont: " + string(i-cont,">>>9").
    FIND ITEM WHERE ITEM.it-codigo = tt-preco-fiscal.it-codigo
              NO-ERROR.
    IF AVAIL ITEM THEN DO:
       ASSIGN ITEM.preco-fiscal = tt-preco-fiscal.preco-fiscal
              item.dt-pr-fisc   = da-dt-pr-fisc.
       FOR EACH item-uni-estab WHERE item-uni-estab.it-codigo = ITEM.it-codigo:
           ASSIGN item-uni-estab.preco-fiscal = tt-preco-fiscal.preco-fiscal
                  item-uni-estab.data-pr-fisc = da-dt-pr-fisc.
       END.
    END.
END.
