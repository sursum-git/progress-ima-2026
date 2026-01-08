/* Programa: imp-item-desc.p
** Objetivo: Importar descri‡Æo de itens lan‡adas em uma planilha,
**           para a corre‡Æo do cadastro.
*/

DEF VAR i-cont AS INT.

DEF TEMP-TABLE tt-item-desc
    FIELD it-codigo  LIKE item.it-codigo
    FIELD desc-item  LIKE item.desc-item
    FIELD un         LIKE item.un
    FIELD tipo-con   LIKE item.tipo-con-est.
    
INPUT FROM "c:/lixo/item.csv".
SET ^. /*quando tiver cabecalho*/

REPEAT:
   CREATE tt-item-desc.
   IMPORT DELIMITER ";" tt-item-desc.
END.
INPUT CLOSE.

FOR EACH tt-item-desc:
    IF tt-item-desc.it-codigo = "" THEN NEXT.
    
    ASSIGN i-cont = i-cont + 1.
    PUT SCREEN ROW 10 COLUMN 10 "Item: " + tt-item-desc.it-codigo.
    PUT SCREEN ROW 11 COLUMN 11 "Cont: " + string(i-cont,">>>9").

    FIND ITEM WHERE ITEM.it-codigo = tt-item-desc.it-codigo
              NO-ERROR.
    
    IF AVAIL ITEM THEN DO:
       DISP tt-item-desc.desc-item
            SUBSTR(tt-item-desc.desc-item,1,18)  
            SUBSTR(tt-item-desc.desc-item,19,18).
       /*
       ASSIGN ITEM.desc-item   = tt-item-desc.desc-item
              ITEM.descricao-1 = SUBSTR(tt-item-desc.desc-item,1,18)
              ITEM.descricao-2 = SUBSTR(tt-item-desc.desc-item,19,18).
       */
    END.
END.
