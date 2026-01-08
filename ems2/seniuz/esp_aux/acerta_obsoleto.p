DEF VAR c-linha AS CHAR FORMAT "x(16)".

input from "c:/temp/movto.exp" NO-ECHO.
REPEAT.
   SET c-linha.
   FIND ITEM WHERE ITEM.it-codigo = c-linha NO-ERROR.
   DISP c-linha. 
   /*
   IF AVAIL ITEM THEN
      ASSIGN ITEM.cod-obsoleto = 1.
   
   FOR EACH cot-item WHERE cot-item.item-cotacao = c-linha.
       ASSIGN cot-item.cod-obsoleto = 1.
   END.
   
   FOR EACH dp-item WHERE dp-item.it-codigo = c-linha.
       ASSIGN dp-item.cod-obsoleto = 1.
   END.

   FOR EACH item-invest WHERE item-invest.it-codigo = c-linha.
       ASSIGN ITEM-invest.cod-obsoleto = 1.
   END.

   FOR EACH item-uni-estab WHERE item-uni-estab.it-codigo = c-linha.
       ASSIGN item-uni-estab.cod-obsoleto = 1.
   END.

   FOR EACH item-mat-estab WHERE item-mat-estab.it-codigo = c-linha.
       ASSIGN item-mat-estab.cod-obsoleto = 1.
   END.

   FOR EACH mnt-item WHERE mnt-item.it-codigo = c-linha.
       ASSIGN mnt-item.cod-obsoleto = 1.
   END.
   */
END.
INPUT CLOSE.

