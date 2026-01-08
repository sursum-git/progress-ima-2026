/* Programa: upc-pd4000e1.p
** Objetivo: Trigger de 'Entry' para o campo fi-lote (lote)
**           Sugerir o valor do Lote  baseado no acondicionamento...
** Autor...: Prodb - Toninho  Mar‡o/2004
*/

DEF NEW GLOBAL SHARED VAR wh-acond AS WIDGET-HANDLE NO-UNDO.

IF SELF:SCREEN-VALUE = "" THEN DO.
   FIND corte-comerc WHERE
        corte-comerc.descricao = wh-acond:SCREEN-VALUE
        NO-LOCK NO-ERROR.

   IF AVAIL corte-comerc THEN DO.
      CASE corte-comerc.tp-embalag.
           WHEN 1 THEN ASSIGN SELF:SCREEN-VALUE = 'RP'.
           WHEN 2 THEN ASSIGN SELF:SCREEN-VALUE = 'PP'.
           WHEN 4 THEN ASSIGN SELF:SCREEN-VALUE = 'CA'.
      END CASE.
   END.
END.


