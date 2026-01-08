/******************************************************************************
*   Programa .....: epc-cd0704z.p                                             *
*   Data .........: 05/06/2017                                                *
*   Cliente ......: Ima                                                       *
*   Programador ..: Toninho                                                   *
*   Objetivo .....: Concatenar o Retorno do Zoom ao Screen-Value              *
*                                                                             *
******************************************************************************/
DEF NEW GLOBAL SHARED VAR wh-coligada AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-coligada AS CHAR.

IF VALID-HANDLE(wh-coligada) THEN DO.
   IF c-coligada <> '' THEN
      ASSIGN wh-coligada:SCREEN-VALUE = c-coligada + "," + wh-coligada:SCREEN-VALUE.

   ASSIGN c-coligada = ''.
END.
