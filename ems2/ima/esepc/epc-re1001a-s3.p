/*******************************************************************************
** Programa: epc-re1001a-s3.p                                                 **  
** Objetivo: Validar caso o usuario digite o estabelecimento 5 na IMA         **
** Autor...:                                                                  **
*******************************************************************************/
DEF NEW GLOBAL SHARED VAR wh-cod-estabel AS WIDGET-HANDLE NO-UNDO.
MESSAGE 'entrei'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

IF VALID-HANDLE(wh-cod-estabel) AND
   wh-cod-estabel:SCREEN-VALUE = '5' THEN DO.
   MESSAGE 'Estabelecimento 5 n∆o percente a IMA'
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   APPLY 'entry' TO wh-cod-estabel.
END.




