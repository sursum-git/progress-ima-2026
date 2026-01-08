/*******************************************************************************
** Programa: epc-re2001a-l.p                                                  **  
** Objetivo: Validar campo de Codigo de Rejeiá∆o                              **
** Autor...: Toninho Maio/2016                                                **
*******************************************************************************/
DEF NEW GLOBAL SHARED VAR wh-cod-rejei AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-desc-rejei AS WIDGET-HANDLE NO-UNDO.

FIND cod-rejeicao WHERE 
     cod-rejeicao.codigo-rejei = INT(wh-cod-rejei:SCREEN-VALUE) NO-LOCK NO-ERROR.
IF NOT AVAIL cod-rejeicao THEN DO:
   MESSAGE "C¢digo de Rejeiá∆o n∆o Cadastrado..."
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
   APPLY 'ENTRY' TO wh-cod-rejei.
   RETURN "NOK".
END.
IF VALID-HANDLE(wh-desc-rejei) THEN
   wh-desc-rejei:SCREEN-VALUE = UPPER(cod-rejeicao.descricao).

