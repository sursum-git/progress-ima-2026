/*******************************************************************************
** Programa: epc-re2001a-l.p                                                  **  
** Objetivo: Validar campo de Codigo de Rejei‡Æo                              **
** Autor...: Toninho Maio/2016                                                **
*******************************************************************************/
DEF NEW GLOBAL SHARED VAR wh-cod-rejei AS WIDGET-HANDLE NO-UNDO.

IF SELF:SCREEN-VALUE = "Devolu‡Æo Cliente" THEN DO.
   ASSIGN wh-cod-rejei:SENSITIVE = YES.
END.
ELSE DO.
    ASSIGN wh-cod-rejei:SCREEN-VALUE = ''.
    ASSIGN wh-cod-rejei:SENSITIVE = NO.
END.



