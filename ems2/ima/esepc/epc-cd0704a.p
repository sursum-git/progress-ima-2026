/********************************************************************************************
** Programa: epc-cd0704a.p                                                                 **  
** Objetivo: Validar campo de Ramo de Atividade                                                                      **
** Autor...: Anderson Fagner Maio/2009                                                     **
** Observ..: Extená∆o da Epc(epc-cd0704.p) do programa de cadastro de emitente(cd0704)     **
** Esta epc Ç chamada na TRIGGERS de LEAVE do Bot∆o Ramo de Atividade(h-fi-ramo)           **
** e no evento ASSING                                                                      **
********************************************************************************************/

DEF NEW GLOBAL SHARED VAR h-fi-ramo        AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fi-desc-ramo   AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR vg-l-valida-ramo AS LOG NO-UNDO.

FIND ramo-ativ WHERE cod-ramo-ativ = int(h-fi-ramo:SCREEN-VALUE) NO-LOCK NO-ERROR.
IF NOT AVAIL ramo-ativ THEN DO:
    MESSAGE "C¢digo do ramo de atividade n∆o foi encontrado"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    /*RETURN NO-APPLY.*/
    APPLY 'ENTRY' TO h-fi-ramo.
    RETURN "NOK".
END.
ELSE DO:
    h-fi-ramo:SCREEN-VALUE = string(ramo-ativ.cod-ramo-ativ).     
    h-fi-desc-ramo:SCREEN-VALUE = ramo-ativ.descricao.
END.
