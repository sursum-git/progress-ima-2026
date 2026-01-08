/****************************************************************************
** Programa: upc-ft0516a1.p 
** Objetivo: Desabilita a Primeira Posicao (IMPRESS«O) do RADIO-BUTTONS 
**           e Marca RADIO-BUTTONS  para (Reempress∆o) quando o usuario 
             pertencer ao grupo EPO ou UCR
**           
** AUTOR   : F†bio Coelho Lanza / Antonio Geraldo de Souza. (JUNHO-2010)
*****************************************************************************/
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO. 
DEF NEW GLOBAL SHARED VAR h-rs-imprime  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-romaneio   AS WIDGET-HANDLE NO-UNDO.

FIND FIRST usuar_grp_usuar WHERE 
           usuar_grp_usuar.cod_usuar     = c-seg-usuario AND
           usuar_grp_usuar.cod_grp_usuar = "NFE"
           NO-LOCK NO-ERROR.

IF NOT AVAIL usuar_grp_usuar THEN DO:
   ASSIGN h-rs-imprime:SCREEN-VALUE = '2'.
   h-rs-imprime:DISABLE(ENTRY(1, h-rs-imprime:RADIO-BUTTONS)).
END.

IF VALID-HANDLE(wh-romaneio) THEN DO.
   ASSIGN wh-romaneio:VISIBLE = YES.
   wh-romaneio:MOVE-TO-TOP().
END.
