/****************************************************************************
** Programa: upc-ft0516.p 
** Objetivo: Criar Toggles EMITE ROMANEIO e marca-o se o usuario pertencer ao 
**           grupo EPO.  
**           Desabilita a Primeira Posicao (IMPRESS«O) do RADIO-BUTTONS 
**           quando o usuario n∆o pertencer ao grupo EPO ou UCR
**           Imprimir o Romaneio da DANFE
**           
** AUTOR   : F†bio Coelho Lanza / Antonio Geraldo de Souza. (JUNHO-2010)
*****************************************************************************/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-romaneio   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-rs-imprime  AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-win    AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-frame  AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-campo  AS WIDGET-HANDLE NO-UNDO.

/* Main Block ***************************************************************/

IF p-wgh-frame:NAME = "fpage0" AND
   NOT VALID-HANDLE(wh-romaneio) THEN DO.

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.

   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME  = "btOK" THEN /* Chamada para Imprimir ROMANEIO */
         ON 'mouse-select-click':U OF h-objeto PERSISTENT RUN esupc/upc-ft0516a2.p (INPUT p-wgh-frame). 
       
      IF h-objeto:NAME = "fPage2" THEN DO:
         ASSIGN h-campo = h-objeto:FIRST-CHILD.
         ASSIGN h-campo = h-campo:FIRST-CHILD.
         DO WHILE VALID-HANDLE(h-campo):

            IF h-campo:NAME = "c-cod-estabel" THEN 
               ON 'ENTRY':U OF h-campo PERSISTENT RUN esupc/upc-ft0516a1.p.  

            IF h-campo:NAME = "rs-imprime" THEN
               ASSIGN h-rs-imprime = h-campo.

            ASSIGN h-campo = h-campo:NEXT-SIBLING.
         END.
      END.

      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.

   CREATE TOGGLE-BOX wh-romaneio
        ASSIGN FRAME              = p-wgh-frame /* deveria ser handle da fPage2 */ 
               ROW                = 13.5
               COL                = 35.0
               VISIBLE            = YES
               LABEL              = "Emitir Romaneio"
               SENSITIVE          = YES.

   FIND usuar_grp_usuar WHERE 
        usuar_grp_usuar.cod_usuar     = c-seg-usuario AND
        usuar_grp_usuar.cod_grp_usuar = "EP0" 
        NO-LOCK NO-ERROR.
   IF AVAIL usuar_grp_usuar THEN 
      ASSIGN wh-romaneio:SCREEN-VALUE = 'YES'. 
END.

