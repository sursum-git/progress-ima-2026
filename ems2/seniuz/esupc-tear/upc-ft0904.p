/* Programa: upc-ft0904.p
** Objetivo: Desabilitar os Botäes de navega‡Æo para os representantes
** Autor...: DBNet - Toninho  Agosto/2005
** Alterado: Gilvando Souza Araujo
** Observ..: 
*/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Main Block ***************************************************************/

IF c-seg-usuario BEGINS "rep" THEN 
   RUN esupc/upc-dsb-buttons.p (INPUT p-ind-object, INPUT p-wgh-frame).

