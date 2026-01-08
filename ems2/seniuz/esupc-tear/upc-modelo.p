/* Programa: 
** Objetivo: 
**           
** Autor...: 
*/

/* Parameter Definitions ****************************************************/
define input parameter p-ind-event  as character.
define input parameter p-ind-object as character.
define input parameter p-wgh-object as handle.
define input parameter p-wgh-frame  as widget-handle.
define input parameter p-cod-table  as character.
define input parameter p-row-table  as rowid.

/* Global Variable Definitions **********************************************/

/* Variable Definitions *****************************************************/
DEF VAR c-objeto AS CHAR NO-UNDO.

/* Variavies criadas na viewer dinamicamente*********************************/

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

MESSAGE p-ind-event 
        p-ind-object
        STRING(p-cod-table)  
        STRING(P-row-table)
        c-objeto
        p-wgh-object:FILE-NAME
        VIEW-AS ALERT-BOX INFO BUTTONS OK.


