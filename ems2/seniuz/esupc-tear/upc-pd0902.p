/* Parameter Definitions ****************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR h-bt-documentos  AS WIDGET-HANDLE NO-UNDO.

/* Variable Definitions *****************************************************/
DEF VAR c-objeto          AS CHAR NO-UNDO.

/* Variavies criadas na viewer dinamicamente*********************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "CONTAINER" THEN DO: 

   CREATE BUTTON h-bt-documentos
          ASSIGN FRAME = p-wgh-frame
                 WIDTH        = 5
                 HEIGHT       = 1.22
                 ROW          = 1.27
                 COL          = 65.3
                 VISIBLE      = YES
                 SENSITIVE    = YES
                 TOOLTIP      = "Documentos Digitalizados do Cliente"
                 TRIGGERS:
                     ON "CHOOSE":U PERSISTENT RUN esapi/doctos-digitalizados.p. 
                END TRIGGERS.

   h-bt-documentos:LOAD-IMAGE("image/im-docs.bmp").
END.
