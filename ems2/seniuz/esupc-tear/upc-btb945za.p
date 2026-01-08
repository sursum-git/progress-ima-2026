
/* Parameter Definitions ****************************************************/
define input parameter p-ind-event  as character.
define input parameter p-ind-object as character.
define input parameter p-wgh-object as handle.
define input parameter p-wgh-frame  as widget-handle.
define input parameter p-cod-table  as character.
define input parameter p-row-table  as rowid.


DEF NEW GLOBAL SHARED VAR wh-btn-confirma AS WIDGET-HANDLE NO-UNDO.

/* Main Block ***************************************************************/

IF p-ind-event = "INITIALIZE" THEN DO:
   CREATE BUTTON wh-btn-confirma
         ASSIGN FRAME = p-wgh-frame
                ROW          = 1.17
                COL          = 50
                WIDTH        = 4
                HEIGHT       = 1.2
                VISIBLE      = YES
                SENSITIVE    = YES
                TOOLTIP      = "Desconecta Usu rio do Banco de Dados"
                TRIGGERS:
                    ON "CHOOSE":U PERSISTENT RUN esupc/upc-btb945zac1.p (INPUT p-wgh-frame).
               END TRIGGERS.

    wh-btn-confirma:LOAD-IMAGE("image/imt-chck1.bmp").
END.
