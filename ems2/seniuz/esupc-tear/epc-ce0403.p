
/* Parameter Definitions ****************************************************/
define input parameter p-ind-event  as character.
define input parameter p-ind-object as character.
define input parameter p-wgh-object as handle.
define input parameter p-wgh-frame  as widget-handle.
define input parameter p-cod-table  as character.
define input parameter p-row-table  as rowid.

DEF NEW GLOBAL SHARED VAR wh-narrativa  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-rec-fisico AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-rect-rf    AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

/* Main Block ***************************************************************/

IF p-wgh-frame:NAME = "f-pg-sel" AND
   NOT VALID-HANDLE(wh-rec-fisico) THEN DO.

   CREATE RECTANGLE wh-rect-rf
        ASSIGN FRAME              = p-wgh-frame
               ROW                = 6
               COL                = 24
               WIDTH              = 49
               HEIGHT             = 2.4
               EDGE-PIXELS        = 3
               GRAPHIC-EDGE       = YES
               FILLED             = NO
               VISIBLE            = YES.

   CREATE TOGGLE-BOX wh-rec-fisico
        ASSIGN FRAME              = p-wgh-frame
               ROW                = 6.3
               COL                = 28.8
               VISIBLE            = YES
               LABEL              = "Imprimir Lan‡amentos Recebimento F¡sico"
               SENSITIVE          = YES.

   CREATE TOGGLE-BOX wh-narrativa
         ASSIGN FRAME              = p-wgh-frame
                ROW                = 7.3
                COL                = 28.8
                VISIBLE            = YES
                LABEL              = "Imprimir Narrativa"
                SENSITIVE          = YES.
END.
