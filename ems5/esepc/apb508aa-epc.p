
def input param p-ind-event      as char          no-undo.
def input param p-ind-object     as char          no-undo.
def input param p-wgh-object     as handle        no-undo.
def input param p-wgh-frame      as widget-handle no-undo.
def input param p-cod-table      as char          no-undo.
def input param p-row-table      as recid         no-undo.

DEF NEW GLOBAL SHARED VAR h-campo AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
 
DEF VAR h-frame AS WIDGET-HANDLE NO-UNDO.
/* MESSAGE                                 */
/*                                         */
/*     p-ind-event                         */
/*     p-ind-object                        */
/*     p-wgh-object                        */
/*     p-wgh-frame                         */
/*     p-cod-table                         */
/*     p-row-table                         */
/*                                         */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.  */

If  p-ind-event = "initialize" THEN DO:
    
    DO:
        assign h-frame = p-wgh-frame:FIRST-CHILD.
               h-objeto = h-frame:FIRST-CHILD.
        do  while valid-handle(h-objeto):
            MESSAGE h-objeto:NAME
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            IF  h-objeto:TYPE <> "field-group" THEN DO:
                /*IF  h-objeto:NAME = "cod_pais" THEN DO:
                    ASSIGN h-campo = h-objeto.
                    Leave.
                END.*/
                assign h-objeto = h-objeto:NEXT-SIBLING.
            END.
            ELSE DO:
                Assign h-objeto = h-objeto:first-child.
            END.
        end.
    END.
    MESSAGE STRING(h-campo:NAME) VIEW-AS ALERT-BOX.
END.





