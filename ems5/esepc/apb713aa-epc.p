
def input param p-ind-event      as char          no-undo.
def input param p-ind-object     as char          no-undo.
def input param p-wgh-object     as handle        no-undo.
def input param p-wgh-frame      as widget-handle no-undo.
def input param p-cod-table      as char          no-undo.
def input param p-row-table      as recid         no-undo.

DEF NEW GLOBAL SHARED VAR h-campo AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-atu AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
 
DEF VAR h-frame AS WIDGET-HANDLE NO-UNDO.
/* MESSAGE                                */
/*                                        */
/*     p-ind-event                        */
/*     p-ind-object                       */
/*     p-wgh-object                       */
/*     p-wgh-frame                        */
/*     p-cod-table                        */
/*     p-row-table                        */
/*                                        */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */

If  p-ind-event = "initialize" THEN DO:
    
    DO:
        assign h-frame = p-wgh-frame:FIRST-CHILD.
               h-objeto = h-frame:FIRST-CHILD.
        
        do  while valid-handle(h-objeto):
/*             MESSAGE h-objeto:NAME                  */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */
           /* IF h-objeto:NAME = 'bt_exi' THEN DO:
                ON 'mouse-select-click':U OF h-objeto PERSISTENT RUN esepc\apb713aa1-epc.p.
                
            END.*/
            IF h-objeto:NAME = 'bt_atz' THEN DO:
/*                 MESSAGE 'arquivo correto para chamar no botao a epc' */
/*                     VIEW-AS ALERT-BOX INFO BUTTONS OK.               */
                ASSIGN wh-bt-atu = h-objeto:HANDLE.
                ON 'mouse-select-click':U OF h-objeto PERSISTENT RUN esepc\apb713aa2-epc.p.
                
            END.
            assign h-objeto = h-objeto:NEXT-SIBLING.
            
        end.
    END.
   /* MESSAGE STRING(h-campo:NAME) VIEW-AS ALERT-BOX.*/
END.










