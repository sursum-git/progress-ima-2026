
def input param p-ind-event      as char          no-undo.
def input param p-ind-object     as char          no-undo.
def input param p-wgh-object     as handle        no-undo.
def input param p-wgh-frame      as widget-handle no-undo.
def input param p-cod-table      as char          no-undo.
def input param p-row-table      as recid         no-undo.

DEF NEW GLOBAL SHARED VAR h-campo       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-ok      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-cancel  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-janela     AS WIDGET-HANDLE NO-UNDO.


DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
 
DEF VAR h-frame AS WIDGET-HANDLE NO-UNDO.
/*MESSAGE

    p-ind-event   
    p-ind-object  
    p-wgh-object  
    p-wgh-frame   
    p-cod-table   
    p-row-table   

    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/


If  p-ind-event = "initialize" THEN DO:
    
    DO:
        assign h-frame      = p-wgh-frame:FIRST-CHILD
               h-objeto     = h-frame:FIRST-CHILD
               wh-janela    = h-frame:WINDOW.
        /*ON 'end-error' OF  wh-janela ANYWHERE PERSISTENT RUN apb713ab3-epc.p. */

        do  while valid-handle(h-objeto):
           /* MESSAGE h-objeto:NAME
                VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
            IF h-objeto:NAME = 'bt_can' THEN DO:
/*                 MESSAGE 'chama epc botao cancelar'     */
/*                     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
                ASSIGN wh-bt-cancel = h-objeto:HANDLE.
                ON 'mouse-select-click':U OF h-objeto PERSISTENT RUN esepc\apb713ab1-epc.p.
                
            END.
            IF h-objeto:NAME = 'bt_ok' THEN DO:
/*                 MESSAGE 'chama epc botao ok'           */
/*                     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
                ASSIGN wh-bt-ok = h-objeto:HANDLE.
                ON 'mouse-select-click':U OF h-objeto PERSISTENT RUN esepc\apb713ab2-epc.p.
                
            END.

            assign h-objeto = h-objeto:NEXT-SIBLING.
            
        end.
    END.
   /* MESSAGE STRING(h-campo:NAME) VIEW-AS ALERT-BOX.*/
END.










