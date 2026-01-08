
def input param p-ind-event      as char          no-undo.
def input param p-ind-object     as char          no-undo.
def input param p-wgh-object     as handle        no-undo.
def input param p-wgh-frame      as widget-handle no-undo.
def input param p-cod-table      as char          no-undo.
def input param p-row-table      as recid         no-undo.

DEF VAR h-window AS HANDLE.
DEF VAR h-frame AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

IF c-seg-usuario <> 'seniuz' THEN RETURN 'ok'.


If  p-ind-event = "initialize" THEN DO:
    
    assign h-frame = p-wgh-frame:FIRST-CHILD.
        
    ASSIGN h-window = h-frame:WINDOW.

    ON 'END-ERROR':U OF h-window ANYWHERE PERSISTENT RUN c:\temp\t1.p. 

END.










