DEFINE INPUT PARAMETER p-wgh-frame AS WIDGET-HANDLE.

DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-query AS HANDLE.
DEF VAR h-buffer AS HANDLE.

DEF VAR hb-id      AS WIDGET-HANDLE NO-UNDO.
DEF VAR hb-usuario AS WIDGET-HANDLE NO-UNDO.

ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
ASSIGN h-objeto = h-objeto:FIRST-CHILD.
DO WHILE VALID-HANDLE(h-objeto):
    IF h-objeto:NAME = "b-control-licenca" THEN 
       h-query = h-objeto:QUERY.
    ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
END.

ASSIGN h-buffer = h-query:GET-BUFFER-HANDLE(1)
       hb-id = h-buffer:BUFFER-FIELD(1).
       hb-usuario = h-buffer:BUFFER-FIELD(2).


/*proshut {BANCO} -C disconnect hb_id:BUFFER-VALUE */

