DEF INPUT PARAM p-ind-event      AS CHAR               NO-UNDO.
DEF INPUT PARAM p-ind-object     AS CHAR               NO-UNDO.
DEF INPUT PARAM p-wgh-object     AS HANDLE             NO-UNDO.
DEF INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE      NO-UNDO.
DEF INPUT PARAM p-cod-table      AS CHAR               NO-UNDO.
DEF INPUT PARAM p-recid-table    AS RECID              NO-UNDO.
DEFINE VARIABLE h-objeto         AS WIDGET-HANDLE      NO-UNDO.
def var c-objeto as char no-undo. 

assign c-objeto = entry(num-entries(p-wgh-object:private-data, "~/"), 
p-wgh-object:private-data, "~/"). 
 ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
    ASSIGN h-objeto = h-objeto:FIRST-CHILD.
     

    DO WHILE VALID-HANDLE(h-objeto):
        MESSAGE 
              p-ind-event    
              p-ind-object   
              p-wgh-object   
              p-wgh-frame    
              p-cod-table    
              p-recid-table
              c-objeto
            VIEW-AS ALERT-BOX INFO BUTTONS OK.  
        ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
    END.
/*
IF p-ind-event = "DISPLAY" THEN DO:
    ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
    ASSIGN h-objeto = h-objeto:FIRST-CHILD.
     

    DO WHILE VALID-HANDLE(h-objeto):
        MESSAGE 
              p-ind-event    
              p-ind-object   
              p-wgh-object   
              p-wgh-frame    
              p-cod-table    
              p-recid-table
              c-objeto
            VIEW-AS ALERT-BOX INFO BUTTONS OK.  
        ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
    END.
END.

    



  */
