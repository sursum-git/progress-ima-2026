DEF INPUT PARAM p-ind-event        AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object       AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object       AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame        AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table        AS CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table        AS ROWID         NO-UNDO.

DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
DEF VAR c-objeto AS CHAR NO-UNDO.
DEF VAR adm-current-page AS INTEGER NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-container AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-tipo-layout AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR l-contr-usr AS LOG.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

DEF VAR wh-obj AS WIDGET-HANDLE EXTENT 20 NO-UNDO.
DEF VAR i-level AS INTEGER INITIAL 1.
                          
DEF NEW GLOBAL SHARED VAR h-loc-entrega AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

assign c-objeto = entry(num-entries(p-wgh-object:private-data, "~/"), p-wgh-object:private-data, "~/").
/*
OUTPUT TO c:\ponto-epc.txt APPEND.

DISPLAY p-ind-event FORMAT "x(30)"
        p-ind-object FORMAT "x(30)"
        c-objeto FORMAT "x(30)"
        p-cod-table FORMAT "x(30)"
        STRING(p-row-table) FORMAT "x(20)"
        WITH DOWN NO-LABELS WIDTH 200.
OUTPUT CLOSE.

  */

IF p-ind-event  = "AFTER-ENABLE" AND 
   p-ind-object = "VIEWER"       AND  
   c-objeto     = "v09In385.w"   THEN DO:
    
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = 'loc-entrega' THEN DO:
         ASSIGN h-loc-entrega = h-objeto.
      END.
      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
   /* Verifica se o usuario tem permiss∆o de Alterar Requisiá∆o 
        (campo "Altera Requisiá∆o" do programa cd1700)*/ 
   FIND usuar-mater WHERE usuar-mater.cod-usuario = c-seg-usuario 
                      AND substring(usuar-mater.char-1,10,01) = "y" 
                    NO-LOCK NO-ERROR.
   IF NOT AVAIL usuar-mater THEN DO:
      ASSIGN h-loc-entrega:SENSITIVE = NO.
   END.
END.
