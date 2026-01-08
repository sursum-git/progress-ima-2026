
DEF INPUT PARAM p-ind-event      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object     AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object     AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-recid-table    AS recid         NO-UNDO.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-browse AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-nome-abrev AS WIDGET-HANDLE.
DEF VAR h-calc-col AS WIDGET-HANDLE.
DEF VAR h-col AS HANDLE.
DEF VAR j AS INT.

DEF VAR h-query AS HANDLE.
IF p-ind-event = "display" THEN DO:

        ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
        
        OUTPUT TO c:\temp\objetos.txt.
        PUT "evento|object|wgh|frame|cod_table|recid|nome|tipo" SKIP.
        DO WHILE VALID-HANDLE(h-objeto):
        PUT  p-ind-event       "|"
             p-ind-object      "|"  
             p-wgh-object      "|"
             p-wgh-frame       "|"
             p-cod-table       "|"
             p-recid-table     "|"
             h-objeto:NAME     "|"
             h-objeto:TYPE SKIP.
            IF h-objeto:NAME = "br_dlg_tit_acr_destinac_cobr" THEN DO.
                 h-calc-col = h-objeto:ADD-CALC-COLUMN("char", "x(50)", "", "Cidade", 10).
                 h-calc-col:LABEL-BGCOLOR  = ?.
                 ASSIGN h-browse = h-objeto
                    h-query = h-browse:QUERY.
            END.
             
        
            IF h-objeto:TYPE <> "field-group" THEN
               ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
            ELSE
              ASSIGN h-objeto = h-objeto:FIRST-CHILD.
        
        END.
        OUTPUT CLOSE.
        ON 'row-display':U OF h-browse PERSISTENT RUN esepc/acr739ta01-epc.p (INPUT h-query, INPUT h-calc-col). 
END.

