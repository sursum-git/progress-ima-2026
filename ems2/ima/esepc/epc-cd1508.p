/******************************************************************************
** Programa       : UPC-CD1508.p
** Cria‡Æo        : 25/04/2018
** Objetivo       : Ordenar Browser por Coluna
*****************************************************************************/

DEF INPUT PARAM p-ind-event   AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object  AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object  AS   HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame   AS   WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table   AS   CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table   AS   ROWID         NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-browse     AS WIDGET-HANDLE NO-UNDO.

IF  p-ind-event  = "AFTER-OPEN-QUERY" AND
    p-ind-object = "CONTAINER"  THEN DO:   

   RUN pi-busca-widget (INPUT   "brSon1",
                        INPUT   p-wgh-frame,
                        OUTPUT  wh-browse). 

    ASSIGN wh-browse:ALLOW-COLUMN-SEARCHING = YES.

    ON 'START-SEARCH':U OF wh-browse PERSISTEN RUN esepc/epc-cd1508-s.p.
    
END.


PROCEDURE pi-busca-widget:

    DEFINE INPUT  PARAM p-campo      AS CHARACTER     NO-UNDO.
    DEFINE INPUT  PARAM p-frame      AS WIDGET-HANDLE NO-UNDO.
    DEFINE OUTPUT PARAM p-wh-campo   AS WIDGET-HANDLE NO-UNDO.
    
    DEFINE VARIABLE wh-objeto        AS WIDGET-HANDLE.
    DEFINE VARIABLE i-level          AS INTEGER          NO-UNDO.
    DEFINE VARIABLE wh-objeto-level  AS WIDGET-HANDLE    EXTENT 20.

    ASSIGN wh-objeto = p-wgh-frame:PARENT. /* Pesquisa objetos em todas as frames / panels */

    ASSIGN i-level = 1.
    DO WHILE VALID-HANDLE(wh-objeto):
        IF wh-objeto:NAME = p-campo THEN DO:
            ASSIGN p-wh-campo = wh-objeto.
            LEAVE.
        END.
        
        IF wh-objeto:TYPE = "frame" OR wh-objeto:TYPE = "field-group" OR wh-objeto:TYPE = "window" THEN
            ASSIGN wh-objeto-level[i-level] = wh-objeto
                   wh-objeto                = wh-objeto:FIRST-CHILD
                   i-level                  = i-level + 1.
        ELSE
            ASSIGN wh-objeto = wh-objeto:NEXT-SIBLING.

        DO WHILE i-level > 1 AND NOT VALID-HANDLE(wh-objeto):
            ASSIGN i-level = i-level - 1
                   wh-objeto = wh-objeto-level[i-level]:NEXT-SIBLING.
        END.
    END.
    
END PROCEDURE.
