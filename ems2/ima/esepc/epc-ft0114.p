DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER          NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER          NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE             NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE      NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER          NO-UNDO.
DEFINE INPUT PARAMETER p-row-table  AS ROWID              NO-UNDO.

DEFINE VARIABLE wh-frame            AS WIDGET-HANDLE      NO-UNDO.

DEFINE VARIABLE wh-objeto           AS WIDGET-HANDLE.
DEFINE VARIABLE i-level             AS INTEGER            NO-UNDO.
DEFINE VARIABLE wh-objeto-level     AS WIDGET-HANDLE      EXTENT 20.

DEFINE NEW GLOBAL SHARED VARIABLE wh-txt-ser AS WIDGET-HANDLE.
DEFINE NEW GLOBAL SHARED VARIABLE wh-cmp-ser AS WIDGET-HANDLE.
DEFINE NEW GLOBAL SHARED VARIABLE wh-serie   AS WIDGET-HANDLE.
DEFINE NEW GLOBAL SHARED VARIABLE wh-estabel AS WIDGET-HANDLE.

/*
DEFINE VARIABLE c-objeto            AS CHARACTER          NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

OUTPUT TO c:\temp\epc-ft0114.txt APPEND.

DISPLAY p-ind-event    FORMAT "x(30)"
        p-ind-object   FORMAT "x(30)"
        STRING(p-cod-table)  
        STRING(p-row-table)
        c-objeto 
        p-wgh-object:FILE-NAME
        WITH DOWN WIDTH 200 NO-LABEL.
        
OUTPUT CLOSE.
*/

IF p-ind-event = "INITIALIZE" AND p-ind-object = "VIEWER" THEN DO:
    ASSIGN wh-objeto = p-wgh-frame:FIRST-CHILD. /* Pesquisa apenas objetos da viewer */
    ASSIGN i-level = 1.

    DO WHILE VALID-HANDLE(wh-objeto):
        IF wh-objeto:NAME = "serie" THEN DO:
            ASSIGN wh-serie = wh-objeto
                   wh-frame = wh-objeto:FRAME.
        END.

        IF wh-objeto:NAME = "cod-estabel" THEN DO:
            ASSIGN wh-estabel = wh-objeto.
        END.

        IF wh-objeto:TYPE = "frame"       OR
           wh-objeto:TYPE = "field-group" OR
           wh-objeto:TYPE = "window"      OR
           wh-objeto:TYPE = "panel-frame" THEN DO:
            ASSIGN wh-objeto-level[i-level] = wh-objeto
                   wh-objeto                = wh-objeto:FIRST-CHILD
                   i-level                  = i-level + 1.
        END.
        ELSE DO:
            ASSIGN wh-objeto = wh-objeto:NEXT-SIBLING.
        END.

        DO WHILE i-level > 1 AND NOT VALID-HANDLE(wh-objeto): 
            ASSIGN i-level = i-level - 1
                   wh-objeto = wh-objeto-level[i-level]:NEXT-SIBLING.
        END.
    END.

    CREATE TEXT wh-txt-ser
        ASSIGN FRAME        = wh-frame
               FORMAT       = "x(14)"
               WIDTH        = 8.3
               ROW          = 11.8
               COLUMN       = 61.1 /*21.1 old*/
               SCREEN-VALUE = "S‚rie Oficial:".

    CREATE FILL-IN wh-cmp-ser
        ASSIGN FRAME        = wh-frame
               FORMAT       = "x(03)"
               WIDTH        = 5
               ROW          = 11.61
               COLUMN       = 70 /*30 old*/
               HEIGHT       = 0.88
               VISIBLE      = YES
               TOOLTIP      = "C¢digo da s‚rie base para emissÆo de notas fiscais"
               SIDE-LABEL-HANDLE = wh-txt-ser.

END.

IF p-ind-event = "ADD" AND p-ind-object = "VIEWER" THEN DO:
    CREATE TEXT wh-txt-ser
    ASSIGN FRAME        = wh-cmp-ser:FRAME
           FORMAT       = "x(14)"
           WIDTH        = 8.3
           ROW          = 11.8
           COLUMN       = 61.1 /*21.1 old*/
           SCREEN-VALUE = "S‚rie Oficial:".
END.

IF p-ind-event = "ENABLE" AND p-ind-object = "VIEWER" THEN DO:
    ASSIGN wh-cmp-ser:SENSITIVE = YES.
END.

IF p-ind-event = "DISABLE" AND p-ind-object = "VIEWER" THEN DO:
    ASSIGN wh-cmp-ser:SENSITIVE = NO.
END.

IF p-ind-event = "DISPLAY" AND p-ind-object = "VIEWER" AND VALID-HANDLE(wh-cmp-ser) THEN DO:
    CREATE TEXT wh-txt-ser
        ASSIGN FRAME        = wh-cmp-ser:FRAME
               FORMAT       = "x(14)"
               WIDTH        = 8.3
               ROW          = 11.8
               COLUMN       = 61.1 /*21.1 old*/
               SCREEN-VALUE = "S‚rie Oficial:".

    FIND FIRST im-ser-estab-ext
        WHERE im-ser-estab-ext.cod-estab = wh-estabel:SCREEN-VALUE AND
              im-ser-estab-ext.serie     = wh-serie:SCREEN-VALUE
        NO-LOCK NO-ERROR.

    IF AVAIL im-ser-estab-ext THEN DO:
        ASSIGN wh-cmp-ser:SCREEN-VALUE = im-ser-estab-ext.serie-ofi.
    END.
    ELSE DO:
        ASSIGN wh-cmp-ser:SCREEN-VALUE = "".
    END.
END.

IF p-ind-event = "ASSIGN" AND p-ind-object = "VIEWER" THEN DO:
    FIND FIRST im-ser-estab-ext
        WHERE im-ser-estab-ext.cod-estab = wh-estabel:SCREEN-VALUE AND
              im-ser-estab-ext.serie     = wh-serie:SCREEN-VALUE
        NO-ERROR.

    IF AVAIL im-ser-estab-ext THEN DO:
        ASSIGN im-ser-estab-ext.serie-ofi = wh-cmp-ser:SCREEN-VALUE.
    END.
    ELSE DO:
        CREATE im-ser-estab-ext.
        ASSIGN im-ser-estab-ext.cod-estab = wh-estabel:SCREEN-VALUE
               im-ser-estab-ext.serie     = wh-serie:SCREEN-VALUE
               im-ser-estab-ext.serie-ofi = wh-cmp-ser:SCREEN-VALUE.
    END.
END.
