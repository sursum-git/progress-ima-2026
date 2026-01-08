
/* Parameter Definitions ****************************************************/
define input parameter p-ind-event  as character.
define input parameter p-ind-object as character.
define input parameter p-wgh-object as handle.
define input parameter p-wgh-frame  as widget-handle.
define input parameter p-cod-table  as character.
define input parameter p-row-table  as rowid.

DEF NEW GLOBAL SHARED VAR wh-max-30  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-rect-30 AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-rect-30 AS HANDLE NO-UNDO.

DEF VAR h-objeto AS HANDLE.

/* Main Block ***************************************************************/
IF p-wgh-frame:NAME = "f-pg-par" AND
   NOT VALID-HANDLE(wh-max-30) THEN DO.

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = "r-classif" THEN
         ASSIGN h-objeto:WIDTH = h-objeto:WIDTH - 15.

      IF h-objeto:NAME = "rect-20" THEN DO.
         ASSIGN h-objeto:WIDTH = h-objeto:WIDTH - 20.

         CREATE RECTANGLE wh-rect-30
             ASSIGN FRAME              = p-wgh-frame
                    ROW                = h-objeto:ROW
                    COL                = h-objeto:COLUMN + 20
                    WIDTH              = h-objeto:WIDTH
                    HEIGHT             = h-objeto:HEIGHT
                    EDGE-PIXELS        = h-objeto:EDGE-PIXELS
                    GRAPHIC-EDGE       = h-objeto:GRAPHIC-EDGE 
                    FILLED             = h-objeto:FILLED
                    FGCOLOR            = h-objeto:FGCOLOR
                    VISIBLE            = YES.

         CREATE TEXT tx-rect-30
                ASSIGN FRAME        = p-wgh-frame
                       FORMAT       = "x(17)"
                       WIDTH        = 10
                       SCREEN-VALUE = "Nome Cliente"
                       ROW          = wh-rect-30:ROW - 0.3
                       COL          = wh-rect-30:COLUMN + 1
                       VISIBLE      = YES.
      END.
      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
         
   CREATE RADIO-SET wh-max-30
       ASSIGN FRAME              = p-wgh-frame
              RADIO-BUTTONS      = "At‚ 30 Caracteres,1,Maior que 30,2,Todos,3"
              HORIZONTAL         = NO 
              WIDTH              = 16
              HEIGHT             = 2
              ROW                = 10
              COL                = 23
              VISIBLE            = YES
              SENSITIVE          = YES.

   ASSIGN wh-max-30:SCREEN-VALUE = '3'.
END.

