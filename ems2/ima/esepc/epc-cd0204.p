/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Variable Definitions *****************************************************/
DEF NEW GLOBAL SHARED VAR  h-gramatura     AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR  h-lbl-gramatura AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR  h-perc-enc      AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR  h-lbl-perc-enc  AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR  h-perc-var      AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR  h-lbl-perc-var  AS HANDLE NO-UNDO.

DEF VAR c-objeto AS CHAR NO-UNDO.
DEF VAR h-objeto AS HANDLE NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "BEFORE-INITIALIZE" AND
   c-objeto = "v36in172.w" THEN DO. 

   CREATE FILL-IN h-gramatura
       ASSIGN FRAME     = p-wgh-frame
              DATA-TYPE = "DECIMAL"
              FORMAT    = ">>9.99"
              WIDTH     = 8
              VISIBLE   = YES
              SENSITIVE = NO
              HEIGHT    = 0.88
              TOOLTIP   = "Gramatura (g/m):".

   CREATE TEXT h-lbl-gramatura
       ASSIGN FRAME     = p-wgh-frame 
              FORMAT    = "x(16)" 
              WIDTH     = 12           
              VISIBLE   = YES         
              SENSITIVE = NO          
              SCREEN-VALUE = "Gramatura (g/m):".

   CREATE FILL-IN h-perc-enc
       ASSIGN FRAME     = p-wgh-frame
              DATA-TYPE = "DECIMAL"
              FORMAT    = ">>9.99"
              WIDTH     = 8
              ROW       = 4.3
              COLUMN    = 68
              VISIBLE   = YES
              SENSITIVE = NO
              HEIGHT    = 0.88
              TOOLTIP   = "% Encolhimento".

   CREATE TEXT h-lbl-perc-enc
       ASSIGN FRAME     = p-wgh-frame 
              FORMAT    = "x(16)" 
              ROW       = 4.5
              COLUMN    = 55.5
              WIDTH     = 12           
              VISIBLE   = YES         
              SENSITIVE = NO          
              SCREEN-VALUE = "% Encolhimento: ".

   CREATE FILL-IN h-perc-var
       ASSIGN FRAME     = p-wgh-frame
              DATA-TYPE = "DECIMAL"
              FORMAT    = ">>9.99"
              WIDTH     = 8
              ROW       = 5.3
              COLUMN    = 68
              VISIBLE   = YES
              SENSITIVE = NO
              HEIGHT    = 0.88
              TOOLTIP   = "% Var Maxima para o Pre‡o".

   CREATE TEXT h-lbl-perc-var
       ASSIGN FRAME     = p-wgh-frame 
              FORMAT    = "x(16)" 
              ROW       = 5.5
              COLUMN    = 55.5
              WIDTH     = 12           
              VISIBLE   = YES         
              SENSITIVE = NO          
              SCREEN-VALUE = "% Var Max Pre‡o: ".

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto).
      IF h-objeto:NAME = 'lote-economi' THEN DO.
         ASSIGN h-gramatura:ROW = h-objeto:ROW
                h-lbl-gramatura:ROW = h-objeto:ROW + 0.1
                h-gramatura:COL = h-objeto:COL + 40
                h-lbl-gramatura:COL = h-objeto:COL + 28.

         LEAVE.
      END.
      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
END.

IF p-ind-event = "INITIALIZE" AND 
   p-ind-object = "CONTAINER" THEN DO.
   RUN select-page IN p-wgh-object (INPUT 2).
   RUN select-page IN p-wgh-object (INPUT 1).
END.

IF p-ind-event = 'DISPLAY' AND 
   c-objeto = "v36in172.w" THEN DO.

   FIND item WHERE
        ROWID(item) = p-row-table NO-ERROR.

   FIND item-ext WHERE
        item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

   ASSIGN h-gramatura:SCREEN-VALUE = '0'
          h-perc-enc:SCREEN-VALUE = '0'
          h-perc-var:SCREEN-VALUE = '0'.
   IF AVAIL item-ext THEN 
      ASSIGN h-gramatura:SCREEN-VALUE = STRING(item-ext.gramatura)
             h-perc-enc:SCREEN-VALUE = STRING(item-ext.perc-enc)
             h-perc-var:SCREEN-VALUE = STRING(item-ext.var-max-preco).
END.

IF p-ind-event = 'ENABLE'  AND
   c-objeto = "v36in172.w" THEN 
   ASSIGN h-gramatura:SENSITIVE = YES
          h-perc-enc:SENSITIVE = YES
          h-perc-var:SENSITIVE = YES.

IF p-ind-event = 'DISABLE' AND
   c-objeto = "v36in172.w" THEN 
   ASSIGN h-gramatura:SENSITIVE = NO
          h-perc-enc:SENSITIVE = NO
          h-perc-var:SENSITIVE = NO.

IF p-ind-event = 'ASSIGN' AND
   c-objeto = "v36in172.w" THEN DO.
   FIND item WHERE
        ROWID(item) = p-row-table NO-ERROR.
   FIND item-ext WHERE
        item-ex.it-codigo = item.it-codigo NO-ERROR.

   IF NOT AVAIL item-ext THEN DO.
      CREATE item-ext.
      ASSIGN item-ext.it-codigo = item.it-codigo.
   END.
   ASSIGN item-ext.gramatura = h-gramatura:INPUT-VALUE
          item-ext.perc-enc = h-perc-enc:INPUT-VALUE
          item-ext.var-max-preco = h-perc-var:INPUT-VALUE.
END.

