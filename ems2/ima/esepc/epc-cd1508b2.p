DEF NEW GLOBAL SHARED VAR h-preco-min-cif AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-preco-min-fob AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-preco-venda AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-preco-fob AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-dt-inival AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR gr-tb-preco AS ROWID NO-UNDO.

DEF VAR de-ind-finan AS DEC INIT 1.
DEF VAR i-prazo-medio AS INT INIT 90.
DEF VAR i-ct AS INT.
DEF VAR l-ok AS LOGICAL INIT NO.

FIND tb-preco WHERE
     ROWID(tb-preco) = gr-tb-preco NO-LOCK NO-ERROR.

ASSIGN h-dt-inival:SCREEN-VALUE = STRING(tb-preco.dt-inival).

FIND FIRST tab-finan WHERE 
           tab-finan.dt-ini-val <= TODAY AND 
           tab-finan.dt-fim-val >= TODAY NO-LOCK NO-ERROR.
     
DO i-ct = 1 TO EXTENT(tab-finan.tab-dia-fin).
   IF tab-finan.tab-dia-fin[i-ct] >= i-prazo-medio THEN DO.
      ASSIGN l-ok = YES.
      LEAVE. 
   END.
END.
IF l-ok THEN
   ASSIGN de-ind-finan = tab-finan.tab-ind-fin[i-ct].

IF h-preco-venda:SCREEN-VALUE <> h-preco-fob:SCREEN-VALUE THEN  /* Sofreu altera‡Æo */
   ASSIGN SELF:SCREEN-VALUE = STRING(h-preco-venda:INPUT-VALUE / de-ind-finan).

ASSIGN h-preco-fob:SCREEN-VALUE = h-preco-venda:SCREEN-VALUE
       h-preco-min-cif:SCREEN-VALUE = h-preco-venda:SCREEN-VALUE
       h-preco-min-fob:SCREEN-VALUE = h-preco-venda:SCREEN-VALUE.

APPLY 'ENTRY' TO h-preco-fob.
RETURN NO-APPLY.
