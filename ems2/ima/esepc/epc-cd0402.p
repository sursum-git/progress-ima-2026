/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR gr-transporte  AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-bt-exporta  AS HANDLE.

DEFINE NEW GLOBAL SHARED VARIABLE wh-tg-frete AS WIDGET-HANDLE.


/* Variable Definitions *****************************************************/
DEF VAR c-objeto          AS CHAR NO-UNDO.

// Main Block
ASSIGN c-objeto = entry(num-entries(p-wgh-object:private-data, "~/"), p-wgh-object:private-data, "~/").

/*
RUN CepOnline/upc/upc-cd0402.p (INPUT p-ind-event,
                                INPUT p-ind-object,
                                INPUT p-wgh-object,
                                INPUT p-wgh-frame,
                                INPUT p-cod-table,
                                INPUT p-row-table).
*/

IF p-row-table <> ? THEN
   ASSIGN gr-transporte = p-row-table.

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "CONTAINER" THEN DO: 

   CREATE BUTTON h-bt-exporta
          ASSIGN FRAME = p-wgh-frame
                 WIDTH        = 5
                 HEIGHT       = 1.22
                 ROW          = 1.30
                 COL          = 58
                 VISIBLE      = YES
                 SENSITIVE    = YES
                 TOOLTIP      = "Exporta Transportadora para Clientes/Cidade/UF"
                 TRIGGERS:
                     ON "CHOOSE":U PERSISTENT RUN esp/escd0402.p.
                END TRIGGERS.

   h-bt-exporta:LOAD-IMAGE("image/toolbar/im-exp.bmp").

END.

IF p-ind-event = "BEFORE-INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05ad268.w" THEN DO:

   CREATE TOGGLE-BOX wh-tg-frete
          ASSIGN FRAME        = p-wgh-frame
                 LABEL        = 'Informa Valor de Frete no Pedido de Venda ' 
                 WIDTH        = 40
                 ROW          = 8
                 COLUMN       = 16.7
                 HEIGHT       = 0.88
                 VISIBLE      = YES
                 SENSITIVE    = NO
                 TOOLTIP      = "Permite Informar o Valor do Frete Negociado para o Pedido de Venda".
END.

IF p-ind-event = "ENABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05ad268.w" THEN DO:
   ASSIGN wh-tg-frete:SENSITIVE = YES.
END.

IF p-ind-event = "DISABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05ad268.w" THEN DO:
   ASSIGN wh-tg-frete:SENSITIVE = NO.
END.

IF p-ind-event = "DISPLAY" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05ad268.w" THEN DO:

   FIND transporte WHERE
        ROWID(transporte) = p-row-table NO-LOCK NO-ERROR.

   IF AVAIL transporte THEN
      ASSIGN wh-tg-frete:SCREEN-VALUE = STRING(transporte.log-2).
END.

IF p-ind-event = "ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05ad268.w" THEN DO:

   FIND transporte WHERE
        ROWID(transporte) = p-row-table NO-LOCK NO-ERROR.

   ASSIGN transporte.log-2 = LOGICAL(wh-tg-frete:SCREEN-VALUE).
END.



RETURN 'OK'.
