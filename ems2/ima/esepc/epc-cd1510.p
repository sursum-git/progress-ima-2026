DEF INPUT PARAM p-ind-event   AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object  AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object  AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame   AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table   AS CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table   AS ROWID         NO-UNDO.

DEFINE NEW GLOBAL SHARED VAR gr-emitente     AS ROWID NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR tx-ttd    AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-cb-ttd AS WIDGET-HANDLE NO-UNDO.

DEFINE VARIABLE h-objeto      AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE c-objeto      AS CHAR NO-UNDO.
DEFINE VARIABLE cLista11      AS CHARACTER NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

IF p-ind-event  = "BEFORE-INITIALIZE"    AND 
   p-ind-object = "VIEWER"               AND 
   c-objeto     = "v19ad098.w"  THEN DO:

   CREATE TEXT tx-ttd
        ASSIGN FRAME         = p-wgh-frame
               FORMAT        = "x(18)"
               WIDTH         = 20
               SCREEN-VALUE  = 'Enquadramento TTD:'
               ROW           = 5.2
               COL           = 3
               VISIBLE       = YES.

   RUN esapi/getOpcoesListaCB.p (INPUT 11, OUTPUT cLista11).
   CREATE COMBO-BOX wh-cb-ttd
         ASSIGN FRAME              = p-wgh-frame
                ROW                = 5
                COL                = 18
                FORMAT             = "x(50)"
                WIDTH              = 61
                INNER-LINES        = 10 
                VISIBLE            = YES
                SENSITIVE          = NO
                LIST-ITEM-PAIRS    = cLista11
                TOOLTIP            = "Informe o Enquadramento TTD". 
END.

IF p-ind-event  = "ENABLE" AND
   p-ind-object = "VIEWER" AND 
   c-objeto     = "v19ad098.w"  THEN DO:

   FIND emitente WHERE
        ROWID(emitente) = p-row-table NO-LOCK NO-ERROR.
   IF emitente.estado = 'SC' THEN
      ASSIGN wh-cb-ttd:SENSITIVE = YES.
END.

IF p-ind-event  = "DISABLE" AND
   p-ind-object = "VIEWER" AND 
   c-objeto     = "v19ad098.w"  THEN DO:
   ASSIGN wh-cb-ttd:SENSITIVE = NO.
END.

IF p-ind-event  = "DISPLAY" AND
   p-ind-object = "VIEWER" AND
   c-objeto     = "v19ad098.w"  THEN DO:
   FIND emitente WHERE
        ROWID(emitente) = p-row-table NO-LOCK NO-ERROR.

   ASSIGN gr-emitente = ROWID(emitente).

   FIND ext-emitente WHERE 
        ext-emitente.cod-emitente = emitente.cod-emitente SHARE-LOCK NO-ERROR.

   ASSIGN wh-cb-ttd:SCREEN-VALUE = '0'.
   IF AVAIL ext-emitente THEN
      ASSIGN wh-cb-ttd:SCREEN-VALUE = ext-emitente.enquad_ttd.
END.


IF p-ind-event  = 'ASSIGN'     AND
   p-ind-object = "VIEWER" AND 
   c-objeto     = "v19ad098.w"  THEN DO:
   
   FIND emitente WHERE 
        ROWID(emitente) = p-row-table SHARE-LOCK NO-ERROR.
   
   FIND ext-emitente WHERE 
        emitente.cod-emitente = ext-emitente.cod-emitente SHARE-LOCK NO-ERROR.
   
   IF NOT AVAIL ext-emitente THEN DO:
      CREATE ext-emitente.
      ASSIGN ext-emitente.cod-emitente = emitente.cod-emitente.
   END.
   ASSIGN ext-emitente.enquad_ttd = wh-cb-ttd:SCREEN-VALUE.

   ASSIGN gr-emitente = ROWID(emitente).
END.


