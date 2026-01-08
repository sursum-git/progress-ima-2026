/*********************** Defini‡Æo de Parƒmetros *************************/
DEFINE INPUT PARAMETER p-ind-event                  AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-ind-object                 AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-object                 AS HANDLE         NO-UNDO.
DEFINE INPUT PARAMETER p-wgh-frame                  AS WIDGET-HANDLE  NO-UNDO.
DEFINE INPUT PARAMETER p-cod-table                  AS CHARACTER      NO-UNDO.
DEFINE INPUT PARAMETER p-row-table                  AS ROWID          NO-UNDO.

/******************** Defini‡Æo de Vari veis Globais **********************/
DEFINE NEW GLOBAL SHARED VAR wgh-cod-grp-usuar AS WIDGET-HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wgh-cod-grp-usuar-lbl AS WIDGET-HANDLE NO-UNDO.

/******************** Defini‡Æo de Vari veis Locais **********************/
DEFINE VARIABLE c-objeto                            AS CHARACTER      NO-UNDO.

/************************** In¡cio do Programa ***************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME,"~/"), p-wgh-object:FILE-NAME, "~/") NO-ERROR.

IF p-ind-event = "BEFORE-INITIALIZE" AND 
   p-ind-object = "VIEWER" AND
   c-objeto = "v01di182.w" THEN DO:

   CREATE TEXT wgh-cod-grp-usuar-lbl
        ASSIGN ROW          = 4.85
               COLUMN       = 45.5
               FRAME        = p-wgh-frame
               SENSITIVE    = NO
               VISIBLE      = YES
               FORMAT       = 'x(22)'
               SCREEN-VALUE = 'Grupo Faturamento:'.

   CREATE FILL-IN wgh-cod-grp-usuar
        ASSIGN WIDTH      = 8
               ROW        = 4.65
               COLUMN     = 60
               FORMAT     = "x(30)"
               HEIGHT     = 0.88
               WIDTH      = 25
               VISIBLE    = YES
               FRAME      = p-wgh-frame.
               
END.

IF p-ind-event = "ENABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01di182.w" THEN DO:
   ASSIGN wgh-cod-grp-usuar:SENSITIVE = YES.
END.

IF p-ind-event = "DISABLE" AND
   p-ind-object = "VIEWER"  AND
   c-objeto = "v01di182.w" THEN DO:
   ASSIGN wgh-cod-grp-usuar:SENSITIVE = NO.
END.

IF p-ind-event = "DISPLAY" AND
   p-ind-object = "VIEWER"  AND
   c-objeto = "v01di182.w"  THEN DO:

   FIND FIRST ser-estab NO-LOCK
        WHERE ROWID(ser-estab) = p-row-table NO-ERROR.
   IF AVAIL ser-estab THEN DO:
      FIND FIRST ext-ser-estab WHERE
                 ext-ser-estab.serie       = ser-estab.serie AND 
                 ext-ser-estab.cod-estabel = ser-estab.cod-estabel 
                 NO-LOCK NO-ERROR.
      ASSIGN wgh-cod-grp-usuar:SCREEN-VALUE = ' '.
      IF AVAIL ext-ser-estab THEN
         ASSIGN wgh-cod-grp-usuar:SCREEN-VALUE = ext-ser-estab.cod-grp-usuar.
   END.   
END.

IF p-ind-event = "ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01di182.w" THEN DO:
   FIND FIRST ser-estab WHERE 
        ROWID(ser-estab) = p-row-table  NO-LOCK NO-ERROR.
   IF AVAIL ser-estab THEN DO:
      FIND FIRST ext-ser-estab WHERE 
                 ext-ser-estab.serie = ser-estab.serie AND 
                 ext-ser-estab.cod-estabel = ser-estab.cod-estabel 
                 EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL ext-ser-estab THEN DO.
         CREATE ext-ser-estab.
         ASSIGN ext-ser-estab.serie = ser-estab.serie
                ext-ser-estab.cod-estabel = ser-estab.cod-estabel.
      END.
      ASSIGN ext-ser-estab.cod-grp-usuar = wgh-cod-grp-usuar:SCREEN-VALUE.
   END.
END.

IF p-ind-event = "DELETE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01di182.w" THEN DO:
   FIND FIRST ser-estab WHERE 
        ROWID(ser-estab) = p-row-table NO-LOCK NO-ERROR.
   IF AVAIL ser-estab THEN DO:
      FIND FIRST ext-ser-estab WHERE 
                 ext-ser-estab.serie = ser-estab.serie AND
                 ext-ser-estab.cod-estabel = ser-estab.cod-estabel 
                 EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL ext-ser-estab THEN
         DELETE ext-ser-estab.
   END.  
END.

