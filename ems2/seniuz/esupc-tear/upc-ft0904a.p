/****************************************************************************
** Programa: upc-cd0904a.p 
** Objetivo: Mostrar a DATA DE EMBARQUE de uma nota fiscal.
**           
**           
** Autor   : FµBIO COELHO LANZA - JULHO/2010 
*****************************************************************************/
/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/

/* Variable Definitions *****************************************************/
DEF NEW GLOBAL SHARED VAR wh-dt-embarque AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-dt-embarque AS HANDLE NO-UNDO.

/* Variavies criadas na viewer dinamicamente*********************************/
DEF VAR c-objeto AS CHAR NO-UNDO.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").
/* Main Block ***************************************************************/

/*
MESSAGE "p-wgh-frame  " p-wgh-frame         SKIP
        "p-ind-event  " p-ind-event         SKIP
        "p-ind-object " p-ind-object        SKIP
        "p-cod-table  " STRING(p-cod-table) SKIP 
        "p-row-table  " STRING(P-row-table)    SKIP
        "c-objeto     " c-objeto               SKIP
        "p-whh-object " p-wgh-object:FILE-NAME SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/

IF p-ind-event = "BEFORE-INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v03di135.w" THEN DO:

   CREATE TEXT tx-dt-embarque
          ASSIGN FRAME        = p-wgh-frame
                 FORMAT       = "x(17)"
                 WIDTH        = 14
                 SCREEN-VALUE = "Data Embarque:"
                 ROW          = 11.30
                 COL          = 43.6
                 VISIBLE      = YES.
   CREATE FILL-IN wh-dt-embarque
          ASSIGN FRAME              = p-wgh-frame
                 SIDE-LABEL-HANDLE  = tx-dt-embarque:HANDLE 
                 DATA-TYPE          = "CHARACTER" 
                 FORMAT             = "x(10)" 
                 WIDTH              = 11
                 HEIGHT             = 0.88
                 ROW                = 11.25
                 COL                = 54.94  
                 LABEL              = "Data Embarque:" 
                 VISIBLE            = YES
                 SENSITIVE          = NO.

END.

IF p-ind-event = "DISPLAY" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v03di135.w" THEN DO:
   
   FIND nota-fiscal WHERE
        ROWID(nota-fiscal) = p-row-table NO-LOCK NO-ERROR.

   IF AVAIL nota-fiscal THEN DO.
      IF nota-fiscal.dt-embarque <> ? THEN
         ASSIGN wh-dt-embarque:SCREEN-VALUE = STRING(nota-fiscal.dt-embarque, "99/99/9999").
      ELSE
         ASSIGN wh-dt-embarque:SCREEN-VALUE = "".
   END.
   ELSE 
      ASSIGN wh-dt-embarque:SCREEN-VALUE = "".

END.
