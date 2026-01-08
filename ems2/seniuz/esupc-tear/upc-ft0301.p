/* Programa: upc-ft0301.p
** Objetivo: Manuten‡Æo nos Dados Complementares dos Parƒmtros do Faturamento 
** Autor...: Prodb - Toninho  Fevereiro/2008
*/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/

/* Variavies criadas na viewer dinamicamente*********************************/
DEF NEW GLOBAL SHARED VAR wh-vl-min-nf AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-vl-min-nf AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-perc-frete   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-perc-frete   AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-rect1        AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-rect1        AS HANDLE NO-UNDO.

DEFINE NEW GLOBAL SHARED VAR h-container    AS HANDLE. 


/* Variable Definitions *****************************************************/
DEF VAR h-objeto AS HANDLE.
DEF VAR c-objeto AS CHAR NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "CONTAINER" THEN DO:  /* Cria o Folder Socio */
   ASSIGN h-container = p-wgh-object.
END.

IF p-ind-event = "BEFORE-INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v06di139.w" THEN DO:

   CREATE RECTANGLE wh-rect1
          ASSIGN FRAME         = p-wgh-frame
                 ROW           = 1.3
                 COL           = 36
                 WIDTH         = 26
                 HEIGHT        = 3
                 EDGE-PIXELS   = 2
                 GRAPHIC-EDGE  = YES
                 FILLED        = NO.
   CREATE TEXT tx-rect1
          ASSIGN FRAME        = p-wgh-frame
                 FORMAT       = "x(17)"
                 WIDTH        = 14
                 SCREEN-VALUE = " Parƒmetros TEAR"
                 ROW          = 1.1
                 COL          = 37
                 VISIBLE      = YES.

   CREATE TEXT tx-vl-min-nf
          ASSIGN FRAME        = p-wgh-frame
                 FORMAT       = "x(17)"
                 WIDTH        = 14
                 SCREEN-VALUE = "Valor Min NF:"
                 ROW          = 2.15
                 COL          = 40
                 VISIBLE      = YES.
   CREATE FILL-IN wh-vl-min-nf
          ASSIGN FRAME              = p-wgh-frame
                 SIDE-LABEL-HANDLE  = tx-vl-min-nf:HANDLE 
                 DATA-TYPE          = "DECIMAL" 
                 FORMAT             = ">>>,>>9.99" 
                 WIDTH              = 11
                 HEIGHT             = 0.88
                 ROW                = 2
                 COL                = 50 
                 LABEL              = "Valor Min NF:" 
                 VISIBLE            = YES
                 SENSITIVE          = NO.

   CREATE TEXT tx-perc-frete
          ASSIGN FRAME        = p-wgh-frame
                 FORMAT       = "x(17)"
                 WIDTH        = 14
                 SCREEN-VALUE = "Valor Min DUP:"
                 ROW          = 3.15
                 COL          = 39
                 VISIBLE      = YES.
   CREATE FILL-IN wh-perc-frete
          ASSIGN frame              = p-wgh-frame
                 side-label-handle  = tx-perc-frete:HANDLE  
                 DATA-TYPE          = "DECIMAL" 
                 FORMAT             = ">>>,>>9.99"  
                 WIDTH              = 11
                 HEIGHT             = 0.88
                 ROW                = 3
                 COL                = 50
                 LABEL              = "Valor Min DUP:" 
                 VISIBLE            = YES
                 SENSITIVE          = NO.
END.

IF p-ind-event = "ENABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v06di139.w" THEN DO:
   ASSIGN wh-vl-min-nf:SENSITIVE = YES
          wh-perc-frete:SENSITIVE = YES.
END.

IF p-ind-event = "DISABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v06di139.w" THEN DO:
   ASSIGN wh-vl-min-nf:SENSITIVE = NO
          wh-perc-frete:SENSITIVE = NO.
END.

IF p-ind-event = "ADD" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v06di139.w" THEN DO:
   ASSIGN wh-vl-min-nf:LABEL = "Frete M¡nimo:"
          wh-vl-min-nf:SCREEN-VALUE = '0'
          wh-perc-frete:LABEL = "Perc Frete:"
          wh-perc-frete:SCREEN-VALUE = '0'.
END.

IF p-ind-event = "DISPLAY" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v06di139.w" THEN DO:

   /* Refresh no Pagina 1 do Container */
   RUN select-page IN h-container (INPUT 2). 
   RUN select-page IN h-container (INPUT 1). 

   FIND FIRST param-dis NO-LOCK NO-ERROR.

   IF AVAIL param-dis THEN
      ASSIGN wh-vl-min-nf:SCREEN-VALUE = STRING(param-dis.vl-min-nf)
             wh-perc-frete:SCREEN-VALUE = STRING(param-dis.vl-min-dup).
   ELSE DO.
       ASSIGN wh-vl-min-nf:SCREEN-VALUE = '0'
              wh-perc-frete:SCREEN-VALUE = '0'.
   END.
END.

IF p-ind-event = "ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v06di139.w" THEN DO:

   FIND FIRST param-dis NO-ERROR.

   IF NOT AVAIL param-dis THEN
      CREATE param-dis.

   ASSIGN param-dis.vl-min-nf = DECIMAL(wh-vl-min-nf:SCREEN-VALUE)
          param-dis.vl-min-dup  = DECIMAL(wh-perc-frete:SCREEN-VALUE).   
END.

