/* Programa: upc-cd0701.p
** Objetivo: Dar manuten‡Æo nos Dados complementares dos Grupos de Clientes, 
**           (gr-cliente-ext) referentes …s customiza‡äes da Tear Tˆxtil Ind.Com.Ltda.
** Autor...: Gilvando - Junho/2006
*/

/* Parameter Definitions ****************************************************/
define input parameter p-ind-event  as character.
define input parameter p-ind-object as character.
define input parameter p-wgh-object as handle.
define input parameter p-wgh-frame  as widget-handle.
define input parameter p-cod-table  as character.
define input parameter p-row-table  as rowid.

/* Global Variable Definitions **********************************************/

/* Variavies criadas na viewer dinamicamente*********************************/
DEF NEW GLOBAL SHARED VAR wh-qld-otima       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-qld-boa         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-qld-regular     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-qld-direcionada AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-rect1           AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-qld-prefer      AS HANDLE NO-UNDO.

/* Variable Definitions *****************************************************/
DEF VAR c-objeto AS CHAR NO-UNDO.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "BEFORE-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.

   DO WHILE VALID-HANDLE(h-objeto):
      /*
      MESSAGE "Name: " h-objeto:NAME SKIP
              "Frame: " h-objeto:FRAME
              VIEW-AS ALERT-BOX.
      */
      IF h-objeto:NAME = "cod-transp" THEN
         ASSIGN h-objeto:FRAME:HEIGHT = h-objeto:FRAME:HEIGHT + 4
                h-objeto:FRAME:WIDTH = h-objeto:FRAME:WIDTH + 27.
      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
END.

/*
IF p-ind-event = "BEFORE-INITIALIZE" THEN
   MESSAGE "p-ind-object: " p-ind-object SKIP
           "c-objeto: " c-objeto
           VIEW-AS ALERT-BOX.
*/

IF p-ind-event = "BEFORE-INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v02ad129.w" THEN DO:

   CREATE RECTANGLE wh-rect1
        ASSIGN FRAME         = p-wgh-frame
               ROW           = 7.8
               COL           = 28
               EDGE-PIXELS   = 2
               GRAPHIC-EDGE  = YES
               HEIGHT-PIXELS = 60
               WIDTH-PIXELS  = 240
               FILLED        = NO.
   
   CREATE TEXT tx-qld-prefer
        ASSIGN FRAME         = p-wgh-frame
               FORMAT        = "x(30)"
               WIDTH         = 15.5
               SCREEN-VALUE  = "Qualidade Preferencial"
               ROW           = 7.5
               COL           = 37
               VISIBLE       = YES.

   CREATE TOGGLE-BOX wh-qld-otima
        ASSIGN FRAME     = p-wgh-frame
               ROW       = 8.2
               COL       = 30.7
               VISIBLE   = YES
               LABEL     = "A - àtima"
               SENSITIVE = NO
               TOOLTIP   = "Qualidade ¢tima.".

   CREATE TOGGLE-BOX wh-qld-boa
        ASSIGN FRAME     = p-wgh-frame
               ROW       = 9.2
               COL       = 30.7
               VISIBLE   = YES
               LABEL     = "B - Boa"
               SENSITIVE = NO
               TOOLTIP   = "Qualidade boa.".
   
   CREATE TOGGLE-BOX wh-qld-regular
        ASSIGN FRAME     = p-wgh-frame
               ROW       = 8.2
               COL       = 45.7
               VISIBLE   = YES
               LABEL     = "C - Regular"
               SENSITIVE = NO
               TOOLTIP   = "Qualidade regular.".

   CREATE TOGGLE-BOX wh-qld-direcionada
        ASSIGN FRAME     = p-wgh-frame
               ROW       = 9.2
               COL       = 45.7
               VISIBLE   = YES
               LABEL     = "D - Direcionada"
               SENSITIVE = NO
               TOOLTIP   = "Qualidade direcionada.".
END.

IF p-ind-event = "ENABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v02ad129.w" THEN DO: 
   ASSIGN wh-qld-otima:SENSITIVE       = YES
          wh-qld-boa:SENSITIVE         = YES
          wh-qld-regular:SENSITIVE     = YES
          wh-qld-direcionada:SENSITIVE = YES. 
END.

IF p-ind-event = "DISABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v02ad129.w" THEN DO: 
   ASSIGN wh-qld-otima:SENSITIVE       = NO 
          wh-qld-boa:SENSITIVE         = NO 
          wh-qld-regular:SENSITIVE     = NO
          wh-qld-direcionada:SENSITIVE = NO. 
END.

IF p-ind-event = "ADD" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v02ad129.w" THEN DO: 
   ASSIGN tx-qld-prefer:SCREEN-VALUE      = "Qualidade Preferencial"
          wh-qld-otima:SCREEN-VALUE       = 'no'
          wh-qld-boa:SCREEN-VALUE         = 'no'
          wh-qld-regular:SCREEN-VALUE     = 'no'
          wh-qld-direcionada:SCREEN-VALUE = 'no'.  
END.

IF p-ind-event = "DISPLAY" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v02ad129.w" THEN DO:

   FIND gr-cli WHERE
        ROWID(gr-cli) = p-row-table NO-LOCK NO-ERROR.

   FIND gr-cli-ext WHERE
        gr-cli-ext.cod-gr-cli = gr-cli.cod-gr-cli NO-LOCK NO-ERROR.

   IF AVAIL gr-cli-ext THEN
      ASSIGN wh-qld-otima:SCREEN-VALUE       = STRING(gr-cli-ext.qld-otima)
             wh-qld-boa:SCREEN-VALUE         = STRING(gr-cli-ext.qld-boa)
             wh-qld-regular:SCREEN-VALUE     = STRING(gr-cli-ext.qld-reg)
             wh-qld-direcionada:SCREEN-VALUE = STRING(gr-cli-ext.qld-dir).
   ELSE DO:
      ASSIGN wh-qld-otima:SCREEN-VALUE       = 'no' 
             wh-qld-boa:SCREEN-VALUE         = 'no' 
             wh-qld-regular:SCREEN-VALUE     = 'no' 
             wh-qld-direcionada:SCREEN-VALUE = 'no'.
   END.
END.

IF p-ind-event = "ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v02ad129.w" THEN DO:

   FIND gr-cli WHERE
        ROWID(gr-cli) = p-row-table NO-LOCK NO-ERROR.

   FIND gr-cli-ext WHERE
        gr-cli-ext.cod-gr-cli = gr-cli.cod-gr-cli NO-ERROR.

   IF NOT AVAIL gr-cli-ext THEN DO:
      CREATE gr-cli-ext.
      ASSIGN gr-cli-ext.cod-gr-cli = gr-cli.cod-gr-cli.
   END.
   ASSIGN gr-cli-ext.qld-otima = LOGICAL(wh-qld-otima:SCREEN-VALUE)
          gr-cli-ext.qld-boa   = LOGICAL(wh-qld-boa:SCREEN-VALUE)
          gr-cli-ext.qld-reg   = LOGICAL(wh-qld-regular:SCREEN-VALUE)
          gr-cli-ext.qld-dir   = LOGICAL(wh-qld-direcionada:SCREEN-VALUE).   
END.
