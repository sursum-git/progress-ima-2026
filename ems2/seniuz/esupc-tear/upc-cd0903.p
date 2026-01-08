/* Programa: upc-cd0903.p
** Objetivo: Dar manutená∆o nos Dados complementares dos Itens, 
**           (item-ext) referentes Ös customizaá‰es da Tear Tàxtil Ind.Com.Ltda.
** Autor...: Prodb - Toninho  Setembro/2009
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
DEF NEW GLOBAL SHARED VAR h-container AS HANDLE. 

DEF NEW GLOBAL SHARED VAR wh-rect-ncm AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-ncm-n AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-ncm-c AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-ncm-m AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-est AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-est AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bco AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-bco AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-tto AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-tto AS HANDLE NO-UNDO.

/* Variable Definitions *****************************************************/
DEF VAR h-objeto AS HANDLE.
DEF VAR c-objeto AS CHAR NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

IF p-ind-object = "CONTAINER" AND
   NOT VALID-HANDLE(h-container) THEN
   ASSIGN h-container = p-wgh-object.

/* Main Block ***************************************************************/

IF p-ind-event = "BEFORE-INITIALIZE" AND
   c-objeto = "v46in172.w" THEN DO:

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "RECT-26" THEN 
            ASSIGN h-objeto:WIDTH = h-objeto:WIDTH - 8.

         IF h-objeto:NAME = "rs-apuracao" THEN 
            ASSIGN h-objeto:WIDTH = h-objeto:WIDTH - 5
                   h-objeto:COL = h-objeto:COL - 3.

         IF h-objeto:NAME = "c-desc-classif-fisc" THEN 
            ASSIGN h-objeto:WIDTH = h-objeto:WIDTH - 10.
                   
         IF h-objeto:NAME = "c-desc-cod-servico" THEN 
            ASSIGN h-objeto:WIDTH = h-objeto:WIDTH - 10.
         
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.

   CREATE RECTANGLE wh-rect-ncm
        ASSIGN FRAME              = p-wgh-frame
               ROW                = 1.25
               COL                = 67
               WIDTH              = 16
               HEIGHT             = 3.80
               EDGE-PIXELS        = 2
               GRAPHIC-EDGE       = YES
               FILLED             = NO
               VISIBLE            = YES.

   CREATE TEXT tx-ncm-n
          ASSIGN FRAME              = p-wgh-frame
                 FORMAT             = "x(5)"
                 WIDTH              = 5
                 SCREEN-VALUE       = "N C M"
                 ROW                = 1
                 COL                = 68
                 VISIBLE            = YES.

   CREATE TEXT tx-est
          ASSIGN FRAME              = p-wgh-frame
                 FORMAT             = "x(17)"
                 WIDTH              = 4
                 SCREEN-VALUE       = "EST:"
                 ROW                = 2
                 COL                = 68.6
                 VISIBLE            = YES.
   CREATE FILL-IN wh-est           
          ASSIGN FRAME              = p-wgh-frame
                 SIDE-LABEL-HANDLE  = tx-est:HANDLE 
                 DATA-TYPE          = "CHARACTER" 
                 FORMAT             = "9999.99.99" 
                 WIDTH              = 9
                 HEIGHT             = 0.88
                 ROW                = 1.85
                 COL                = 72.3  
                 LABEL              = "EST:" 
                 VISIBLE            = YES
                 SENSITIVE          = no.

   CREATE TEXT tx-bco
          ASSIGN FRAME              = p-wgh-frame
                 FORMAT             = "x(17)"
                 WIDTH              = 4
                 SCREEN-VALUE       = "BCO:"
                 ROW                = 3
                 COL                = 68.5
                 VISIBLE            = YES.
   CREATE FILL-IN wh-bco
          ASSIGN FRAME              = p-wgh-frame
                 SIDE-LABEL-HANDLE  = tx-bco:HANDLE 
                 DATA-TYPE          = "CHARACTER" 
                 FORMAT             = "9999.99.99" 
                 WIDTH              = 9
                 HEIGHT             = 0.88
                 ROW                = 2.85
                 COL                = 72.3  
                 LABEL              = "BCO:" 
                 VISIBLE            = YES
                 SENSITIVE          = NO.

   CREATE TEXT tx-tto
          ASSIGN FRAME              = p-wgh-frame
                 FORMAT             = "x(17)"
                 WIDTH              = 4
                 SCREEN-VALUE       = "TTO:"
                 ROW                = 4
                 COL                = 68.5
                 VISIBLE            = YES.
   CREATE FILL-IN wh-tto
          ASSIGN FRAME              = p-wgh-frame
                 SIDE-LABEL-HANDLE  = tx-tto:HANDLE 
                 DATA-TYPE          = "CHARACTER" 
                 FORMAT             = "9999.99.99" 
                 WIDTH              = 9
                 HEIGHT             = 0.88
                 ROW                = 3.85
                 COL                = 72.3  
                 LABEL              = "TTO:" 
                 VISIBLE            = YES
                 SENSITIVE          = NO.
END.

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v46in172.w" THEN 
   RUN select-page IN h-container (INPUT 1).

IF p-ind-event = "ENABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v46in172.w" THEN DO:
   ASSIGN wh-est:SENSITIVE = YES
          wh-bco:SENSITIVE = YES
          wh-tto:SENSITIVE = YES.
END.

IF p-ind-event = "DISABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v46in172.w" THEN DO:
   ASSIGN wh-est:SENSITIVE = NO
          wh-bco:SENSITIVE = NO
          wh-tto:SENSITIVE = NO.
END.

IF p-ind-event = "ADD" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v46in172.w" THEN DO:
   ASSIGN wh-est:LABEL = "EST:"
          wh-est:SCREEN-VALUE = FILL("0",8)
          wh-bco:LABEL = "BCO:"
          wh-bco:SCREEN-VALUE = FILL("0",8)
          wh-tto:LABEL = "TTO:"
          wh-tto:SCREEN-VALUE = FILL("0",8).
END.

IF p-ind-event = "DISPLAY" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v46in172.w" THEN DO:

   FIND item WHERE
        ROWID(item) = p-row-table NO-LOCK NO-ERROR.

   FIND item-ext WHERE
        item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

   ASSIGN wh-est:SCREEN-VALUE = IF AVAIL item-ext AND item-ext.cod-ncm-est <> ""
                                THEN item-ext.cod-ncm-est ELSE FILL("0",8)
          wh-bco:SCREEN-VALUE = IF AVAIL item-ext AND item-ext.cod-ncm-est <> ""
                                THEN item-ext.cod-ncm-bco ELSE FILL("0",8)
          wh-tto:SCREEN-VALUE = IF AVAIL item-ext AND item-ext.cod-ncm-est <> ""
                                THEN item-ext.cod-ncm-tto ELSE FILL("0",8).
END.

IF p-ind-event = "VALIDATE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v46in172.w" THEN DO:

   IF INTEGER(wh-est:SCREEN-VALUE) = 0 OR
      INTEGER(wh-bco:SCREEN-VALUE) = 0 OR
      INTEGER(wh-tto:SCREEN-VALUE) = 0 THEN DO.
      MESSAGE "Informaá‰es de NCM s∆o obrigat¢rias"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO wh-est.
      RETURN 'NOK'.
   END.
END.

IF p-ind-event = "ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v46in172.w" THEN DO:

   FIND item WHERE
        ROWID(item) = p-row-table NO-LOCK NO-ERROR.

   FIND item-ext WHERE
        item-ext.it-codigo = item.it-codigo NO-ERROR.

   IF NOT AVAIL item-ext THEN DO.
      CREATE item-ext.
      ASSIGN item-ext.it-codigo = item.it-codigo.
   END.
   ASSIGN item-ext.cod-ncm-est = wh-est:INPUT-VALUE
          item-ext.cod-ncm-bco = wh-bco:INPUT-VALUE
          item-ext.cod-ncm-tto = wh-tto:INPUT-VALUE.
END.

