/* Programa: upc-cd1508b.p
** Objetivo: Solicitar Pre‡os para Rolo Perfeito e Defeituoso 
** Autor...: SeniuZ - Toninho  Setembro/2012
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
DEF NEW GLOBAL SHARED VAR wh-preco-rp AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-preco-rp AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-preco-rd AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-preco-rd AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-all-ref  AS HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-cod-refer AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-it-codigo AS WIDGET-HANDLE NO-UNDO.

/* Variable Definitions *****************************************************/
DEF VAR c-objeto AS CHAR NO-UNDO.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-preco-cif AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-preco-fob AS WIDGET-HANDLE NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "BEFORE-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "preco-min-cif" THEN
            ASSIGN h-preco-cif = h-objeto.

         IF h-objeto:NAME = "preco-min-fob" THEN
            ASSIGN h-preco-fob = h-objeto.

         IF h-objeto:NAME = "cod-refer" THEN
            ASSIGN h-cod-refer = h-objeto.

         IF h-objeto:NAME = "it-codigo" THEN
            ASSIGN h-it-codigo = h-objeto.

         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.

   CREATE TEXT tx-preco-rp
          ASSIGN FRAME        = p-wgh-frame
                 FORMAT       = "x(17)"
                 WIDTH        = 14
                 SCREEN-VALUE = "Pre‡o RP Med:"
                 ROW          = 7.55
                 COL          = 70.5
                 VISIBLE      = YES.
   CREATE FILL-IN wh-preco-rp
          ASSIGN FRAME              = p-wgh-frame
                 SIDE-LABEL-HANDLE  = tx-preco-rp:HANDLE 
                 DATA-TYPE          = "DECIMAL" 
                 FORMAT             = ">>,>>9.99" 
                 WIDTH              = 10
                 HEIGHT             = 0.88
                 ROW                = 7.4
                 COL                = 81  
                 LABEL              = "Pre‡o RP Med:" 
                 VISIBLE            = YES
                 SENSITIVE          = no.

   CREATE TEXT tx-preco-rd
          ASSIGN FRAME        = p-wgh-frame
                 FORMAT       = "x(17)"
                 WIDTH        = 14
                 SCREEN-VALUE = "Pre‡o RD/RP Peq:"
                 ROW          = 8.55
                 COL          = 68.3
                 VISIBLE      = YES.
   CREATE FILL-IN wh-preco-rd
          ASSIGN FRAME              = p-wgh-frame
                 SIDE-LABEL-HANDLE  = tx-preco-rd:HANDLE  
                 DATA-TYPE          = "DECIMAL" 
                 FORMAT             = ">>,>>9.99" 
                 WIDTH              = 10
                 HEIGHT             = 0.88
                 ROW                = 8.4
                 COL                = 81
                 LABEL              = "Pre‡o RD/RP Peq:" 
                 VISIBLE            = YES
                 SENSITIVE          = NO.

   CREATE TOGGLE-BOX wh-all-ref
       ASSIGN FRAME              = p-wgh-frame
              WIDTH              = 10
              HEIGHT             = 0.88
              ROW                = 2.3
              COL                = 35.7
              LABEL              = "Todas" 
              VISIBLE            = YES
              SENSITIVE          = NO
              TRIGGERS:
                   ON "VALUE-CHANGED" PERSISTENT RUN esupc/upc-cd1508b1.p (INPUT h-cod-refer).
              END TRIGGERS.

   wh-preco-rp:MOVE-AFTER-TAB(h-preco-cif).
   wh-preco-rd:MOVE-AFTER-TAB(h-preco-fob).
   wh-all-ref:MOVE-AFTER-TAB(h-cod-refer).
END.

IF p-ind-event = "AFTER-ENABLE" THEN 
   ASSIGN wh-preco-rp:SENSITIVE = YES
          wh-preco-rd:SENSITIVE = YES
          wh-all-ref:SENSITIVE = h-cod-refer:SENSITIVE.

IF p-ind-event = "AFTER-DISPLAY" THEN DO:
   FIND preco-item WHERE
        ROWID(preco-item) = p-row-table NO-LOCK NO-ERROR.

   FIND preco-item-ext WHERE
        preco-item-ext.nr-tabpre = preco-item.nr-tabpre AND
        preco-item-ext.it-codigo = preco-item.it-codigo AND
        preco-item-ext.cod-refer = preco-item.cod-refer
        NO-LOCK NO-ERROR.

   IF AVAIL preco-item-ext THEN
      ASSIGN wh-preco-rp:SCREEN-VALUE = STRING(preco-item-ext.preco-rp)
             wh-preco-rd:SCREEN-VALUE = STRING(preco-item-ext.preco-rd).
   ELSE 
      ASSIGN wh-preco-rp:SCREEN-VALUE = '0'
             wh-preco-rd:SCREEN-VALUE = '0'.
END.

IF p-ind-event = "BEFORE-ASSIGN" THEN DO:
   IF LOGICAL(wh-all-ref:SCREEN-VALUE) THEN DO.
      FIND item WHERE
           item.it-codigo = h-it-codigo:SCREEN-VALUE NO-LOCK NO-ERROR.
  
      FIND FIRST ref-item WHERE
                 ref-item.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

      IF AVAIL ref-item THEN DO.
         ASSIGN h-cod-refer:SCREEN-VALUE = ref-item.cod-refer.
         APPLY 'LEAVE' TO h-cod-refer.
      END.
   END.

   FIND preco-item WHERE
        ROWID(preco-item) = p-row-table NO-LOCK NO-ERROR.
   IF AVAIL preco-item THEN DO.
      FIND preco-item-ext WHERE
           preco-item-ext.nr-tabpre = preco-item.nr-tabpre AND
           preco-item-ext.it-codigo = preco-item.it-codigo AND
           preco-item-ext.cod-refer = preco-item.cod-refer
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL preco-item-ext THEN DO.
         ASSIGN preco-item-ext.preco-rd   = DECIMAL(wh-preco-rd:SCREEN-VALUE)  
                preco-item-ext.preco-rp   = DECIMAL(wh-preco-rp:SCREEN-VALUE).
      END.
      ELSE DO.
         CREATE preco-item-ext.
         ASSIGN preco-item-ext.cod-refer  = preco-item.cod-refer
                preco-item-ext.it-codigo  = preco-item.it-codigo
                preco-item-ext.nr-tabpre  = IF AVAIL preco-item THEN preco-item.nr-tabpre ELSE ? 
                preco-item-ext.preco-rd   = DECIMAL(wh-preco-rd:SCREEN-VALUE)
                preco-item-ext.preco-rp   = DECIMAL(wh-preco-rp:SCREEN-VALUE).
      END. 
   END.
END.

