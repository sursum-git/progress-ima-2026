
/* Parameter Definitions ****************************************************/
define input parameter p-ind-event  as character.
define input parameter p-ind-object as character.
define input parameter p-wgh-object as handle.
define input parameter p-wgh-frame  as widget-handle.
define input parameter p-cod-table  as character.
define input parameter p-row-table  as rowid.

DEF NEW GLOBAL SHARED VAR c-seg-usuario   AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-romaneio     AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-win    AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-frame  AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

/* Main Block ***************************************************************/

IF p-wgh-frame:NAME = "f-pg-sel" AND
   NOT VALID-HANDLE(wh-romaneio) THEN DO.

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = "rect-10" THEN
         ASSIGN h-objeto:row = 9.4.

      IF h-objeto:NAME = "rs-imprime" THEN DO.
         ASSIGN h-objeto:row = 9.8.

         FIND usuar_grp_usuar WHERE 
              usuar_grp_usuar.cod_usuar     = c-seg-usuario AND
              (usuar_grp_usuar.cod_grp_usuar = "EP0" OR
               usuar_grp_usuar.cod_grp_usuar = "UCR") 
              NO-LOCK NO-ERROR.

         IF NOT AVAIL usuar_grp_usuar THEN 
            h-objeto:DISABLE(ENTRY(1, h-objeto:RADIO-BUTTONS)).
      END.
      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.

   CREATE TOGGLE-BOX wh-romaneio
        ASSIGN FRAME              = p-wgh-frame
               ROW                = 8.2
               COL                = 28.8
               VISIBLE            = YES
               LABEL              = "Emitir Romaneio"
               SENSITIVE          = YES.

   FIND usuar_grp_usuar WHERE 
        usuar_grp_usuar.cod_usuar     = c-seg-usuario AND
        usuar_grp_usuar.cod_grp_usuar = "EP0" 
        NO-LOCK NO-ERROR.
   
   IF AVAIL usuar_grp_usuar THEN 
      ASSIGN wh-romaneio:SCREEN-VALUE = 'YES'. 

   ASSIGN h-win = p-wgh-frame:WINDOW.
   ASSIGN h-frame = h-win:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-frame):
      IF h-frame:NAME = "f-relat" THEN DO.
         ASSIGN h-objeto = h-frame:FIRST-CHILD.
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
         DO WHILE VALID-HANDLE(h-objeto):
            IF h-objeto:NAME = "im-pg-imp" THEN
               APPLY 'mouse-select-click' TO h-objeto.
            ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
         END.
      END.
      ASSIGN h-frame = h-frame:NEXT-SIBLING.
   END.
END.

IF p-wgh-frame:NAME = "f-pg-imp" THEN DO.
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = "text-destino-bloq" THEN
         ASSIGN h-objeto:WIDTH = 21
                h-objeto:SCREEN-VALUE = "Destino Bloqueto / Romaneio". 

      IF h-objeto:NAME = "rs-destino-bloq" THEN DO.
         ON 'mouse-select-click':U OF h-objeto PERSISTENT RUN esupc/upc-ft0513m2.p (INPUT p-wgh-frame).  

         IF wh-romaneio:SCREEN-VALUE = 'YES' THEN DO. 
            ASSIGN h-objeto:SCREEN-VALUE = '1'.
            APPLY 'mouse-select-click' TO h-objeto.
         END.
      END.

      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.

   ASSIGN h-win = p-wgh-frame:WINDOW.
   ASSIGN h-frame = h-win:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-frame):
      IF h-frame:NAME = "f-relat" THEN DO.
         ASSIGN h-objeto = h-frame:FIRST-CHILD.
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
         DO WHILE VALID-HANDLE(h-objeto):
            IF h-objeto:NAME  = "bt-executar" THEN 
               ON 'mouse-select-click':U OF h-objeto PERSISTENT RUN esupc/upc-ft0513m1.p (INPUT p-wgh-frame). 
            ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
         END.
      END.
      ASSIGN h-frame = h-frame:NEXT-SIBLING.
   END.
END.

