DEF INPUT PARAM p-ind-event   AS CHAR          NO-UNDO FORMAT 'x(50)'.
DEF INPUT PARAM p-ind-object  AS CHAR          NO-UNDO FORMAT 'x(50)'.
DEF INPUT PARAM p-wgh-object  AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame   AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table   AS CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table   AS ROWID         NO-UNDO.
def new global shared var h-campo         as widget-handle no-undo.

DEF NEW GLOBAL SHARED VAR p-wgh-frame-pg-1  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-lbl-cod-rejei  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cod-rejei      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cb-cod-observa AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-desc-rejei     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cod-estabel    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR l-atu             AS LOGICAL.

DEFINE VAR c-objeto       AS CHAR NO-UNDO FORMAT 'x(50)'.
DEFINE VAR h-objeto       AS WIDGET-HANDLE NO-UNDO.
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:private-data, "~/").

IF p-ind-event = "before-INITIALIZE" THEN 
   ASSIGN l-atu = NO.
IF p-ind-event = "AFTER-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD. 
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
          /*MESSAGE h-objeto:NAME
              VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

          IF  h-objeto:NAME = "fPage1" THEN DO:
              ASSIGN h-campo = h-objeto:FIRST-CHILD
                     p-wgh-frame-pg-1 = h-objeto. 
              CREATE TEXT wh-lbl-cod-rejei
              ASSIGN FRAME         = p-wgh-frame-pg-1
                     FORMAT        = "x(15)"
                     WIDTH         = 20
                     SCREEN-VALUE  = "Cod. Devolu‡Æo:"
                     ROW           = 9.8
                     COL           = 37.6
                     VISIBLE       = YES.

              CREATE FILL-IN wh-cod-rejei
              ASSIGN FRAME             = p-wgh-frame-pg-1
                     SIDE-LABEL-HANDLE = wh-lbl-cod-rejei
                     DATA-TYPE         = "integer"
                     FORMAT            = ">>9"
                     WIDTH             = 4
                     ROW               = 9.6
                     COL               = 49.3
                     VISIBLE           = YES
                     SENSITIVE         = YES
                     HEIGHT            = 0.88
                     TOOLTIP           = "C¢digo da Devolu‡Æo"
                     TRIGGERS:
                         ON "MOUSE-SELECT-DBLCLICK":U PERSISTENT RUN esepc/epc-re2001a-z.p.
                         ON "LEAVE":U PERSISTENT RUN esepc/epc-re2001a-s1.p.
                     END TRIGGERS.
                  
                  CREATE FILL-IN wh-desc-rejei
                  ASSIGN FRAME             = p-wgh-frame-pg-1
                         DATA-TYPE         = "CHARACTER"
                         FORMAT            = "x(30)"
                         WIDTH             = 28
                         ROW               = 9.6
                         COL               = 53.7
                         VISIBLE           = YES
                         SENSITIVE         = NO
                         HEIGHT            = 0.88.
              DO  WHILE VALID-HANDLE(h-campo):
                  /*MESSAGE h-campo:NAME
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
                  IF  h-campo:TYPE <> "field-group" THEN DO:

                      IF h-campo:NAME = 'cb-cod-observa' THEN DO:
                         ASSIGN wh-cb-cod-observa = h-campo.
                      END.
                      
                  
                      ASSIGN h-campo = h-campo:NEXT-SIBLING.
                  END.
                  ELSE
                      ASSIGN h-campo = h-campo:FIRST-CHILD.
              END.
              wh-cod-rejei:LOAD-MOUSE-POINTER("image/lupa.cur":U).
              IF h-objeto:NAME = 'dt-trans' THEN 
                 wh-cod-rejei:MOVE-AFTER-TAB-ITEM(h-objeto).  

              IF  wh-cb-cod-observa:SCREEN-VALUE <> 'Devolu‡Æo Cliente' THEN
                  ASSIGN wh-cod-rejei:SENSITIVE = NO
                         wh-cod-rejei:SCREEN-VALUE = ''.
              /*IF VALID-HANDLE(wh-cb-cod-observa) THEN
                 MESSAGE 'wh:' string(p-row-table)
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

              IF p-row-table <> ? AND l-atu = NO AND wh-cb-cod-observa:SCREEN-VALUE = 'Devolu‡Æo Cliente' THEN DO:
                  ASSIGN l-atu = YES.
                  FIND FIRST docum-est NO-LOCK
                      WHERE rowid(docum-est) = p-row-table NO-ERROR.
/*                   MESSAGE AVAIL docum-est SKIP           */
/*                           string(p-row-table)            */
/*                       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
                  IF AVAIL docum-est THEN DO:
/*                       MESSAGE 'entrei do docum-est'          */
/*                           VIEW-AS ALERT-BOX INFO BUTTONS OK. */
                     ASSIGN wh-cod-rejei:SCREEN-VALUE = string(docum-est.int-2).
/*                      MESSAGE wh-cod-rejei:SCREEN-VALUE      */
/*                          VIEW-AS ALERT-BOX INFO BUTTONS OK. */
                     FIND cod-rejeicao WHERE 
                           cod-rejeicao.codigo-rejei = INT(wh-cod-rejei:SCREEN-VALUE) NO-LOCK NO-ERROR.
                     IF AVAIL cod-rejeicao THEN DO:
/*                          MESSAGE 'achou o c¢digo de rejeicao'   */
/*                              VIEW-AS ALERT-BOX INFO BUTTONS OK. */
                        ASSIGN wh-desc-rejei:SCREEN-VALUE  = cod-rejeicao.descricao.
                     END.
                        
                  END.
              END.


          END.
      END.
      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.   
END.

IF p-ind-event = "after-assign"  THEN DO:

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO: 
         IF  h-objeto:NAME = "fPage1" THEN DO:
             FIND docum-est EXCLUSIVE-LOCK
                 WHERE rowid(docum-est) = p-row-table NO-ERROR.
             IF AVAIL docum-est THEN
                ASSIGN docum-est.int-2 = INT(wh-cod-rejei:SCREEN-VALUE).
         END.
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
   END.


END.





  
 
    


