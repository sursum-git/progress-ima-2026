/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Variable Definitions *****************************************************/
DEF NEW GLOBAL SHARED VAR  h-restr-priorid     AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR  h-lbl-restr-priorid AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR  h-tg-ccredito       AS HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-ind-cre-cli         AS HANDLE NO-UNDO.

DEF VAR c-objeto     AS CHAR NO-UNDO.
DEF VAR h-objeto     AS HANDLE NO-UNDO.
DEF VAR i-ct         AS INTEGER.
DEF VAR c-lst-priori AS CHAR INIT "10,12,15,16,17,18".

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "INITIALIZE" AND 
   c-objeto = 'v32ad098.w' THEN DO. 

   CREATE TOGGLE-BOX h-tg-ccredito
          ASSIGN FRAME        = p-wgh-frame
                 LABEL        = 'S¢ Cart∆o Cred.' 
                 WIDTH        = 13
                 ROW          = 7.1
                 COLUMN       = 8.5
                 HEIGHT       = 0.88
                 VISIBLE      = YES
                 SENSITIVE    = NO
                 TOOLTIP      = "".

   CREATE FILL-IN h-restr-priorid
       ASSIGN FRAME     = p-wgh-frame
              DATA-TYPE = "CHARACTER"
              FORMAT    = "x(20)"
              WIDTH     = 15.4
              VISIBLE   = YES
              SENSITIVE = NO
              HEIGHT    = 0.88
              HELP      = "Informe Prioridades (" + c-lst-priori + ") Separadas por V°rgula (,)"
              TOOLTIP   = "Prioridades Restritas para Venda".

   CREATE TEXT h-lbl-restr-priorid
       ASSIGN FRAME     = p-wgh-frame 
              FORMAT    = "x(19)" 
              WIDTH     = 12           
              VISIBLE   = YES         
              SENSITIVE = NO          
              SCREEN-VALUE = "Restr. Prioridade:".

   
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto).
      
      IF h-objeto:NAME = 'ind-cre-cli' THEN DO.
         ASSIGN h-ind-cre-cli = h-objeto.

         ASSIGN h-objeto:RADIO-BUTTONS = REPLACE(h-objeto:RADIO-BUTTONS,
                                                 ENTRY(5,h-objeto:RADIO-BUTTONS),
                                                 "Desativado"). 

         ASSIGN h-objeto:RADIO-BUTTONS = REPLACE(h-objeto:RADIO-BUTTONS,
                                                 ENTRY(9,h-objeto:RADIO-BUTTONS),
                                                 ENTRY(9,h-objeto:RADIO-BUTTONS) + " ou DÇbito"). 

         //ON "CURSOR-UP":U OF h-objeto PERSISTENT RUN esepc/epc-cm0102a.p.
      END.
      
      IF h-objeto:NAME = 'lim-adicional' THEN
         ASSIGN h-objeto:WIDTH = h-objeto:WIDTH - 4.6.

      IF h-objeto:NAME = 'dt-fim-cred' THEN DO.
         ASSIGN h-restr-priorid:ROW = h-objeto:ROW - 1
                h-lbl-restr-priorid:ROW = h-objeto:ROW - 0.9
                h-restr-priorid:COL = h-objeto:COL + 26
                h-lbl-restr-priorid:COL = h-objeto:COL + 14.

         h-restr-priorid:MOVE-BEFORE-TAB-ITEM(h-objeto).

         LEAVE.
      END.
      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
   
END.

IF p-ind-event = 'DISPLAY' AND 
   c-objeto = 'v32ad098.w' THEN DO.
   FIND emitente WHERE
        ROWID(emitente) = p-row-table NO-ERROR.

   FIND ext-emitente WHERE
        ext-emitente.cod-emit = emitente.cod-emit NO-LOCK NO-ERROR.

   IF VALID-HANDLE(h-restr-priorid) THEN DO.
      ASSIGN h-restr-priorid:SCREEN-VALUE = ''
             h-tg-ccredito:SCREEN-VALUE = 'NO'.
          
      IF AVAIL ext-emitente THEN 
         ASSIGN h-restr-priorid:SCREEN-VALUE = STRING(ext-emitente.restr-priorid)
                h-tg-ccredito:SCREEN-VALUE = STRING(ext-emitente.cred-so-cartao).
   END.
END.

IF p-ind-event = 'ENABLE' AND 
   c-objeto = 'v32ad098.w' THEN 
   ASSIGN h-restr-priorid:SENSITIVE = YES
          h-tg-ccredito:SENSITIVE = YES.

IF p-ind-event = 'DISABLE' AND
   c-objeto = 'v32ad098.w' THEN 
   ASSIGN h-restr-priorid:SENSITIVE = NO
          h-tg-ccredito:SENSITIVE = NO.

IF p-ind-event = 'VALIDATE' AND
   c-objeto = 'v32ad098.w' THEN DO.
   IF h-restr-priorid:SCREEN-VALUE <> '' THEN DO.
      DO i-ct = 1 TO NUM-ENTRIES(h-restr-priorid:SCREEN-VALUE).
         IF LOOKUP(ENTRY(i-ct,h-restr-priorid:SCREEN-VALUE),c-lst-priori) = 0 THEN DO.
            MESSAGE 'Existem Prioridades Inv†lidas, Verifique'
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY 'ENTRY' TO h-restr-priorid.
            RETURN 'NOK'.
         END.
      END.
   END.
END.

IF p-ind-event = 'ASSIGN' AND
   c-objeto = 'v32ad098.w' THEN DO.
   FIND emitente WHERE
        ROWID(emitente) = p-row-table NO-ERROR.

   FIND ext-emitente WHERE
        ext-emitente.cod-emit = emitente.cod-emit NO-ERROR.

   IF NOT AVAIL ext-emitente THEN DO.
      CREATE ext-emitente.
      ASSIGN ext-emitente.cod-emit = emitente.cod-emit.
   END.
   ASSIGN ext-emitente.restr-priorid = h-restr-priorid:INPUT-VALUE
          ext-emitente.cred-so-cartao = h-tg-ccredito:INPUT-VALUE.
END.

