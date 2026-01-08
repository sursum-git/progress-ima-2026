/*******************************************************************
** Programa: epc-cd0704.p                                         **
** Objetivo: Epc do programa de cadastro de emitente(cd0704)      **
** Autor...: Anderson Fagner Maio/2009                            **
** Observ..:                                                      **  
*******************************************************************/


def input param p-ind-event        as char          no-undo.
def input param p-ind-object       as char          no-undo.
def input param p-wgh-object       as handle        no-undo.
def input param p-wgh-frame        as widget-handle no-undo.
def input param p-cod-table        as char          no-undo.
def input param p-row-table        as rowid         no-undo.

DEF NEW GLOBAL SHARED VAR h-fi-ramo      AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fi-desc-ramo AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-tx-ramo      AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-data-implant AS HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR vg-cod-ramo-ativ AS INT NO-UNDO.

DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
def var c-objeto as char no-undo.
DEF VAR wh-obj AS WIDGET-HANDLE EXTENT 20 NO-UNDO.
DEF VAR i-level AS INTEGER INITIAL 1.


DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.


/*
def var wh-pesquisa as widget-handle.
def new global shared var l-implanta as logical init no.
def new global shared var wh-fill    as widget-handle no-undo.
def new global shared var wh-window  as handle no-undo.
def new global shared var adm-broker-hdl as handle no-undo.
  */


assign c-objeto = entry(num-entries(p-wgh-object:private-data, "~/"), p-wgh-object:private-data, "~/").
               /*
OUTPUT TO c:\ponto-epc.txt APPEND.

DISPLAY p-ind-event FORMAT "x(30)"
        p-ind-object FORMAT "x(30)"
        c-objeto FORMAT "x(30)"
        p-cod-table FORMAT "x(30)"
        STRING(p-row-table) FORMAT "x(20)"
        WITH DOWN NO-LABELS WIDTH 200.
OUTPUT CLOSE.*/

/*
RUN CepOnline/upc/upc-cd0704.p (INPUT p-ind-event,
                                INPUT p-ind-object,
                                INPUT p-wgh-object,
                                INPUT p-wgh-frame,
                                INPUT p-cod-table,
                                INPUT p-row-table).

*/

IF p-ind-event  = "BEFORE-INITIALIZE"    AND 
   p-ind-object = "VIEWER"               AND 
   c-objeto     = "'advwr\v24ad099.w'"  THEN DO:

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = 'atividade' OR h-objeto:NAME = 'telef-modem' THEN
         h-objeto:VISIBLE = NO.

      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
END.

IF p-ind-event  = "BEFORE-INITIALIZE"    AND 
   p-ind-object = "VIEWER"               AND 
   c-objeto     = "'advwr\v24ad098.w'"  THEN DO:

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = 'atividade' OR h-objeto:NAME = 'telef-modem' THEN
         h-objeto:VISIBLE = NO.

      IF h-objeto:NAME = 'data-implant' THEN 
         ASSIGN h-data-implant = h-objeto.

      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.

   CREATE TEXT h-tx-ramo
       ASSIGN FRAME     = p-wgh-frame
              FORMAT       = "x(14)"
              WIDTH        = 8
              ROW          = 5.15
              COLUMN       = 53
              SCREEN-VALUE = "Cod Ramo:"
              VISIBLE      = YES.
    
   CREATE FILL-IN h-fi-ramo
       ASSIGN FRAME             = p-wgh-frame
              SIDE-LABEL-HANDLE = h-tx-ramo
              DATA-TYPE         = "integer"
              FORMAT            = "999"
              WIDTH             = 4
              ROW               = 5
              COL               = 61.3
              VISIBLE           = YES
              SENSITIVE         = NO
              HEIGHT            = 0.88
              TOOLTIP           = "C¢digo Ramo de Atividade"
              TRIGGERS:
                  ON "MOUSE-SELECT-DBLCLICK":U PERSISTENT RUN esepc/epc-cd0704z.w.
                  ON "leave":U PERSISTENT RUN esepc/epc-cd0704a.p.
              END TRIGGERS.

   h-fi-ramo:LOAD-MOUSE-POINTER("image/lupa.cur":U).
              
   CREATE FILL-IN h-fi-desc-ramo
       ASSIGN FRAME     = p-wgh-frame
              DATA-TYPE = "CHARACTER"
              FORMAT    = "x(35)"
              WIDTH     = 19.3
              ROW       = 5
              COL       = 65.5
              VISIBLE   = YES
              SENSITIVE = NO
              HEIGHT    = 0.88
              TOOLTIP   = "Descri‡Æo Ramo de Atividade".
   
END.

IF p-ind-event  = 'ENABLE'     AND
   c-objeto     = "'advwr\v24ad098.w'"  THEN DO:
   ASSIGN h-fi-ramo:SENSITIVE = YES.
END.
IF p-ind-event  = 'AFTER-ENABLE'     AND
   c-objeto     = "'advwr\v24ad098.w'"  THEN DO:
   ASSIGN h-data-implant:SENSITIVE = NO.
END.


IF p-ind-event  = 'DISABLE'     AND
   c-objeto     = "'advwr\v24ad098.w'"  THEN DO:
   h-fi-ramo:SENSITIVE = NO.
END.

IF p-ind-event  = 'DISPLAY'     AND
   c-objeto     = "'advwr\v24ad098.w'"  THEN DO:
   
   FIND emitente WHERE
        ROWID(emitente) = p-row-table NO-LOCK NO-ERROR.

   IF AVAIL emitente THEN DO:
       
       FIND FIRST ext-emitente WHERE ext-emitente.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.
       FIND FIRST ramo-ativ WHERE ramo-ativ.cod-ramo-ativ = ext-emitente.cod-ramo-ativ NO-LOCK NO-ERROR.
       
       IF AVAIL ext-emitente THEN
          h-fi-ramo:SCREEN-VALUE = string(ext-emitente.cod-ramo-ativ).
       ELSE
          h-fi-ramo:SCREEN-VALUE = "0000".
       
       IF AVAIL ramo-ativ THEN
          h-fi-desc-ramo:SCREEN-VALUE = ramo-ativ.descricao.  
       ELSE
          h-fi-desc-ramo:SCREEN-VALUE = "".
   END.

END.

IF p-ind-event  = 'BEFORE-ASSIGN' AND
   c-objeto     = "'advwr\v23ad098.w'"THEN DO:
     
    RUN esepc/epc-cd0704a.p.
END.

IF p-ind-event  = 'ASSIGN'     AND
   c-objeto     = "'advwr\v24ad098.w'"  THEN DO:
   
   FIND emitente WHERE ROWID(emitente) = p-row-table SHARE-LOCK NO-ERROR.
   
   IF AVAIL emitente THEN DO:
      
      FIND ext-emitente WHERE emitente.cod-emitente = ext-emitente.cod-emitente SHARE-LOCK NO-ERROR.
       
      IF NOT AVAIL ext-emitente THEN DO:
         CREATE ext-emitente.
         ASSIGN ext-emitente.cod-emitente = emitente.cod-emitente.
      
      END.
      
      ASSIGN ext-emitente.cod-ramo-ativ = int(h-fi-ramo:SCREEN-VALUE).

   END.


END.


