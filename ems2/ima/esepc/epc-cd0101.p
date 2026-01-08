def input param p-ind-event        as char          no-undo.
def input param p-ind-object       as char          no-undo.
def input param p-wgh-object       as handle        no-undo.
def input param p-wgh-frame        as widget-handle no-undo.
def input param p-cod-table        as char          no-undo.
def input param p-row-table        as rowid         no-undo.

DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
def var c-objeto as char no-undo.

DEF VAR wh-obj AS WIDGET-HANDLE EXTENT 20 NO-UNDO.
DEF VAR i-level AS INTEGER INITIAL 1.

DEF NEW GLOBAL SHARED VAR h-cd0101 AS HANDLE NO-UNDO.
DEF VAR adm-current-page AS INTEGER NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-fi-usuario      AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-tx-usuario      AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fi-senha        AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-tx-senha        AS HANDLE NO-UNDO.

assign c-objeto = entry(num-entries(p-wgh-object:private-data, "~/"), p-wgh-object:private-data, "~/").
/*
OUTPUT TO c:\ponto-epc.txt APPEND.

DISPLAY p-ind-event FORMAT "x(30)"
        p-ind-object FORMAT "x(30)"
        c-objeto FORMAT "x(30)"
        p-cod-table FORMAT "x(30)"
        STRING(p-row-table) FORMAT "x(20)"
        WITH DOWN NO-LABELS WIDTH 200.
OUTPUT CLOSE.
*/


IF p-ind-event = 'BEFORE-INITIALIZE' AND 
   p-ind-object = 'CONTAINER' THEN 
   ASSIGN h-cd0101 = p-wgh-object.

IF p-ind-event  = "BEFORE-INITIALIZE" AND 
   p-ind-object = "VIEWER"            AND  
   c-objeto     = "v01un192.w"        THEN DO:
    /*
   CREATE TEXT h-tx-usuario
       ASSIGN FRAME        = p-wgh-frame
              FORMAT       = "x(14)"
              WIDTH        = 6
              ROW          = 2
              COLUMN       = 20
              SCREEN-VALUE = "Usu rio :"
              VISIBLE      = YES .
              
   
   CREATE FILL-IN h-fi-usuario
       ASSIGN FRAME             = p-wgh-frame
              SIDE-LABEL-HANDLE = h-tx-usuario
              DATA-TYPE         = "CHARACTER"
              FORMAT            = "x(50)"
              WIDTH             = 18.3
              ROW               = 1.9
              COL               = 27.15
              HEIGHT            = 0.88
              TOOLTIP           = "Usu rio"
              VISIBLE           = YES 
              SENSITIVE         = YES.
              /*TRIGGERS:
                  /*ON "entry":U PERSISTENT RUN esepc/epc-cd0401a.p.*/
                  ON "MOUSE-SELECT-DBLCLICK":U PERSISTENT RUN  eszoom/z01ra001.w.
                  ON "leave":U PERSISTENT RUN esepc/epc-cd0401a.p.
              END TRIGGERS.*/
   
   CREATE TEXT h-tx-senha
       ASSIGN FRAME        = p-wgh-frame
              FORMAT       = "x(14)"
              WIDTH        = 6
              ROW          = 2
              COLUMN       = 48
              SCREEN-VALUE = "Senha :"
              VISIBLE      = YES .

   CREATE FILL-IN h-fi-senha
       ASSIGN FRAME             = p-wgh-frame
              SIDE-LABEL-HANDLE = h-tx-senha
              DATA-TYPE         = "CHARACTER"
              FORMAT            = "x(15)"
              WIDTH             = 15
              ROW               = 1.9
              COL               = 54
              HEIGHT            = 0.88
              TOOLTIP           = "Senha"
              PASSWORD-FIELD    = YES
              VISIBLE           = YES 
              SENSITIVE         = YES.
              /*TRIGGERS:
                  /*ON "entry":U PERSISTENT RUN esepc/epc-cd0401a.p.*/
                  ON "MOUSE-SELECT-DBLCLICK":U PERSISTENT RUN  eszoom/z01ra001.w.
                  ON "leave":U PERSISTENT RUN esepc/epc-cd0401a.p.
              END TRIGGERS.*/
      */
END.

IF p-ind-event  = "DISPLAY"    AND 
   p-ind-object = "VIEWER"     AND  
   c-objeto     = "v01un192.w" THEN DO:
   /*
   FIND FIRST param-global NO-LOCK NO-ERROR.                               
   ASSIGN h-fi-usuario:SCREEN-VALUE = SUBSTR(param-global.char-2,200,50) 
          h-fi-senha:SCREEN-VALUE   = SUBSTR(param-global.char-2,251,15). */
END.

IF p-ind-event  = "ASSIGN"     AND 
   p-ind-object = "VIEWER"     AND  
   c-objeto     = "v01un192.w" THEN DO:
   
    /*
   FIND FIRST param-global NO-LOCK NO-ERROR.
   
   ASSIGN SUBSTR(param-global.char-2,200,50) = ""
          SUBSTR(param-global.char-2,251,15) = "".

   ASSIGN SUBSTR(param-global.char-2,200,50) = h-fi-usuario:SCREEN-VALUE
          SUBSTR(param-global.char-2,251,15) = h-fi-senha:SCREEN-VALUE.
   */
END.

/*
IF p-ind-event  = "ENABLE"    AND 
   c-objeto     = "v01un192.w" THEN DO:
   RUN GET-ATTRIBUTE IN h-cd0101 ('Current-Page':U).
   ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

   RUN select-page IN h-cd0101 (INPUT 5).
   RUN select-page IN h-cd0101 (INPUT adm-current-page).
END.
*/
