/* Programa: 
** Objetivo: 
**           
** Autor...: Toninho
*/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/

/* Variable Definitions *****************************************************/
DEF VAR c-objeto       AS CHAR NO-UNDO.

/* Variavies criadas na viewer dinamicamente*********************************/
DEF NEW GLOBAL SHARED VAR wh-cap-prod-dia AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-cap-prod-dia AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-nr-seq-prod  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-seq-prod     AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-compoe-seq   AS WIDGET-HANDLE NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "VIEWER" AND 
   c-objeto = 'v17in144.w' THEN DO:

   CREATE TOGGLE-BOX wh-compoe-seq
          ASSIGN FRAME              = p-wgh-frame
                 ROW                = 6
                 COL                = 55
                 VISIBLE            = YES
                 LABEL              = "Compäe Sequenciamento de Produ‡ao"
                 SENSITIVE          = NO
                 TOOLTIP            = ""
                 TRIGGERS:
                     ON "VALUE-CHANGED":U PERSISTENT RUN esupc/upc-cd0111a.p. 
                 END TRIGGERS.


   CREATE TEXT tx-seq-prod
          ASSIGN FRAME        = p-wgh-frame
                 FORMAT       = "x(13)"
                 WIDTH        = 10
                 SCREEN-VALUE = "Sequencia:"
                 ROW          = 7.1
                 COL          = 47
                 VISIBLE      = YES.
   CREATE FILL-IN wh-nr-seq-prod
          ASSIGN FRAME              = p-wgh-frame
                 SIDE-LABEL-HANDLE  = tx-seq-prod:HANDLE  
                 DATA-TYPE          = "INTEGER" 
                 FORMAT             = ">>9" 
                 WIDTH              = 4
                 HEIGHT             = 0.88
                 ROW                = 6.9
                 COL                = 55
                 LABEL              = "Sequencia:" 
                 VISIBLE            = YES
                 SENSITIVE          = NO.


   CREATE TEXT tx-cap-prod-dia
          ASSIGN FRAME        = p-wgh-frame
                 FORMAT       = "x(20)"
                 WIDTH        = 20
                 SCREEN-VALUE = "Capacidade Prod/Dia:"
                 ROW          = 8.1
                 COL          = 39.5
                 VISIBLE      = YES.
   CREATE FILL-IN wh-cap-prod-dia
          ASSIGN FRAME              = p-wgh-frame
                 SIDE-LABEL-HANDLE  = tx-cap-prod-dia:HANDLE 
                 DATA-TYPE          = "DECIMAL" 
                 FORMAT             = ">>>,>>9.99" 
                 WIDTH              = 10
                 HEIGHT             = 0.88
                 ROW                = 7.9
                 COL                = 55
                 LABEL              = "Capacidade Prod/Dia:" 
                 VISIBLE            = YES
                 SENSITIVE          = no.

END.

IF p-ind-event = "DISPLAY" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v17in144.w" THEN DO:

   FIND grup-maquina WHERE
        ROWID(grup-maquina) = p-row-table NO-LOCK NO-ERROR.

   IF VALID-HANDLE(wh-cap-prod-dia) THEN DO.
      FIND cf-ext-grup-maquina WHERE
           cf-ext-grup-maquina.gm-codigo = grup-maquina.gm-codigo NO-LOCK NO-ERROR.
    
      IF AVAIL cf-ext-grup-maquina THEN 
         ASSIGN wh-cap-prod-dia:SCREEN-VALUE = STRING(cf-ext-grup-maquina.cap-prod-dia)
                wh-nr-seq-prod:SCREEN-VALUE = STRING(cf-ext-grup-maquina.nr-seq-prod)
                wh-compoe-seq:SCREEN-VALUE = STRING(cf-ext-grup-maquina.log-seq-prod).
      ELSE
         ASSIGN wh-cap-prod-dia:SCREEN-VALUE = ""
                wh-nr-seq-prod:SCREEN-VALUE = ""
                wh-compoe-seq:SCREEN-VALUE = 'NO'.
   END.
END.

IF p-ind-event = "ENABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v17in144.w" THEN DO:

   ASSIGN wh-cap-prod-dia:SENSITIVE = YES
          wh-compoe-seq:SENSITIVE = YES.
END.

IF p-ind-event = "ADD" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v17in144.w" THEN DO:

   ASSIGN wh-cap-prod-dia:LABEL = "Capacidade Prod/Dia:"
          wh-nr-seq-prod:LABEL = "Sequencia:".
END.

IF p-ind-event = "DISABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v17in144.w" THEN DO:

   ASSIGN wh-cap-prod-dia:SENSITIVE = NO
          wh-nr-seq-prod:SENSITIVE = NO
          wh-compoe-seq:SENSITIVE = NO.
END.

IF p-ind-event = "ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v17in144.w" THEN DO:

   FIND grup-maquina WHERE
        ROWID(grup-maquina) = p-row-table NO-LOCK NO-ERROR.

   FIND cf-ext-grup-maquina WHERE
        cf-ext-grup-maquina.gm-codigo = grup-maquina.gm-codigo NO-ERROR.

   IF NOT AVAIL cf-ext-grup-maquina THEN DO.
      CREATE cf-ext-grup-maquina.
      ASSIGN cf-ext-grup-maquina.gm-codigo = grup-maquina.gm-codigo.
   END.
   
   ASSIGN cf-ext-grup-maquina.cap-prod-dia = DEC(wh-cap-prod-dia:SCREEN-VALUE)    
          cf-ext-grup-maquina.nr-seq-prod  = INT(wh-nr-seq-prod:SCREEN-VALUE)
          cf-ext-grup-maquina.log-seq-prod = LOGICAL(wh-compoe-seq:SCREEN-VALUE).
END.

