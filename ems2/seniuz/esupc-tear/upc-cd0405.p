/****************************************************************************
** Programa: upc-cd0405.p 
** Objetivo: Criar um campo de digitacao Toggle-Box.
**
**           Este Novo campo sera gravado na tabela MENSAGEM-EXT
**
** Autor   :  
*****************************************************************************/
/* Parameter Definitions ****************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR h-cod-mensagem AS WIDGET-HANDLE NO-UNDO.

/* Variable Definitions *****************************************************/
DEF VAR h-objeto          AS WIDGET-HANDLE NO-UNDO.
DEF VAR c-objeto          AS CHAR NO-UNDO.

/* Variavies criadas na viewer dinamicamente*********************************/
DEF NEW GLOBAL SHARED VAR wh-tg-rpr AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-tg-pcp AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-tg-dev AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-doca AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-doca AS HANDLE NO-UNDO.

/* Main Block ***************************************************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01ad176.w" THEN DO:

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "cod-mensagem" THEN 
            ASSIGN h-cod-mensagem = h-objeto
                   h-cod-mensagem:FORMAT = ">>>9".

         IF h-objeto:NAME = 'texto-mensag' THEN 
            ASSIGN h-objeto:HEIGHT = h-objeto:HEIGHT - 1
                   h-objeto:ROW = h-objeto:ROW + 1.

         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.

   CREATE TOGGLE-BOX wh-tg-pcp
          ASSIGN FRAME              = p-wgh-frame
                 ROW                = 3
                 COL                = 50.0
                 VISIBLE            = YES
                 LABEL              = "Utilizado pelo PCP"
                 SENSITIVE          = NO
                 TOOLTIP            = "Campo Utilizado pela Programaá∆o do PCP".

   CREATE TOGGLE-BOX wh-tg-rpr
          ASSIGN FRAME              = p-wgh-frame
                 ROW                = 3.7
                 COL                = 50.0
                 VISIBLE            = YES
                 LABEL              = "Utilizado pelo Reprocesso"
                 SENSITIVE          = NO
                 TOOLTIP            = "Campo Utilizado pelo Reprocesso".

   CREATE TOGGLE-BOX wh-tg-dev
          ASSIGN FRAME              = p-wgh-frame
                 ROW                = 4.4
                 COL                = 50.0
                 VISIBLE            = YES
                 LABEL              = "Utilizado pela DEVOLUÄ«O"
                 SENSITIVE          = NO
                 TOOLTIP            = "Campo Utilizado pela Devoluá∆o de Mercadorias"
                 TRIGGERS:
                     ON "VALUE-CHANGED":U PERSISTENT RUN esupc/upc-cd0405a.p.
                 END TRIGGERS.

   CREATE TEXT tx-doca
          ASSIGN FRAME        = p-wgh-frame
                 FORMAT       = "x(10)"
                 WIDTH        = 10
                 SCREEN-VALUE = "Doca:"
                 ROW          = 4.4
                 COL          = 75
                 VISIBLE      = YES.
   CREATE FILL-IN wh-doca
          ASSIGN FRAME              = p-wgh-frame
                 SIDE-LABEL-HANDLE  = tx-doca:HANDLE 
                 DATA-TYPE          = "CHARACTER" 
                 FORMAT             = "x(6)"
                 WIDTH              = 7
                 HEIGHT             = 0.88
                 ROW                = 4.2
                 COL                = 80  
                 LABEL              = "Doca:" 
                 VISIBLE            = YES
                 SENSITIVE          = NO
            TRIGGERS:
                ON "MOUSE-SELECT-DBLCLICK":U PERSISTENT RUN esupc/upc-cd0405z.p.
            END TRIGGERS.

   wh-doca:LOAD-MOUSE-POINTER("image/lupa.cur").

END.

IF p-ind-event = "ENABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01ad176.w" THEN DO: 
   ASSIGN wh-tg-pcp:SENSITIVE = YES
          wh-tg-rpr:SENSITIVE = YES
          wh-tg-dev:SENSITIVE = YES
          wh-doca:SENSITIVE = LOGICAL(wh-tg-dev:SCREEN-VALUE).

   ASSIGN wh-doca:FORMAT = "999/999".
END.

IF p-ind-event = "DISABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01ad176.w" THEN DO: 
   ASSIGN wh-tg-pcp:SENSITIVE = NO
          wh-tg-rpr:SENSITIVE = NO
          wh-tg-dev:SENSITIVE = NO
          wh-doca:SENSITIVE = NO.

   ASSIGN wh-doca:FORMAT = "999/999".
END.

IF p-ind-event = "ADD" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01ad176.w" THEN DO: 

   FIND LAST mensagem NO-LOCK NO-ERROR.

   ASSIGN wh-tg-pcp:SCREEN-VALUE = "NO"
          wh-tg-rpr:SCREEN-VALUE = "NO"
          wh-tg-dev:SCREEN-VALUE = "NO".

   IF VALID-HANDLE(h-cod-mensagem) THEN
      ASSIGN h-cod-mensagem:SCREEN-VALUE = IF AVAIL mensagem 
                                           THEN STRING(mensagem.cod-mensagem + 1)
                                           ELSE '1'.
END.

IF p-ind-event = "DISPLAY" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01ad176.w" THEN DO:

   IF VALID-HANDLE(wh-tg-pcp) THEN
      ASSIGN wh-tg-pcp:SCREEN-VALUE = "NO"
             wh-tg-rpr:SCREEN-VALUE = "NO"
             wh-tg-dev:SCREEN-VALUE = "NO".

   IF VALID-HANDLE(wh-doca) THEN
      ASSIGN wh-doca:FORMAT = "999/999".

   FIND mensagem WHERE
        ROWID(mensagem) = p-row-table NO-LOCK NO-ERROR.
   /*
   IF AVAIL mensagem THEN 
      ASSIGN wh-tg-dev:SCREEN-VALUE = STRING(mensagem.log-1)
             wh-tg-pcp:SCREEN-VALUE = STRING(mensagem.log-2)
             wh-doca:SCREEN-VALUE = SUBSTR(mensagem.char-2,100,6).
   */

   FIND mensagem-ext OF mensagem SHARE-LOCK NO-ERROR.
   IF AVAIL mensagem-ext THEN DO.
      CASE mensagem-ext.utilizacao.
          WHEN 1 THEN ASSIGN wh-tg-pcp:SCREEN-VALUE = 'YES'.
          WHEN 2 THEN ASSIGN wh-tg-rpr:SCREEN-VALUE = 'YES'.
          WHEN 3 THEN ASSIGN wh-tg-dev:SCREEN-VALUE = 'YES'.
      END CASE.
      ASSIGN wh-doca:SCREEN-VALUE = mensagem-ext.doca.
   END.
END.

IF p-ind-event = "VALIDATE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01ad176.w" THEN DO: 

   IF wh-doca:SCREEN-VALUE <> "" THEN DO.
      ASSIGN wh-doca:FORMAT = "x(6)".
      FIND ob-localiz WHERE
           ob-localiz.cod-localiz = wh-doca:SCREEN-VALUE NO-LOCK NO-ERROR.
    
      IF NOT AVAIL ob-localiz THEN DO.
         MESSAGE 'Doca n∆o Cadastrada...'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO wh-doca.
         RETURN "NOK".
      END.
   END.
END.

IF p-ind-event = "ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01ad176.w" THEN DO:
   FIND mensagem WHERE
        ROWID(mensagem) = p-row-table NO-LOCK NO-ERROR.
   IF AVAIL mensagem THEN DO.
      FIND mensagem-ext OF mensagem SHARE-LOCK NO-ERROR.
      IF NOT AVAIL mensagem-ext THEN DO.
         CREATE mensagem-ext.
         ASSIGN mensagem-ext.cod-mensagem = mensagem.cod-mensagem.
      END.
      ASSIGN mensagem-ext.utilizacao = IF LOGICAL(wh-tg-pcp:SCREEN-VALUE) 
                                       THEN 1 
                                       ELSE IF LOGICAL(wh-tg-rpr:SCREEN-VALUE)
                                            THEN 2
                                            ELSE 3
             mensagem-ext.doca = wh-doca:SCREEN-VALUE.
      
      ASSIGN mensagem.log-1 = LOGICAL(wh-tg-dev:SCREEN-VALUE)
             mensagem.log-2 = LOGICAL(wh-tg-pcp:SCREEN-VALUE)
             SUBSTR(mensagem.char-2,100,6) = wh-doca:SCREEN-VALUE.
   END.
END.

