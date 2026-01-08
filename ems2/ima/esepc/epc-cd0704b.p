/******************************************************************************
** Programa: epc-cd0704b.p                                                   **
** Objetivo:                                                                 **
** Autor...:                                                                 **
** Observ..:                                                                 **
**                                                                           **
******************************************************************************/
 
/* Parameter Definitions *****************************************************/
DEFINE INPUT PARAMETER p-ind-event         AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object        AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object        AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame         AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table         AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table         AS ROWID.

DEF NEW GLOBAL SHARED VAR h-nome           AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-fi-area AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-cb-area        AS HANDLE.

DEF NEW GLOBAL SHARED VAR h-tx-cb-area     AS HANDLE.

DEF NEW GLOBAL SHARED VAR h-tx-celular1    AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-tx-celular2    AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-tx-celular3    AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-fi-celular1    AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-fi-celular2    AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-fi-celular3    AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-tg-messenger1  AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-tg-messenger2  AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-tg-messenger3  AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-cb-aplicativo1 AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-cb-aplicativo2 AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-cb-aplicativo3 AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-tx-aplicativo1 AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-tx-aplicativo2 AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-tx-aplicativo3 AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-rect           AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-tx-rect        AS HANDLE.

DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
DEF VAR c-objeto AS CHAR NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

FIND im-param WHERE
     im-param.cod-param = "APLICATIVOS_MESSENGER_CELULAR" NO-LOCK NO-ERROR.

IF p-ind-event  = "INITIALIZE"   AND 
   p-ind-object = "VIEWER"              AND 
   c-objeto     = "'advwr\v07ad043.w'"  THEN DO:

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = 'area' THEN DO.
         ASSIGN h-fi-area = h-objeto.
         ASSIGN h-fi-area:HIDDEN = YES.
      END.

      IF h-objeto:NAME = 'nome' THEN 
         ASSIGN h-nome = h-objeto.

      IF h-objeto:NAME = 'observacao' THEN 
         ASSIGN h-objeto:WIDTH = 41.

      IF h-objeto:NAME = 'e-mail' THEN 
         ASSIGN h-objeto:WIDTH = 41.

      IF h-objeto:NAME = 'telefone' THEN 
         ASSIGN h-objeto:LABEL  = "Telefone FIXO:".

      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.

   
   CREATE COMBO-BOX h-cb-area
       ASSIGN FRAME      = p-wgh-frame
              DATA-TYPE  = "CHARACTER"
              FORMAT     = "x(35)"
              LIST-ITEMS = "Financeiro,Fiscal,Comercial,Outros"
              WIDTH      = 19
              ROW        = h-fi-area:ROW
              COL        = h-fi-area:COLUMN
              VISIBLE    = YES
              SENSITIVE  = NO
              TRIGGERS:
                  ON 'VALUE-CHANGED' PERSISTENT RUN esepc/epc-cd0704b-v3.p.
              END TRIGGERS.
   CREATE TEXT h-tx-cb-area
       ASSIGN FRAME        = p-wgh-frame
              FORMAT       = "x(5)"
              WIDTH        = 4
              ROW          = h-fi-area:ROW
              COLUMN       = h-fi-area:COLUMN - 4
              HEIGHT       = 0.88
              SCREEN-VALUE = "µrea:"
              VISIBLE      = YES.


   CREATE RECTANGLE h-rect
       ASSIGN FRAME        = p-wgh-frame
              WIDTH        = 23
              HEIGHT       = 9.4
              ROW          = 5
              COL          = 65
              VISIBLE      = YES
              EDGE-PIXELS  = 2
              GRAPHIC-EDGE = YES.
   CREATE TEXT h-tx-rect
       ASSIGN FRAME        = p-wgh-frame
              FORMAT       = "x(9)"
              WIDTH        = 5
              ROW          = 4.8
              COLUMN       = 66.3
              SCREEN-VALUE = "Celular"
              VISIBLE      = YES .


   CREATE TEXT h-tx-celular1
       ASSIGN FRAME        = p-wgh-frame
              FORMAT       = "x(4)"
              WIDTH        = 3
              ROW          = 5.7
              COLUMN       = 66.3
              SCREEN-VALUE = "Tel:"
              VISIBLE      = YES.
   CREATE FILL-IN h-fi-celular1
       ASSIGN FRAME             = p-wgh-frame
              NAME              = "celular1"
              SIDE-LABEL-HANDLE = h-tx-celular1
              DATA-TYPE         = "CHARACTER"
              FORMAT            = "x(15)"
              WIDTH             = 17.3
              ROW               = 5.5
              COL               = 69.4
              HEIGHT            = 0.88
              TOOLTIP           = "Numero do Celular"
              VISIBLE           = YES 
              SENSITIVE         = NO
              TRIGGERS:
                  ON 'VALUE-CHANGED' PERSISTENT RUN esepc/epc-cd0704b-v1.p.
              END TRIGGERS.
   CREATE TOGGLE-BOX h-tg-messenger1
         ASSIGN FRAME        = p-wgh-frame
                NAME         = "messenger1"
                LABEL        = 'Messenger' 
                WIDTH        = 15
                ROW          = 6.4
                COLUMN       = 69.2
                HEIGHT       = 0.88
                VISIBLE      = YES
                TOOLTIP      = "Indica se existe Messenger nesse Celular"
                TRIGGERS:
                    ON 'VALUE-CHANGED' PERSISTENT RUN esepc/epc-cd0704b-v2.p.
                END TRIGGERS.
   CREATE TEXT h-tx-aplicativo1
        ASSIGN FRAME        = p-wgh-frame
               FORMAT       = "x(4)"
               WIDTH        = 3
               ROW          = 7.3
               COLUMN       = 69.2
               SCREEN-VALUE = "App:"
               VISIBLE      = YES.
   CREATE COMBO-BOX h-cb-aplicativo1
        ASSIGN FRAME      = p-wgh-frame
               DATA-TYPE  = "CHARACTER"
               FORMAT     = "x(35)"
               LIST-ITEMS = " ," + im-param.val-param
               WIDTH      = 14
               ROW        = 7.2
               COL        = 72.5
               VISIBLE    = YES
               SENSITIVE  = NO
               TOOLTIP      = "Aplicativo Utilizado no Messenger (bate-papo)".


   CREATE TEXT h-tx-celular2
       ASSIGN FRAME        = p-wgh-frame
              FORMAT       = "x(4)"
              WIDTH        = 3
              ROW          = h-tx-celular1:ROW + 3
              COLUMN       = 66.3
              SCREEN-VALUE = "Tel:"
              VISIBLE      = YES .
   CREATE FILL-IN h-fi-celular2
       ASSIGN FRAME             = p-wgh-frame
              NAME              = "celular2"
              SIDE-LABEL-HANDLE = h-tx-celular2
              DATA-TYPE         = "CHARACTER"
              FORMAT            = "x(15)"
              WIDTH             = 17.3
              ROW               = h-fi-celular1:ROW + 3
              COL               = 69.4
              HEIGHT            = 0.88
              TOOLTIP           = "Numero do Celular"
              VISIBLE           = YES 
              SENSITIVE         = NO
              TRIGGERS:
                  ON 'VALUE-CHANGED' PERSISTENT RUN esepc/epc-cd0704b-v1.p.
              END TRIGGERS.
   CREATE TOGGLE-BOX h-tg-messenger2
          ASSIGN FRAME        = p-wgh-frame
                 NAME         = "messenger2"
                 LABEL        = 'Messenger' 
                 WIDTH        = 15
                 ROW          = h-tg-messenger1:ROW + 3
                 COLUMN       = 69.2
                 HEIGHT       = 0.88
                 VISIBLE      = YES
                 TOOLTIP      = "Indica se existe Messenger nesse Celular"
                 TRIGGERS:
                     ON 'VALUE-CHANGED' PERSISTENT RUN esepc/epc-cd0704b-v2.p.
                 END TRIGGERS.
   CREATE TEXT h-tx-aplicativo2
       ASSIGN FRAME        = p-wgh-frame
              FORMAT       = "x(4)"
              WIDTH        = 3
              ROW          = h-tx-aplicativo1:ROW + 3
              COLUMN       = 69.2
              SCREEN-VALUE = "App:"
              VISIBLE      = YES.
   CREATE COMBO-BOX h-cb-aplicativo2
       ASSIGN FRAME      = p-wgh-frame
              DATA-TYPE  = "CHARACTER"
              FORMAT     = "x(35)"
              LIST-ITEMS = " ," + im-param.val-param
              WIDTH      = 14
              ROW        = h-cb-aplicativo1:ROW + 3
              COL        = 72.5
              VISIBLE    = YES
              SENSITIVE  = NO
              TOOLTIP      = "Aplicativo Utilizado no Messenger (bate-papo)".


   CREATE TEXT h-tx-celular3
         ASSIGN FRAME        = p-wgh-frame
                FORMAT       = "x(4)"
                WIDTH        = 3
                ROW          = h-tx-celular2:ROW + 3
                COLUMN       = 66.3
                SCREEN-VALUE = "Tel:"
                VISIBLE      = YES .
   CREATE FILL-IN h-fi-celular3
         ASSIGN FRAME             = p-wgh-frame
                NAME              = "celular3"
                SIDE-LABEL-HANDLE = h-tx-celular3
                DATA-TYPE         = "CHARACTER"
                FORMAT            = "x(15)"
                WIDTH             = 17.3
                ROW               = h-fi-celular2:ROW + 3
                COL               = 69.4
                HEIGHT            = 0.88
                TOOLTIP           = "Numero do Celular"
                VISIBLE           = YES 
                SENSITIVE         = NO
                TRIGGERS:
                    ON 'VALUE-CHANGED' PERSISTENT RUN esepc/epc-cd0704b-v1.p.
                END TRIGGERS.
   CREATE TOGGLE-BOX h-tg-messenger3
          ASSIGN FRAME        = p-wgh-frame
                 NAME         = "messenger3"
                 LABEL        = 'Messenger' 
                 WIDTH        = 15
                 ROW          = h-tg-messenger2:ROW + 3
                 COLUMN       = 69.2
                 HEIGHT       = 0.88
                 VISIBLE      = YES
                 TOOLTIP      = "Indica se existe Messenger nesse Celular"
                 TRIGGERS:
                     ON 'VALUE-CHANGED' PERSISTENT RUN esepc/epc-cd0704b-v2.p.
                 END TRIGGERS.
   CREATE TEXT h-tx-aplicativo3
      ASSIGN FRAME        = p-wgh-frame
             FORMAT       = "x(4)"
             WIDTH        = 3
             ROW          = h-tx-aplicativo2:ROW + 3
             COLUMN       = 69.2
             SCREEN-VALUE = "App:"
             VISIBLE      = YES.
   CREATE COMBO-BOX h-cb-aplicativo3
      ASSIGN FRAME      = p-wgh-frame
             DATA-TYPE  = "CHARACTER"
             FORMAT     = "x(35)"
             LIST-ITEMS = " ," + im-param.val-param
             WIDTH      = 14
             ROW        = h-cb-aplicativo2:ROW + 3
             COL        = 72.5
             VISIBLE    = YES
             SENSITIVE  = NO
             TOOLTIP      = "Aplicativo Utilizado no Messenger (bate-papo)".

   h-cb-area:MOVE-AFTER-TAB(h-nome).

END.

IF p-ind-event  = 'ADD' AND
   p-ind-object = "VIEWER" AND 
   c-objeto     = "'advwr\v07ad043.w'"  THEN DO:

   ASSIGN h-cb-area:SCREEN-VALUE = "Financeiro"
          h-nome:SCREEN-VALUE = "Financeiro".

    CREATE TEXT h-tx-cb-area
        ASSIGN FRAME        = p-wgh-frame
               FORMAT       = "x(5)"
               WIDTH        = 4
               ROW          = h-fi-area:ROW
               COLUMN       = h-fi-area:COLUMN - 4
               HEIGHT       = 0.88
               SCREEN-VALUE = "µrea:"
               VISIBLE      = YES.

   CREATE TEXT h-tx-rect
       ASSIGN FRAME        = p-wgh-frame
              FORMAT       = "x(9)"
              WIDTH        = 5
              ROW          = 4.8
              COLUMN       = 66.3
              SCREEN-VALUE = "Celular"
              VISIBLE      = YES .

   CREATE TEXT h-tx-celular1
       ASSIGN FRAME        = p-wgh-frame
              FORMAT       = "x(4)"
              WIDTH        = 3
              ROW          = 5.7
              COLUMN       = 66.3
              SCREEN-VALUE = "Tel:"
              VISIBLE      = YES .
   CREATE TEXT h-tx-aplicativo1
       ASSIGN FRAME        = p-wgh-frame
              FORMAT       = "x(4)"
              WIDTH        = 3
              ROW          = 7.3
              COLUMN       = 69.2
              SCREEN-VALUE = "App:"
              VISIBLE      = YES.

   CREATE TEXT h-tx-celular2
       ASSIGN FRAME        = p-wgh-frame
              FORMAT       = "x(4)"
              WIDTH        = 3
              ROW          = h-fi-celular1:ROW + 3.2
              COLUMN       = 66.3
              SCREEN-VALUE = "Tel:"
              VISIBLE      = YES .
   CREATE TEXT h-tx-aplicativo2
       ASSIGN FRAME        = p-wgh-frame
              FORMAT       = "x(4)"
              WIDTH        = 3
              ROW          = h-tx-aplicativo1:ROW + 3
              COLUMN       = 69.2
              SCREEN-VALUE = "App:"
              VISIBLE      = YES.

   CREATE TEXT h-tx-celular3
       ASSIGN FRAME        = p-wgh-frame
              FORMAT       = "x(4)"
              WIDTH        = 3
              ROW          = h-fi-celular2:ROW + 3.2
              COLUMN       = 66.3
              SCREEN-VALUE = "Tel:"
              VISIBLE      = YES .
   CREATE TEXT h-tx-aplicativo3
       ASSIGN FRAME        = p-wgh-frame
              FORMAT       = "x(4)"
              WIDTH        = 3
              ROW          = h-tx-aplicativo2:ROW + 3
              COLUMN       = 69.2
              SCREEN-VALUE = "App:"
              VISIBLE      = YES.
END.

IF p-ind-event  = 'DISPLAY' AND
   p-ind-object = "VIEWER" AND 
   c-objeto     = "'advwr\v07ad043.w'"  THEN DO:

   FIND cont-emit WHERE
        ROWID(cont-emit) = p-row-table NO-LOCK NO-ERROR.

   IF VALID-HANDLE(h-cb-area) THEN DO.
      ASSIGN h-cb-area:SCREEN-VALUE = 'Outros'.
      IF AVAIL cont-emit THEN
         ASSIGN h-cb-area:SCREEN-VALUE = cont-emit.area.
   END.

   IF VALID-HANDLE(h-fi-celular1) THEN DO.
      ASSIGN h-fi-celular1:SCREEN-VALUE = ""
             h-fi-celular2:SCREEN-VALUE = ""
             h-fi-celular3:SCREEN-VALUE = ""
             h-tg-messenger1:SCREEN-VALUE = "NO"
             h-tg-messenger2:SCREEN-VALUE = "NO"
             h-tg-messenger3:SCREEN-VALUE = "NO"
             h-cb-aplicativo1:SCREEN-VALUE = " "
             h-cb-aplicativo2:SCREEN-VALUE = " "
             h-cb-aplicativo3:SCREEN-VALUE = " ".

      FIND ext-cont-emit WHERE
           ext-cont-emit.cod-emitente = cont-emit.cod-emitente AND
           ext-cont-emit.sequencia = cont-emit.sequencia
           NO-LOCK NO-ERROR.
      IF AVAIL ext-cont-emit THEN DO.
         ASSIGN h-fi-celular1:SCREEN-VALUE = ext-cont-emit.celular1
                h-fi-celular2:SCREEN-VALUE = ext-cont-emit.celular2
                h-fi-celular3:SCREEN-VALUE = ext-cont-emit.celular3
                h-tg-messenger1:SCREEN-VALUE = STRING(ext-cont-emit.messenger1)
                h-tg-messenger2:SCREEN-VALUE = STRING(ext-cont-emit.messenger2)
                h-tg-messenger3:SCREEN-VALUE = STRING(ext-cont-emit.messenger3)
                h-cb-aplicativo1:SCREEN-VALUE = ext-cont-emit.aplicativo1
                h-cb-aplicativo2:SCREEN-VALUE = ext-cont-emit.aplicativo2
                h-cb-aplicativo3:SCREEN-VALUE = ext-cont-emit.aplicativo3.
      END.
   END.
END.

IF p-ind-event  = 'ENABLE' AND
   p-ind-object = "VIEWER" AND 
   c-objeto     = "'advwr\v07ad043.w'"  THEN DO:

   ASSIGN h-cb-area:SENSITIVE = YES.

   ASSIGN h-fi-celular1:SENSITIVE = YES.
   ASSIGN h-tg-messenger1:SENSITIVE = h-fi-celular1:SCREEN-VALUE <> ''.

   ASSIGN h-fi-celular2:SENSITIVE = YES.
   ASSIGN h-tg-messenger2:SENSITIVE = h-fi-celular2:SCREEN-VALUE <> ''.

   ASSIGN h-fi-celular3:SENSITIVE = YES.
   ASSIGN h-tg-messenger3:SENSITIVE = h-fi-celular3:SCREEN-VALUE <> ''.
END.

IF p-ind-event  = 'DISABLE' AND
   p-ind-object = "VIEWER" AND 
   c-objeto     = "'advwr\v07ad043.w'"  THEN DO:

   ASSIGN h-cb-area:SENSITIVE = NO.

   ASSIGN h-fi-celular1:SENSITIVE = NO.
   ASSIGN h-tg-messenger1:SENSITIVE = NO.
   ASSIGN h-fi-celular2:SENSITIVE = NO.
   ASSIGN h-tg-messenger2:SENSITIVE = NO.
   ASSIGN h-fi-celular3:SENSITIVE = NO.
   ASSIGN h-tg-messenger3:SENSITIVE = NO.
END.


IF p-ind-event  = 'ASSIGN' AND
   p-ind-object = "VIEWER" AND 
   c-objeto     = "'advwr\v07ad043.w'"  THEN DO:

   FIND cont-emit WHERE
        ROWID(cont-emit) = p-row-table SHARE-LOCK NO-ERROR.

   IF AVAIL cont-emit THEN
      ASSIGN cont-emit.area = h-cb-area:SCREEN-VALUE.

   IF VALID-HANDLE(h-fi-celular1) THEN DO.
      FIND ext-cont-emit WHERE
           ext-cont-emit.cod-emitente = cont-emit.cod-emitente AND
           ext-cont-emit.sequencia = cont-emit.sequencia
           SHARE-LOCK NO-ERROR.
      IF NOT AVAIL ext-cont-emit THEN DO.
         CREATE ext-cont-emit.
         ASSIGN ext-cont-emit.cod-emitente = cont-emit.cod-emitente
                ext-cont-emit.sequencia = cont-emit.sequencia.
      END.
      ASSIGN ext-cont-emit.celular1    = h-fi-celular1:SCREEN-VALUE
             ext-cont-emit.celular2    = h-fi-celular2:SCREEN-VALUE
             ext-cont-emit.celular3    = h-fi-celular3:SCREEN-VALUE
             ext-cont-emit.messenger1  = LOGICAL(h-tg-messenger1:SCREEN-VALUE)
             ext-cont-emit.messenger2  = LOGICAL(h-tg-messenger2:SCREEN-VALUE)
             ext-cont-emit.messenger3  = LOGICAL(h-tg-messenger3:SCREEN-VALUE)
             ext-cont-emit.aplicativo1 = h-cb-aplicativo1:SCREEN-VALUE
             ext-cont-emit.aplicativo2 = h-cb-aplicativo2:SCREEN-VALUE.
             ext-cont-emit.aplicativo3 = h-cb-aplicativo3:SCREEN-VALUE.
   END.
END.

