/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.
 
/* Variable Definitions *****************************************************/
DEF NEW GLOBAL SHARED VAR c-aprov-canc AS CHAR NO-UNDO.

DEF VAR h-cb-aprov-canc   AS HANDLE NO-UNDO.
DEF VAR h-tx-cb-aprov     AS HANDLE.
DEF VAR h-data            AS HANDLE.
DEF VAR h-objeto          AS HANDLE.
DEF VAR c-objeto          AS CHAR NO-UNDO.
DEF VAR c-lst-ger         AS CHAR.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "AFTER-INITIALIZE" THEN DO. 
   ASSIGN c-aprov-canc = ''.

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = 'da-data' THEN 
         ASSIGN h-data = h-objeto.

      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.

   FOR EACH cm-ext-repres WHERE
            cm-ext-repres.classe <= 2 NO-LOCK.
       FIND repres WHERE
            repres.cod-rep = cm-ext-repres.cod-rep NO-LOCK NO-ERROR.

       IF cm-ext-repres.bloqueado THEN NEXT.
       
       ASSIGN c-lst-ger = IF c-lst-ger = ''
                          THEN repres.nome-abrev
                          ELSE c-lst-ger + ',' + repres.nome-abrev.
   END.
   ASSIGN c-lst-ger = "," + c-lst-ger.


   CREATE COMBO-BOX h-cb-aprov-canc
        ASSIGN FRAME      = p-wgh-frame
               DATA-TYPE  = "CHARACTER"
               FORMAT     = "x(20)"
               LIST-ITEMS = c-lst-ger
               ROW        = 4.1
               COLUMN     = 41
               WIDTH      = 15
               VISIBLE    = YES
               SENSITIVE  = YES
               HELP       = "Informe o Gerente que Autorizou"
               TOOLTIP    = "Informe o Gerente que Autorizou"
               TRIGGERS:
                    ON "VALUE-CHANGED":U PERSISTENT RUN esepc/epc-pd4000a-v1.p.
              END TRIGGERS.

   CREATE TEXT h-tx-cb-aprov
        ASSIGN FRAME        = p-wgh-frame
               FORMAT       = "x(10)"
               WIDTH        = 7.5
               ROW          = 4.1
               COLUMN       = 33
               HEIGHT       = 0.88
               SCREEN-VALUE = "Aprovador:"
               VISIBLE      = YES.

   h-cb-aprov-canc:MOVE-AFTER-TAB(h-data).
END.

