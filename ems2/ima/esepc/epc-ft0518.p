/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

// Variaveis Globais
DEF NEW GLOBAL SHARED VAR h-cod-estabel AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-nr-nota-ini AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-nr-nota-fin AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-serie       AS WIDGET-HANDLE NO-UNDO.


// Objetos Criados Dinamicamente
DEF NEW GLOBAL SHARED VAR h-ed-justif AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-tx-justif AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-cb-envia-email AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-tx-envia-email AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-ed-dest-email  AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-tx-dest-email  AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-ed-copia-email AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-tx-copia-email AS HANDLE NO-UNDO.

// Variaveis Locais
DEF VAR h-win    AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-frame  AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.


/* Main Block ***************************************************************/
FIND FIRST nota-fiscal NO-LOCK NO-ERROR.
FIND FIRST para-fat NO-LOCK NO-ERROR.

FIND estabelec WHERE
     estabelec.cod-estabel = nota-fiscal.cod-estabel NO-LOCK NO-ERROR.

IF p-ind-event = "AFTER-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "btOK" THEN 
            ON 'MOUSE-SELECT-CLICK':U OF h-objeto PERSISTENT RUN esepc/epc-ft0518a.p (INPUT h-objeto).

         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

IF p-wgh-frame:NAME = "fPage0" THEN DO.
   ASSIGN h-win = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-frame = h-win:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-frame):
      IF h-frame:NAME = "fPage2" THEN DO.
         ASSIGN h-objeto = h-frame:FIRST-CHILD.
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
         DO WHILE VALID-HANDLE(h-objeto):
            IF h-objeto:NAME = "c-cod-estabel" THEN DO.
               ASSIGN h-cod-estabel = h-objeto.

               ASSIGN h-objeto:SCREEN-VALUE = estabelec.cod-estabel.
               //ASSIGN h-objeto:SENSITIVE = NO.
            END.

            IF h-objeto:NAME = "c-serie" THEN DO.
               ASSIGN h-serie = h-objeto.
               ASSIGN h-objeto:SCREEN-VALUE = para-fat.serie-pad.
            END.

            IF h-objeto:NAME  = "c-nr-nota-fis-ini" THEN 
               ASSIGN h-nr-nota-ini = h-objeto.

            IF h-objeto:NAME  = "c-nr-nota-fis-fim" THEN DO.
               ASSIGN h-nr-nota-fin = h-objeto.
               ON 'ENTRY':U OF h-objeto PERSISTENT RUN esepc/epc-ft0518s1.p (INPUT h-objeto). 
            END.
            
            IF h-objeto:NAME  = "rs-imprime" THEN 
               ON 'VALUE-CHANGED':U OF h-objeto PERSISTENT RUN esepc/epc-ft0518v1.p (INPUT h-objeto). 

            ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
         END.
         /*
         CREATE TEXT h-tx-justif
             ASSIGN FRAME        = h-frame
                    FORMAT       = "x(1)"
                    WIDTH        = 1
                    ROW          = 10.6
                    COLUMN       = 50
                    SCREEN-VALUE = "J"
                    VISIBLE      = YES.
         */
         CREATE EDITOR h-ed-justif
             ASSIGN FRAME     = h-frame
                    DATA-TYPE = "CHARACTER"
                    WIDTH     = 30
                    HEIGHT    = 1.7
                    ROW       = 10.6
                    COL       = 53.7
                    SCROLLBAR-VERTICAL = YES
                    VISIBLE   = YES
                    SENSITIVE = NO
                    TOOLTIP   = "Justificativa para ReImpress∆o da Nota".
                    
         CREATE TEXT h-tx-envia-email
             ASSIGN FRAME         = h-frame
                    FORMAT        = "x(15)"
                    WIDTH         = 15
                    SCREEN-VALUE  = "Envio de Email:"
                    ROW           = 6.9
                    COL           = 15.2
                    VISIBLE       = YES.
    
         CREATE COMBO-BOX h-cb-envia-email
             ASSIGN FRAME              = h-frame
                    FORMAT             = "x(15)"
                    SIDE-LABEL-HANDLE  = h-tx-envia-email:HANDLE 
                    LIST-ITEMS         = "Danfe,XML,Danfe e XML,Nenhum" 
                    WIDTH              = 15
                    ROW                = 6.8
                    COL                = 26.1
                    VISIBLE            = YES
                    SENSITIVE          = YES
                    TRIGGERS:
                        ON "VALUE-CHANGED":U PERSISTENT RUN esepc/epc-ft0518v2.p. 
                    END TRIGGERS.


         CREATE TEXT h-tx-dest-email
             ASSIGN FRAME        = h-frame
                    FORMAT       = "x(30)"
                    WIDTH        = 15
                    ROW          = 6.9
                    COLUMN       = 44.2
                    SCREEN-VALUE = "Destinat†rio:"
                    VISIBLE      = YES.
         
         CREATE EDITOR h-ed-dest-email
             ASSIGN FRAME     = h-frame
                    DATA-TYPE = "CHARACTER"
                    WIDTH     = 30
                    HEIGHT    = 2.5
                    ROW       = 6.8
                    COL       = 53.7
                    SCROLLBAR-VERTICAL = YES
                    VISIBLE   = YES
                    SENSITIVE = NO
                    TOOLTIP   = "Destinat†rio(s) do e-mail".

         CREATE TEXT h-tx-copia-email
             ASSIGN FRAME        = h-frame
                    FORMAT       = "x(7)"
                    WIDTH        = 7
                    ROW          = 9.5
                    COLUMN       = 48.7
                    SCREEN-VALUE = "C¢pia:"
                    VISIBLE      = YES.
         
         CREATE EDITOR h-ed-copia-email
             ASSIGN FRAME     = h-frame
                    DATA-TYPE = "CHARACTER"
                    WIDTH     = 30
                    HEIGHT    = 0.88
                    ROW       = 9.4
                    COL       = 53.7
                    SCROLLBAR-VERTICAL = NO
                    VISIBLE   = YES
                    SENSITIVE = NO
                    TOOLTIP   = "C¢pia(s) do e-mail".

         ASSIGN h-cb-envia-email:SCREEN-VALUE = 'Danfe e XML'.
         APPLY 'VALUE-CHANGED' TO h-cb-envia-email.
         h-cb-envia-email:MOVE-AFTER-TAB(h-nr-nota-fin).
      END.
      ASSIGN h-frame = h-frame:NEXT-SIBLING.
   END.
   APPLY 'VALUE-CHANGED' TO h-cb-envia-email.
   APPLY 'ENTRY' TO h-nr-nota-ini.
END.


