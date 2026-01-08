/* Programa: upc-cd0402.p
** Objetivo: Dar manuten‡Æo nos Dados complementares das Transportadoras, 
**           (transporte-ext) referentes …s customiza‡äes da Tear Tˆxtil Ind.Com.Ltda.
** Autor...: Prodb - Toninho  Dezembro/2004
*/

/* Parameter Definitions ****************************************************/
define input parameter p-ind-event  as character.
define input parameter p-ind-object as character.
define input parameter p-wgh-object as handle.
define input parameter p-wgh-frame  as widget-handle.
define input parameter p-cod-table  as character.
define input parameter p-row-table  as rowid.

/* Global Variable Definitions **********************************************/

/* Variavies criadas na viewer dinamicamente*********************************/
DEF NEW GLOBAL SHARED VAR wh-frete-minimo AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-frete-minimo AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-perc-frete   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-perc-frete   AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bloqueado    AS WIDGET-HANDLE NO-UNDO. 
DEF NEW GLOBAL SHARED VAR tx-bloqueado    AS HANDLE NO-UNDO.        

/* Global Variable Definitions for CEPONLINE *******************************/
DEFINE NEW GLOBAL SHARED VAR h-cep          AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-endereco     AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-bairro       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-cidade       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-estado       AS HANDLE.


/* Variable Definitions *****************************************************/
DEF VAR c-objeto AS CHAR NO-UNDO.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "BEFORE-INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v06ad268.w" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
       IF h-objeto:TYPE <> "field-group" THEN DO:
          IF h-objeto:NAME = "endereco" THEN DO.
             ASSIGN h-endereco = h-objeto.
             /*ON 'entry':U OF h-objeto PERSISTENT RUN esupc/upc-cd0402b.p.*/
          END.

          IF h-objeto:NAME = "bairro" THEN
             ASSIGN h-bairro = h-objeto.

          IF h-objeto:NAME = "cidade" THEN
             ASSIGN h-cidade = h-objeto.

          IF h-objeto:NAME = "estado" THEN
             ASSIGN h-estado = h-objeto.

          IF h-objeto:NAME = "cep" THEN DO.
             ASSIGN h-cep = h-objeto.
             ON 'leave':U OF h-objeto PERSISTENT RUN esupc/upc-cd0402a.p.
          END.
          ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
       END.
       ELSE 
          ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

IF p-ind-event = "BEFORE-INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05ad268.w" THEN DO:

   CREATE TEXT tx-frete-minimo
          ASSIGN frame        = p-wgh-frame
                 format       = "x(17)"
                 width        = 14
                 screen-value = "Frete Minimo:"
                 ROW          = 8.15
                 COL          = 20.7
                 VISIBLE      = YES.
   CREATE FILL-IN wh-frete-minimo
          ASSIGN frame              = p-wgh-frame
                 side-label-handle  = tx-frete-minimo:HANDLE 
                 DATA-TYPE          = "DECIMAL" 
                 format             = ">>>,>>>,>>9.99<<" 
                 width              = 16
                 height             = 0.88
                 row                = 8
                 col                = 30.2  
                 label              = "Frete Minimo:" 
                 visible            = YES
                 sensitive          = no.

   CREATE TEXT tx-perc-frete
          ASSIGN FRAME        = p-wgh-frame
                 format       = "x(17)"
                 WIDTH        = 14
                 SCREEN-VALUE = "Perc Frete:"
                 ROW          = 9.15
                 COL          = 22.3
                 VISIBLE      = YES.
   CREATE FILL-IN wh-perc-frete
          ASSIGN frame              = p-wgh-frame
                 side-label-handle  = tx-perc-frete:HANDLE  
                 DATA-TYPE          = "DECIMAL" 
                 FORMAT             = ">>9.999<<" 
                 WIDTH              = 9
                 HEIGHT             = 0.88
                 ROW                = 9
                 COL                = 30.2
                 LABEL              = "Perc Frete:" 
                 VISIBLE            = YES
                 SENSITIVE          = no.
   CREATE TEXT tx-bloqueado
          ASSIGN FRAME        = p-wgh-frame
                 format       = "x(15)"
                 WIDTH        = 14
                 SCREEN-VALUE = "Bloqueado:"
                 ROW          = 9.15
                 COL          = 42.3
                 VISIBLE      = YES.
   CREATE FILL-IN wh-bloqueado
                 ASSIGN frame       = p-wgh-frame
                 side-label-handle  = tx-bloqueado:HANDLE  
                 DATA-TYPE          = "LOGICAL" 
                 FORMAT             = "Sim/NÆo" 
                 WIDTH              = 4
                 HEIGHT             = 0.88
                 ROW                = 9
                 COL                = 50.2
                 TOOLTIP            = "Transportador bloqueado para uso do sistema."
                 LABEL              = "Bloqueado:" 
                 VISIBLE            = YES
                 SENSITIVE          = no.
END.

IF p-ind-event = "ENABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05ad268.w" THEN DO:
   ASSIGN wh-frete-minimo:SENSITIVE = YES
          wh-perc-frete:SENSITIVE = YES
          wh-bloqueado:SENSITIVE = YES.
END.

IF p-ind-event = "DISABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05ad268.w" THEN DO:
   ASSIGN wh-frete-minimo:SENSITIVE = NO
          wh-perc-frete:SENSITIVE = NO
          wh-bloqueado:SENSITIVE = NO.
END.

IF p-ind-event = "ADD" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05ad268.w" THEN DO:
   ASSIGN wh-frete-minimo:LABEL = "Frete M¡nimo:"
          wh-frete-minimo:SCREEN-VALUE = '0'
          wh-perc-frete:LABEL = "Perc Frete:"
          wh-perc-frete:SCREEN-VALUE = '0'
          wh-bloqueado:LABEL = "Bloqueado:"
          wh-bloqueado:SCREEN-VALUE = 'NÆo'.
END.

IF p-ind-event = "DISPLAY" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05ad268.w" THEN DO:

   FIND transporte WHERE
        ROWID(transporte) = p-row-table NO-LOCK NO-ERROR.

   FIND transporte-ext WHERE
        transporte-ext.cod-transp = transporte.cod-transp NO-LOCK NO-ERROR.

   IF AVAIL transporte-ext THEN
      ASSIGN wh-frete-minimo:SCREEN-VALUE = STRING(transporte-ext.frete-minimo)
             wh-perc-frete:SCREEN-VALUE = STRING(transporte-ext.perc-frete)
             wh-bloqueado:SCREEN-VALUE = STRING(transporte-ext.bloqueado).
   ELSE DO.
       ASSIGN wh-frete-minimo:SCREEN-VALUE = '0'
              wh-perc-frete:SCREEN-VALUE = '0'
              wh-bloqueado:SCREEN-VALUE = 'NÆo'.
   END.
END.

IF p-ind-event = "ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05ad268.w" THEN DO:

   FIND transporte WHERE
        ROWID(transporte) = p-row-table NO-LOCK NO-ERROR.

   FIND transporte-ext WHERE
        transporte-ext.cod-transp = transporte.cod-transp NO-ERROR.

   IF NOT AVAIL transporte-ext THEN DO.
      CREATE transporte-ext.
      ASSIGN transporte-ext.cod-transp  = transporte.cod-transp
             transporte-ext.nome-transp = transporte.nome-abrev.
   END.
   ASSIGN transporte-ext.frete-minimo = DECIMAL(wh-frete-minimo:SCREEN-VALUE)
          transporte-ext.perc-frete   = DECIMAL(wh-perc-frete:SCREEN-VALUE)
          transporte-ext.bloqueado    = IF wh-bloqueado:SCREEN-VALUE = 'Sim' THEN YES ELSE NO.
END.

