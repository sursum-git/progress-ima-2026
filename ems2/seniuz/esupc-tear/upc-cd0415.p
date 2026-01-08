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
DEF VAR l-todos        AS LOG.
DEF VAR l-cod-obsoleto AS LOG.
DEF VAR l-cod-fundo    AS LOG.
DEF VAR l-colecao      AS LOG.
DEF VAR l-cor          AS LOG.
DEF VAR i-cont         AS INT.

/* Variavies criadas na viewer dinamicamente*********************************/
DEF NEW GLOBAL SHARED VAR wh-cod-obsoleto AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-cod-obsoleto AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-colecao      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-colecao      AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cod-fundo    AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-cod-fundo    AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cor          AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-cor          AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-variante     AS WIDGET-HANDLE EXTENT 9 NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-variante     AS HANDLE NO-UNDO.

/* Variable Definitions *****************************************************/
DEF NEW GLOBAL SHARED VAR h-cod-refer AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR h-descricao AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR h-sub-prod AS WIDGET-HANDLE.
DEF NEW GLOBAL SHARED VAR l-copia AS LOG.

/* Variaveis para pegar o click no bot∆o de c¢pia */
DEFINE VARIABLE h-panel  AS HANDLE.
DEFINE VARIABLE h-button AS HANDLE.
DEFINE VARIABLE h-objeto AS HANDLE.

/* Vari†veis para pegar o nome da query */
DEFINE NEW GLOBAL SHARED VAR adm-broker-hdl AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-query AS HANDLE.
DEFINE VAR c-objects AS CHAR.
DEFINE VAR i-objects AS INT.
DEFINE VAR h-object  AS HANDLE.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "CONTAINER" THEN DO:
   RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                          INPUT "CONTAINER-TARGET":U,
                                          OUTPUT c-objects).

   DO i-objects = 1 TO NUM-ENTRIES(c-objects):
      ASSIGN h-object = WIDGET-HANDLE(ENTRY(i-objects, c-objects)).
       
      IF INDEX(h-object:PRIVATE-DATA, "qry") <> 0 THEN 
         ASSIGN h-query = h-object.
   END.
END.

IF p-ind-event = "BEFORE-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "Panel-Frame" THEN DO.
            ASSIGN h-panel = h-objeto:FIRST-CHILD. 
            DO WHILE VALID-HANDLE(h-panel).
               IF h-panel:TYPE = "field-group" THEN DO.
                  ASSIGN h-button = h-panel:FIRST-CHILD.
                  DO WHILE VALID-HANDLE(h-button).
                     IF h-button:NAME = 'bt-cop' THEN
                        ON 'mouse-select-click':U OF h-button PERSISTENT RUN esupc/upc-cd0415m1.p.  
                     ASSIGN h-button = h-button:NEXT-SIBLING.
                  END.
               END.
               ASSIGN h-panel = h-panel:NEXT-SIBLING.
            END.
         END.
    
         IF h-objeto:NAME = "cod-refer" THEN 
            ASSIGN h-cod-refer = h-objeto.

         IF h-objeto:NAME = "descricao" THEN 
            ASSIGN h-objeto:WIDTH = h-objeto:WIDTH + 1.
                   h-descricao = h-objeto.

         IF h-objeto:NAME = "c-desc-det" THEN 
            ASSIGN h-objeto:ROW = h-objeto:ROW + 0.5.

         IF h-objeto:NAME = "subproduto" THEN 
            ASSIGN h-sub-prod = h-objeto
                   h-objeto:WIDTH = 11
                   h-objeto:COLUMN = 77.

         IF h-objeto:NAME = "rect-1" THEN 
            ASSIGN h-objeto:HEIGHT = h-objeto:HEIGHT - 0.5
                   h-objeto:ROW = h-objeto:ROW + 0.5.

         IF h-objeto:NAME = "texto" THEN 
            ASSIGN h-objeto:HEIGHT = h-objeto:HEIGHT - 0.5
                   h-objeto:ROW = h-objeto:ROW + 0.5.

         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "VIEWER" THEN DO:

   CREATE TEXT tx-cod-obsoleto
          ASSIGN frame        = p-wgh-frame
                 format       = "x(14)"
                 width        = 14
                 screen-value = "Cod. Obsoleto:"
                 ROW          = 4.1
                 COL          = 23.5
                 VISIBLE      = YES.
   CREATE FILL-IN wh-cod-obsoleto
          ASSIGN frame              = p-wgh-frame
                 side-label-handle  = tx-cod-obsoleto:HANDLE 
                 DATA-TYPE          = "CHARACTER" 
                 format             = "x" 
                 width              = 3
                 height             = 0.88
                 row                = 3.9
                 col                = 34
                 label              = "Cod. Obsoleto:" 
                 visible            = YES
                 sensitive          = no.

   CREATE TEXT tx-cod-fundo
          ASSIGN FRAME        = p-wgh-frame
                 FORMAT       = "x(13)"
                 WIDTH        = 14
                 SCREEN-VALUE = "C¢d. Fundo:"
                 ROW          = 4.1
                 COL          = 53.2
                 VISIBLE      = YES.
   CREATE FILL-IN wh-cod-fundo
          ASSIGN frame              = p-wgh-frame
                 side-label-handle  = tx-cod-fundo:HANDLE  
                 DATA-TYPE          = "CHARACTER" 
                 FORMAT             = "x(4)" 
                 WIDTH              = 7.8
                 HEIGHT             = 0.88
                 ROW                = 3.9
                 COL                = 62
                 LABEL              = "C¢d. Fundo:" 
                 VISIBLE            = YES
                 SENSITIVE          = no.

   CREATE TEXT tx-colecao
          ASSIGN FRAME        = p-wgh-frame
                 FORMAT       = "x(9)"
                 WIDTH        = 9
                 SCREEN-VALUE = "Coleá∆o:"
                 ROW          = 5.1
                 COL          = 27.5
                 VISIBLE      = YES.      
   CREATE FILL-IN wh-colecao
          ASSIGN FRAME              = p-wgh-frame
                 SIDE-LABEL-HANDLE  = tx-colecao:HANDLE 
                 DATA-TYPE          = "CHARACTER" 
                 FORMAT             = "x(20)" 
                 WIDTH              = 18
                 HEIGHT             = 0.88
                 ROW                = 4.9
                 COL                = 34  
                 LABEL              = "Coleá∆o:" 
                 VISIBLE            = YES
                 SENSITIVE          = no.

   CREATE TEXT tx-cor
          ASSIGN frame        = p-wgh-frame
                 format       = "x(9)"
                 width        = 9
                 screen-value = "Cor:"
                 row          = 5.1
                 COL          = 58.7
                 visible      = YES.      
   CREATE FILL-IN wh-cor
          ASSIGN frame              = p-wgh-frame
                 side-label-handle  = tx-cor:handle 
                 DATA-TYPE          = "CHARACTER" 
                 format             = "x(20)" 
                 width              = 18
                 height             = 0.88
                 row                = 4.9
                 col                = 62  
                 label              = "Cor:" 
                 visible            = YES
                 sensitive          = no.

   CREATE TEXT tx-variante
          ASSIGN frame        = p-wgh-frame
                 format       = "x(10)"
                 width        = 11
                 screen-value = "Variantes:"
                 row          = 1.37
                 COL          = 54.7
                 visible      = YES.      
   CREATE FILL-IN wh-variante[1]
          ASSIGN frame              = p-wgh-frame
                 side-label-handle  = tx-variante:handle 
                 DATA-TYPE          = "INTEGER" 
                 format             = ">" 
                 width              = 2
                 height             = 0.88
                 row                = 1.25
                 col                = 62  
                 visible            = YES
                 sensitive          = no.
   CREATE FILL-IN wh-variante[2]
          ASSIGN frame              = p-wgh-frame
                 DATA-TYPE          = "INTEGER" 
                 format             = ">" 
                 width              = 2
                 height             = 0.88
                 row                = 1.25
                 col                = 65  
                 visible            = YES
                 sensitive          = no.
   CREATE FILL-IN wh-variante[3]
          ASSIGN FRAME              = p-wgh-frame
                 DATA-TYPE          = "INTEGER" 
                 FORMAT             = ">" 
                 WIDTH              = 2
                 HEIGHT             = 0.88
                 ROW                = 1.25
                 COL                = 68  
                 VISIBLE            = YES
                 SENSITIVE          = NO.
   CREATE FILL-IN wh-variante[4]
          ASSIGN frame              = p-wgh-frame
                 DATA-TYPE          = "INTEGER" 
                 format             = ">" 
                 width              = 2
                 height             = 0.88
                 row                = 1.25
                 col                = 71  
                 visible            = YES
                 sensitive          = no.
   CREATE FILL-IN wh-variante[5]
          ASSIGN frame              = p-wgh-frame
                 DATA-TYPE          = "INTEGER" 
                 format             = ">" 
                 width              = 2
                 height             = 0.88
                 row                = 1.25
                 col                = 74  
                 visible            = YES
                 sensitive          = no.
   CREATE FILL-IN wh-variante[6]
          ASSIGN frame              = p-wgh-frame
                 DATA-TYPE          = "INTEGER" 
                 format             = ">" 
                 width              = 2
                 height             = 0.88
                 row                = 1.25
                 col                = 77  
                 visible            = YES
                 sensitive          = no.
   CREATE FILL-IN wh-variante[7]
          ASSIGN frame              = p-wgh-frame
                 DATA-TYPE          = "INTEGER" 
                 format             = ">" 
                 width              = 2
                 height             = 0.88
                 row                = 1.25
                 col                = 80  
                 visible            = YES
                 sensitive          = no.
   CREATE FILL-IN wh-variante[8]
          ASSIGN frame              = p-wgh-frame
                 DATA-TYPE          = "INTEGER" 
                 format             = ">" 
                 width              = 2
                 height             = 0.88
                 row                = 1.25
                 col                = 83  
                 visible            = YES
                 sensitive          = no.
   CREATE FILL-IN wh-variante[9]
          ASSIGN frame              = p-wgh-frame
                 DATA-TYPE          = "INTEGER" 
                 format             = ">" 
                 width              = 2
                 height             = 0.88
                 row                = 1.25
                 col                = 86  
                 visible            = YES
                 sensitive          = no.


    h-descricao:MOVE-AFTER-TAB-ITEM(wh-variante[9]).
    h-sub-prod:MOVE-AFTER-TAB-ITEM(wh-cod-fundo).
    wh-variante[1]:MOVE-AFTER-TAB-ITEM(h-cod-refer).
    wh-variante[2]:MOVE-AFTER-TAB-ITEM(wh-variante[1]).
    wh-variante[3]:MOVE-AFTER-TAB-ITEM(wh-variante[2]).
    wh-variante[4]:MOVE-AFTER-TAB-ITEM(wh-variante[3]).
    wh-variante[5]:MOVE-AFTER-TAB-ITEM(wh-variante[4]).
    wh-variante[6]:MOVE-AFTER-TAB-ITEM(wh-variante[5]).
    wh-variante[7]:MOVE-AFTER-TAB-ITEM(wh-variante[6]).
    wh-variante[8]:MOVE-AFTER-TAB-ITEM(wh-variante[7]).
    wh-variante[9]:MOVE-AFTER-TAB-ITEM(wh-variante[8]).
END.

IF p-ind-event = "DISPLAY" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01in377.w" THEN DO:

   FIND referencia WHERE
        ROWID(referencia) = p-row-table NO-LOCK NO-ERROR.

   IF VALID-HANDLE(wh-cod-obsoleto) THEN DO.
      FIND referencia-ext WHERE
           referencia-ext.cod-refer = referencia.cod-refer NO-LOCK NO-ERROR.
    
      IF AVAIL referencia-ext THEN 
         ASSIGN wh-cod-obsoleto:SCREEN-VALUE = referencia-ext.cod-obsoleto
                wh-colecao:SCREEN-VALUE = referencia-ext.colecao 
                wh-cod-fundo:SCREEN-VALUE = referencia-ext.cod-fundo
                wh-cor:SCREEN-VALUE = referencia-ext.cor.
      ELSE
         ASSIGN wh-cod-obsoleto:SCREEN-VALUE = ""
                wh-colecao:SCREEN-VALUE = ""
                wh-cod-fundo:SCREEN-VALUE = ""
                wh-cor:SCREEN-VALUE = "".
   END.
END.

IF p-ind-event = "ENABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01in377.w" THEN DO:

   ASSIGN wh-cod-obsoleto:SENSITIVE = YES
          wh-colecao:SENSITIVE = YES
          wh-cod-fundo:SENSITIVE = YES
          wh-cor:SENSITIVE = YES.
END.

IF p-ind-event = "ADD" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01in377.w" THEN DO:

   ASSIGN wh-cod-obsoleto:LABEL = "Cod. Obsoleto:"
          wh-cod-fundo:LABEL = "C¢d. Fundo:"
          wh-cor:LABEL = "Cor:"
          wh-colecao:LABEL = "Coleá∆o:"
          wh-variante[1]:LABEL = "Variantes:".
END.

IF p-ind-event = "AFTER-ENABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01in377.w" THEN DO:
   APPLY 'entry' TO wh-cod-obsoleto.
   RETURN NO-APPLY.
END.

IF p-ind-event = "DISABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01in377.w" THEN DO:

   IF l-copia THEN DO.
      IF wh-variante[1]:INPUT-VALUE <> 0 THEN DO.
         CREATE referencia.
      END.
   END.
   ASSIGN wh-cod-obsoleto:SENSITIVE = NO
          wh-colecao:SENSITIVE = NO
          wh-cod-fundo:SENSITIVE = NO
          wh-cor:SENSITIVE = NO
          wh-variante[1]:SENSITIVE = NO
          wh-variante[2]:SENSITIVE = NO
          wh-variante[3]:SENSITIVE = NO
          wh-variante[4]:SENSITIVE = NO
          wh-variante[5]:SENSITIVE = NO
          wh-variante[6]:SENSITIVE = NO
          wh-variante[7]:SENSITIVE = NO
          wh-variante[8]:SENSITIVE = NO
          wh-variante[9]:SENSITIVE = NO.
END.

IF p-ind-event = "ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v01in377.w" THEN DO:

/*    IF wh-cod-obsoleto:SCREEN-VALUE < "0" OR                                */
/*       wh-cod-obsoleto:SCREEN-VALUE > "4" THEN DO.                          */
/*       MESSAGE "Codigo obsoleto deve estar entre 0 e 4" VIEW-AS ALERT-BOX.  */
/*       APPLY 'entry' TO wh-cod-obsoleto.                                    */
/*       RETURN 'NOK'.                                                        */
/*    END.                                                                    */

   FIND obsoleto WHERE obsoleto.codigo = wh-cod-obsoleto:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF NOT AVAIL obsoleto THEN DO:
      MESSAGE "Codigo obsoleto n∆o cadastrado." VIEW-AS ALERT-BOX.
      APPLY 'entry' TO wh-cod-obsoleto.
      RETURN 'NOK'.
   END.
   
   FIND fundo-estamp WHERE fundo-estamp.codigo = wh-cod-fundo:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF NOT AVAIL fundo-estamp THEN DO:
      MESSAGE "Codigo de fundo n∆o cadastrado." VIEW-AS ALERT-BOX.
      APPLY 'entry' TO wh-cod-fundo.
      RETURN 'NOK'.
   END.

   FIND referencia WHERE
        ROWID(referencia) = p-row-table NO-LOCK NO-ERROR.

   FIND referencia-ext WHERE
        referencia-ext.cod-refer = referencia.cod-refer NO-ERROR.

   IF NOT AVAIL referencia-ext THEN DO.
      CREATE referencia-ext.
      ASSIGN referencia-ext.cod-refer = referencia.cod-refer.
   END.
   
   IF wh-cod-obsoleto:SCREEN-VALUE <> referencia-ext.cod-obsoleto OR
      wh-cod-fundo:SCREEN-VALUE <> referencia-ext.cod-fundo OR
      wh-colecao:SCREEN-VALUE <> referencia-ext.colecao OR
      wh-cor:SCREEN-VALUE <> referencia-ext.cor THEN DO: /* Alterou */
      RUN esupc/upc-cd0415a.p (OUTPUT l-todos, OUTPUT l-cod-obsoleto,
                               OUTPUT l-cod-fundo, OUTPUT l-colecao, 
                               OUTPUT l-cor).
      IF l-todos = YES OR
         l-cod-obsoleto = YES OR
         l-cod-fundo = YES OR
         l-colecao = YES OR
         l-cor = YES THEN DO:
         FOR EACH ref-item-ext WHERE
                  ref-item-ext.cod-refer = referencia-ext.cod-refer.
             IF l-todos = YES THEN DO:
                ASSIGN /*ref-item-ext.cod-obsoleto = wh-cod-obsoleto:SCREEN-VALUE*/
                       ref-item-ext.cod-fundo = wh-cod-fundo:SCREEN-VALUE
                       ref-item-ext.colecao = wh-colecao:SCREEN-VALUE
                       ref-item-ext.cor = wh-cor:SCREEN-VALUE.
             END.
             ELSE DO:
                IF l-cod-obsoleto = YES THEN
                   ASSIGN ref-item-ext.cod-obsoleto = wh-cod-obsoleto:SCREEN-VALUE.
                IF l-cod-fundo = YES THEN
                   ASSIGN ref-item-ext.cod-fundo = wh-cod-fundo:SCREEN-VALUE.
                IF l-colecao THEN
                   ASSIGN ref-item-ext.colecao = wh-colecao:SCREEN-VALUE.
                IF l-cor = YES THEN
                   ASSIGN ref-item-ext.cor = wh-cor:SCREEN-VALUE.
             END.
         END.
      END.
   END.
   /*
   IF wh-cor:SCREEN-VALUE <> referencia-ext.cor THEN DO. /* Alterou a cor */
      FOR EACH ref-item-ext WHERE
               ref-item-ext.cod-refer = referencia-ext.cod-refer.
          ASSIGN ref-item-ext.cor = wh-cor:SCREEN-VALUE.
      END.
   END.
   */
   ASSIGN referencia-ext.cod-obsoleto = wh-cod-obsoleto:SCREEN-VALUE    
          referencia-ext.cod-fundo    = wh-cod-fundo:SCREEN-VALUE
          referencia-ext.colecao      = wh-colecao:SCREEN-VALUE   
          referencia-ext.cor          = wh-cor:SCREEN-VALUE. 
END.

