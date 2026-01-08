/* Programa: upc-pd4000.p
** Objetivo: Dar manutená∆o nos Dados complementares dos Pedidos de Venda, 
**           referentes Ös customizaá‰es da Tear Tàxtil Ind.Com.Ltda.
** Autor...: Prodb - Toninho  Maráo/2004
** Observ..: Existem Diversos programas que foram desenvolvidos como triggers
**           cada programa existe uma letra e um numero sequencial, onde a
**           letra identifica o tipo do evento (c=choose, v=value-changed
**           m=mouse-seleck-clik, e=entry etc..... 
*/

/* Parameter Definitions ****************************************************/
def input param p-ind-event      as char          no-undo.
def input param p-ind-object     as char          no-undo.
def input param p-wgh-object     as handle        no-undo.
def input param p-wgh-frame      as widget-handle no-undo.
def input param p-cod-table      as char          no-undo.
def input param p-row-table      as rowid         no-undo.

/* Variable Definitions *****************************************************/
DEF NEW GLOBAL SHARED VAR h-fPage2 AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fPage4 AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fPage6 AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fPage8 AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-fPage14 AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-cb-frame AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-nome-abrev AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-nr-pedcli AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-cod-priori AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-campo AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-btCompletOrder AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR l-preco-unico AS LOG.

DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

/* Variavies criadas na viewer dinamicamente*********************************/
DEF NEW GLOBAL SHARED VAR tx-acond AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-acond AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-qt-acumulada AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-qt-acumulada AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR tx-lote AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-lote   AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-btn-confirma AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR de-qtd-tot-ped LIKE ped-item.qt-pedida.
DEF BUFFER b-ped-venda FOR ped-venda.
DEF BUFFER b-ped-item FOR ped-item.    
DEF BUFFER b-ped-item-ext FOR ped-item-ext.

/* Main Block ***************************************************************/
DEF VAR c-objeto AS CHAR.

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

DEF NEW GLOBAL SHARED VAR h-menu AS HANDLE.
DEF VAR h-menu-item AS HANDLE.

IF p-ind-event = "BEFORE-INITIALIZE" THEN DO:

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.

   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = 'nome-abrev' THEN
            ASSIGN h-nome-abrev = h-objeto.

         IF h-objeto:NAME = 'nr-pedcli' THEN DO.
            ASSIGN h-nr-pedcli = h-objeto.
            ASSIGN h-nr-pedcli:LABEL = "PrÇ Nota"
                   h-nr-pedcli:WIDTH = 8.
            ON 'entry':U OF h-nr-pedcli PERSISTENT RUN esupc/upc-pd4000e7.p. 
         END.

         IF h-objeto:NAME = 'cb-frame' THEN
            ASSIGN h-cb-frame = h-objeto.

         IF h-objeto:NAME = 'fpage1' THEN DO.
            ASSIGN h-campo = h-objeto:FIRST-CHILD.
            ASSIGN h-campo = h-campo:FIRST-CHILD.
            DO WHILE VALID-HANDLE(h-campo):
               IF h-campo:NAME = 'btFastAddOrder' THEN DO.
                  ON 'mouse-select-click':U OF h-campo PERSISTENT RUN esupc/upc-pd4000m3.p (INPUT p-wgh-frame). 
               END.

               IF h-campo:NAME = 'btCompleteOrder' THEN DO.
                  ASSIGN h-btCompletOrder = h-campo.
                  ON 'mouse-select-click':U OF h-btCompletOrder PERSISTENT RUN esupc/upc-pd4000m1.p. 
               END.
               ASSIGN h-campo = h-campo:NEXT-SIBLING.
            END.
         END.

         IF h-objeto:NAME = "fpage2" THEN
            ASSIGN h-fpage2 = h-objeto.

         IF h-objeto:NAME = "fpage4" THEN
            ASSIGN h-fpage4 = h-objeto.

         IF h-objeto:NAME = "fpage6" THEN
            ASSIGN h-fPage6 = h-objeto.

         IF h-objeto:NAME = "fpage8" THEN
            ASSIGN h-fPage8 = h-objeto.

         IF h-objeto:NAME = "fpage14" THEN
            ASSIGN h-fPage14 = h-objeto. 

         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END. 
   ASSIGN l-preco-unico = NO.
END.

IF p-ind-event = "AFTER-INITIALIZE" THEN DO:
   /* Cria um POPUP Menu com os Cortes Comerciais */
   CREATE MENU h-menu
          ASSIGN POPUP-ONLY = TRUE.

   FOR EACH corte-comerc WHERE
            corte-comerc.compr-med > 0 NO-LOCK.
       CREATE MENU-ITEM h-menu-item
              ASSIGN PARENT = h-menu
                     LABEL = corte-comerc.codigo + " - " + corte-comerc.descricao
              TRIGGERS:
                  ON "CHOOSE":U PERSISTENT RUN esupc/upc-pd4000c2.p. /* Atribui Menu selecionado ao Acodicionamento */
              END TRIGGERS.
   END.

   CREATE BUTTON wh-btn-confirma
         ASSIGN FRAME = p-wgh-frame
                NO-FOCUS = YES
                FLAT-BUTTON  = YES
                WIDTH        = 4
                ROW          = 2.85
                COL          = 86
                VISIBLE      = YES
                SENSITIVE    = YES
                TOOLTIP      = "Identica se Pedido foi efetivado / Efetiva Pedido"
                TRIGGERS:
                    ON "CHOOSE":U PERSISTENT RUN esupc/upc-pd4000c1.p. /* Confirma Pedido */
               END TRIGGERS.


   CREATE TEXT tx-qt-acumulada
         ASSIGN FRAME        = h-fPage6
                FORMAT       = "x(17)"
                WIDTH        = 17
                SCREEN-VALUE = "Qt Acumulada:"
                ROW          = 12.7
                COL          = 27
                FGCOLOR      = 1
                VISIBLE      = YES. 

   CREATE FILL-IN wh-qt-acumulada
        ASSIGN FRAME             = h-fPage6
               SIDE-LABEL-HANDLE = tx-qt-acumulada:HANDLE
               DATA-TYPE         = "DECIMAL" 
               FORMAT            = "->>,>>>,>>9.9999"
               WIDTH             = 12
               HEIGHT            = 0.88
               ROW               = 12.55
               COL               = 37.7
               FGCOLOR           = 1
               BGCOLOR           = 8
               LABEL             = "Qt Acumulada:"
               VISIBLE           = YES
               SENSITIVE         = NO.

   CREATE TEXT tx-acond
        ASSIGN FRAME        = h-fPage8
               FORMAT       = "x(17)"
               WIDTH        = 17
               SCREEN-VALUE = "Corte Comerc.:"
               ROW          = 2.8
               COL          = 26.8
               VISIBLE      = YES.

   CREATE FILL-IN wh-acond
        ASSIGN FRAME             = h-fPage8
               SIDE-LABEL-HANDLE = tx-acond:HANDLE
               FORMAT            = "x(20)"
               NAME              = "fi-acond"
               WIDTH             = 15
               HEIGHT            = 0.79
               ROW               = 2.7
               COL               = 37.3
               LABEL             = "Corte Comerc.:"
               VISIBLE           = YES
               SENSITIVE         = NO
               POPUP-MENU        = h-menu
               TOOLTIP           = "Click Bot∆o Direito do Mouse para Cortes"
               TRIGGERS:
                   ON "ENTRY":U PERSISTENT RUN esupc/upc-pd4000e8.p. /* Mostra help na linha de status */
               END TRIGGERS.

   CREATE TEXT tx-lote
        ASSIGN FRAME        = h-fPage8
               FORMAT       = "x(5)"
               WIDTH        = 5
               SCREEN-VALUE = "Lote:"
               ROW          = 2.8
               COL          = 53
               VISIBLE      = YES.

   CREATE FILL-IN wh-lote
        ASSIGN FRAME             = h-fPage8
               SIDE-LABEL-HANDLE = tx-lote:HANDLE
               FORMAT            = "x(2)"
               NAME              = "fi-lote"
               WIDTH             = 4
               HEIGHT            = 0.79
               ROW               = 2.7
               COL               = 56.8
               LABEL             = "Lote:"
               VISIBLE           = YES
               SENSITIVE         = NO
               TRIGGERS:
                   ON "ENTRY":U PERSISTENT RUN esupc/upc-pd4000e1.p. /* Atribui primeira Letra do Acodicionamento ao Lote*/
               END TRIGGERS.

   wh-acond:LOAD-MOUSE-POINTER("image/rbm.cur":U).   

   ASSIGN h-campo = h-fpage2:FIRST-CHILD.
   ASSIGN h-campo = h-campo:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-campo):
      IF h-campo:NAME = 'br-itens' THEN 
         ON 'row-display':U OF h-campo PERSISTENT RUN esupc/upc-pd4000e6.p. 
      ASSIGN h-campo = h-campo:NEXT-SIBLING.
   END.

   ASSIGN h-campo = h-fpage4:FIRST-CHILD.
   ASSIGN h-campo = h-campo:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-campo):
      IF h-campo:NAME = "cod-priori" THEN DO.
         ASSIGN h-cod-priori = h-campo.
         ON 'entry':U OF h-campo PERSISTENT RUN esupc/upc-pd4000e5.p. 
      END.
      ASSIGN h-campo = h-campo:NEXT-SIBLING.
   END.

   ASSIGN h-campo = h-fPage6:FIRST-CHILD.
   ASSIGN h-campo = h-campo:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-campo):
      IF h-campo:NAME = "nr-sequencia" THEN
         ON 'leave':U OF h-campo PERSISTENT RUN esupc/upc-pd4000l4.p (INPUT h-fpage6). /* Busca ultimo Item/Referencia */ 

      IF h-campo:NAME = "cod-refer" THEN
         ON 'entry':U OF h-campo PERSISTENT RUN esupc/upc-pd4000e3.p (INPUT h-fPage6). /* Busca ultimo acondionamento/Lote */ 
      
      IF h-campo:NAME = "vl-preori" THEN
         ON 'leave':U OF h-campo PERSISTENT RUN esupc/upc-pd4000l3.p (INPUT h-cb-frame). 

      IF h-campo:NAME = 'btSaveItem' THEN DO.
         ON 'mouse-select-click':U OF h-campo PERSISTENT RUN esupc/upc-pd4000e4.p (INPUT h-fPage6). /* Valida Acondiciomaneto e Lote */
         ON 'ENTER':U OF h-campo PERSISTENT RUN esupc/upc-pd4000e4.p (INPUT h-fPage6). 
      END.
      
      ASSIGN h-campo = h-campo:NEXT-SIBLING.
   END.

   ASSIGN h-campo = h-fPage14:FIRST-CHILD.
   ASSIGN h-campo = h-campo:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-campo):
      IF h-campo:NAME = "btSaveTerm" THEN 
         ON 'mouse-select-click':U OF h-campo PERSISTENT RUN esupc/upc-pd4000m2.p. /* Valida Condiá∆o de Pagamento especial */

      ASSIGN h-campo = h-campo:NEXT-SIBLING.
   END.

   ASSIGN h-campo = h-fPage8:FIRST-CHILD.
   ASSIGN h-campo = h-campo:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-campo):
      IF h-campo:NAME = "cod-entrega" THEN 
         ASSIGN h-campo:WIDTH = 12.

      IF h-campo:NAME = "dt-entrega" THEN DO.
         wh-acond:MOVE-AFTER-TAB-ITEM(h-campo).
         wh-lote:MOVE-AFTER-TAB-ITEM(wh-acond).
         LEAVE.
      END.

      ASSIGN h-campo = h-campo:NEXT-SIBLING.
   END.
END.

IF p-ind-event = "AFTER-DISPLAY" THEN DO.
   FIND ped-venda WHERE 
        ROWID(ped-venda) = p-row-table NO-LOCK.
   
   IF VALID-HANDLE(wh-btn-confirma) THEN DO.
      IF ped-venda.completo THEN
         wh-btn-confirma:LOAD-IMAGE("image/imt-chck1.bmp").
      ELSE
         wh-btn-confirma:LOAD-IMAGE("image/im-uchk4.bmp").
   END.
END.

IF p-ind-event = "before_pi-enableitem" THEN DO:

   ASSIGN wh-acond:SENSITIVE = h-campo:SENSITIVE 
          wh-lote:SENSITIVE = h-campo:SENSITIVE.
   
   ASSIGN wh-acond:LABEL = "Corte Comerc.:"
          wh-lote:LABEL = "Lote:"
          wh-qt-acumulada:LABEL = "Qt Acumulada:"
          wh-qt-acumulada:FGCOLOR = 1
          wh-qt-acumulada:BGCOLOR = 8
          tx-qt-acumulada:FGCOLOR = 1.

   IF VALID-HANDLE(wh-qt-acumulada) THEN 
      ASSIGN wh-qt-acumulada:SCREEN-VALUE = STRING(de-qtd-tot-ped).
END.

IF p-ind-event = "AfterDisplayItem" THEN DO:
   ASSIGN wh-acond:LABEL = "Corte Comerc.:"
          wh-lote:LABEL = "Lote:"
          wh-qt-acumulada:LABEL   = "Qt Acumulada:"
          wh-qt-acumulada:FGCOLOR = 1
          wh-qt-acumulada:BGCOLOR = 8
          tx-qt-acumulada:FGCOLOR = 1.

   FIND ped-item WHERE 
        ROWID(ped-item) = p-row-table NO-LOCK NO-ERROR.

   IF AVAIL ped-item THEN DO.
      FIND ped-item-ext WHERE
           ped-item-ext.nome-abrev   = ped-item.nome-abrev AND
           ped-item-ext.nr-pedcli    = ped-item.nr-pedcli AND
           ped-item-ext.nr-sequencia = ped-item.nr-sequencia AND
           ped-item-ext.it-codigo    = ped-item.it-codigo AND
           ped-item-ext.cod-refer    = ped-item.cod-refer NO-ERROR.

      IF wh-acond:SCREEN-VALUE <> "" AND
         wh-acond:SENSITIVE = YES AND AVAIL ped-item THEN DO.
         IF NOT AVAIL ped-item-ext THEN DO.
            CREATE ped-item-ext.
            ASSIGN ped-item-ext.nome-abrev   = ped-item.nome-abrev
                   ped-item-ext.nr-pedcli    = ped-item.nr-pedcli
                   ped-item-ext.nr-sequencia = ped-item.nr-sequencia
                   ped-item-ext.it-codigo    = ped-item.it-codigo
                   ped-item-ext.cod-refer    = ped-item.cod-refer
                   ped-item-ext.reservado    = NO.
         END.
         FIND corte-comerc WHERE
              corte-comerc.descricao = wh-acond:SCREEN-VALUE
              NO-LOCK NO-ERROR.

         IF AVAIL corte-comerc THEN
            ASSIGN ped-item-ext.acondicionamento = corte-comerc.descricao
                   ped-item-ext.corte-comerc = corte-comerc.codigo
                   ped-item-ext.lote = wh-lote:SCREEN-VALUE + ped-item.cod-refer.
      END.
      ELSE DO.
          ASSIGN wh-acond:SCREEN-VALUE = IF AVAIL ped-item-ext
                                         THEN ped-item-ext.acondicionamento
                                         ELSE ""
                 wh-lote:SCREEN-VALUE = IF AVAIL ped-item-ext
                                        THEN SUBSTR(ped-item-ext.lote,1,2)
                                        ELSE "".
      END.
      ASSIGN wh-acond:SENSITIVE = h-campo:SENSITIVE 
             wh-lote:SENSITIVE = h-campo:SENSITIVE.
   END.

   FIND ped-venda OF ped-item NO-LOCK NO-ERROR.
   IF VALID-HANDLE(wh-btn-confirma) AND AVAIL ped-venda THEN DO.
      IF ped-venda.completo THEN
         wh-btn-confirma:LOAD-IMAGE("image/imt-chck1.bmp").
      ELSE
         wh-btn-confirma:LOAD-IMAGE("image/im-uchk4.bmp").
   END.

   IF VALID-HANDLE(wh-qt-acumulada) THEN DO.
      ASSIGN de-qtd-tot-ped = 0.
      FOR EACH ped-item OF ped-venda NO-LOCK.
          ASSIGN de-qtd-tot-ped = de-qtd-tot-ped + ped-item.qt-pedida.
      END.
      ASSIGN wh-qt-acumulada:SCREEN-VALUE = STRING(de-qtd-tot-ped).
   END.
END.

