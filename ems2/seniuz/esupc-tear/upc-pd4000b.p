/* Programa: upc-pd4000b.p
** Objetivo: Acrescentar um toggle box onde ser  informado se deseja um pre‡o
**           £nico por artigo / lote
** Autor...: DBNet - Toninho  Nov/2005
** Observ..: Existem diversos programas que foram desenvolvidos como triggers,
**           cada programa existe uma letra e um numero sequencial, onde a
**           letra identifica o tipo do evento (t=tab, r=row-display,
**           m=mouse-seleck-clik, e=entry etc..... 
*/

DEF INPUT PARAM p-ind-event      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-ind-object     AS CHAR          NO-UNDO.
DEF INPUT PARAM p-wgh-object     AS HANDLE        NO-UNDO.
DEF INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE NO-UNDO.
DEF INPUT PARAM p-cod-table      AS CHAR          NO-UNDO.
DEF INPUT PARAM p-row-table      AS ROWID         NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-fPage2 AS HANDLE NO-UNDO.
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR wh-preco-artigo  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-rect1       AS WIDGET-HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR l-preco-unico AS LOG.

IF p-ind-event = "BEFORE-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.

   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "btOK" THEN
            ON 'mouse-select-click':U OF h-objeto PERSISTENT RUN esupc/upc-pd4000bm1.p. 

         IF h-objeto:NAME = "fpage2" THEN
            ASSIGN h-fpage2 = h-objeto.

        ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

IF p-ind-event = "AFTER-INITIALIZE" THEN DO:
   CREATE RECTANGLE wh-rect1
        ASSIGN FRAME              = h-fpage2
               ROW                = 10.8
               COL                = 31
               EDGE-PIXELS        = 2
               GRAPHIC-EDGE       = YES
               HEIGHT-PIXELS      = 36
               WIDTH-PIXELS       = 210
               FILLED             = NO.
           
   CREATE TOGGLE-BOX wh-preco-artigo
        ASSIGN FRAME              = h-fpage2
               ROW                = 11.2
               COL                = 32.7
               VISIBLE            = YES
               LABEL              = "Pre‡o énico por Artigo/Lote"
               SENSITIVE          = NO
               TOOLTIP            = "Replica Pre‡o do PRIMEIRO Artigo aos demais Artigos Iguais".

   ASSIGN wh-preco-artigo:SCREEN-VALUE = STRING(l-preco-unico).
END.

