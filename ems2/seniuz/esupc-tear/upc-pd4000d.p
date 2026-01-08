/* Programa: upc-pd4000d.p
** Objetivo: Acrescentar uma coluna de Natureza de Operacao no Browse (br-digita)
**           onde ser† mostrada a NaturOper do Cliente informado
**           Esconder a coluna de Codigo de Entrega para caber a coluna de NatOper
**           Executar as triggers (programas) para o bot∆o copiar, o browse e o
**           nome abreviado do cliente
** Autor...: DBNet - Toninho  Maráo/2005
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
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-browse AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-nome-abrev AS WIDGET-HANDLE.
DEF VAR h-calc-col AS WIDGET-HANDLE.
DEF VAR h-col AS HANDLE.
DEF VAR j AS INT.

DEF VAR h-query AS HANDLE.

IF p-ind-event = "BEFORE-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.

   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = "br-digita" THEN DO.
         h-calc-col = h-objeto:ADD-CALC-COLUMN("char", "x(7)", "", "NatOper", 5).
         h-calc-col:LABEL-BGCOLOR  = ?.

         ASSIGN h-browse = h-objeto
                h-query = h-browse:QUERY.

         DO j = 1 TO h-browse:NUM-COLUMNS.
            ASSIGN h-col = h-browse:GET-BROWSE-COLUMN(j).

            IF h-col:NAME = "nome-abrev" THEN 
               ASSIGN h-nome-abrev = h-col.

            IF h-col:NAME = "cod-entrega" THEN
               ASSIGN h-col:VISIBLE = NO.
         END.
      END.

      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = 'bt-ok' THEN DO.
            ON 'MOUSE-SELECT-CLICK':U OF h-objeto PERSISTENT RUN esupc/upc-pd4000dm1.p (INPUT p-wgh-frame).
            ON 'ENTER':U OF h-objeto PERSISTENT RUN esupc/upc-pd4000dm1.p (INPUT p-wgh-frame). 
         END.
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.

   ON 'row-display':U OF h-browse PERSISTENT RUN esupc/upc-pd4000dr1.p (INPUT h-query, INPUT h-calc-col). 
   ON 'tab':U OF h-nome-abrev PERSISTENT RUN esupc/upc-pd4000dt1.p (INPUT h-nome-abrev, INPUT h-calc-col).
END.

