/* Programa: upc-cp0301d1.p
** Objetivo: Validar os Lotes para os itens de tipo de controle igual a 4
**           (referencia), afim de padronizar o nome do lote 
**           ((RP - Rolo Perfeito, RD - Rolo Defeituoso
**            PP - Pe‡a Perfeita, PD - Pe‡a Defeituosa) + Referencia.
**          
** Autor...: Prodb - Toninho  Outubro/2004
*/

/* Parameter Definitions ****************************************************/
define input parameter p-ind-event  as character.
define input parameter p-ind-object as character.
define input parameter p-wgh-object as handle.
define input parameter p-wgh-frame  as widget-handle.
define input parameter p-cod-table  as character.
define input parameter p-row-table  as rowid.

/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR h-container AS HANDLE. 
DEF NEW GLOBAL SHARED VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-it-codigo AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-cod-refer AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-lote-serie AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-nr-linha AS HANDLE.

/* Variable Definitions *****************************************************/
DEF VAR c-objeto AS CHAR NO-UNDO.

/* Variavies criadas na viewer dinamicamente*********************************/

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "CONTAINER" THEN
   ASSIGN h-container = p-wgh-object.

IF p-ind-event = "BEFORE-INITIALIZE" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "fi-it-codigo" THEN 
            ASSIGN h-it-codigo = h-objeto.

         IF h-objeto:NAME = "fi-cod-refer" THEN 
            ASSIGN h-cod-refer = h-objeto.

         IF h-objeto:NAME = "fi-nr-linha" THEN 
            ASSIGN h-nr-linha = h-objeto.

         IF h-objeto:NAME = "fi-lote-serie" THEN DO.
            ASSIGN h-lote-serie = h-objeto.
            ON 'entry':U OF h-objeto PERSISTENT RUN esupc/upc-cp0301d1e1.p.
         END.

         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.

IF p-ind-event = "ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v04in271.w" THEN DO:

   FIND item WHERE
        item.it-codigo = h-it-codigo:SCREEN-VALUE NO-LOCK NO-ERROR.

   IF h-nr-linha:SCREEN-VALUE <> '4' THEN DO.
      FIND FIRST operacao WHERE
                 operacao.it-codigo = item.it-codigo NO-LOCK NO-ERROR.
    
      IF NOT AVAIL operacao THEN DO.
         FIND FIRST rot-item WHERE
                    rot-item.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

         IF NOT AVAIL rot-item THEN DO.
            MESSAGE "Item da Ordem deve possuir pelo menos 1 (uma) Opera‡Æo..."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RUN select-page IN h-container (INPUT 1). 
            APPLY 'entry' TO h-it-codigo.
            RETURN 'NOK':U.
         END.
      END.
   END.

   IF item.tipo-con-est <> 4 THEN NEXT.

   IF LOOKUP(SUBSTR(h-lote-serie:SCREEN-VALUE,1,2),"PP,PD,RP,RD") = 0 THEN DO.
      MESSAGE "Lote deve inciar com PP,PD,RP,RD"  VIEW-AS ALERT-BOX.
      RUN select-page IN h-container (INPUT 2). 
      APPLY 'entry' TO h-lote-serie.
      RETURN 'NOK'.
   END.
   

   IF NOT h-lote-serie:SCREEN-VALUE MATCHES "*" + h-cod-refer:SCREEN-VALUE THEN DO.
      MESSAGE "Lote deve ser composto de PP,PD,RP,RD + Referencia"
              VIEW-AS ALERT-BOX.
      RUN select-page IN h-container (INPUT 2). 
      APPLY 'entry' TO h-lote-serie.
      RETURN 'NOK'.
   END.

   IF h-nr-linha:SCREEN-VALUE <> '4' THEN DO.
      FIND FIRST ref-estrut WHERE 
                 ref-estrut.it-codigo = h-it-codigo:SCREEN-VALUE AND 
                 ref-estrut.cod-ref-it = h-cod-refer:SCREEN-VALUE 
                 NO-LOCK NO-ERROR.
    
      IF NOT AVAIL ref-estrut THEN DO.
         MESSAGE "Nao encontrada Estrutura para o Item + Referencia Informados"
                  VIEW-AS ALERT-BOX.
         RUN select-page IN h-container (INPUT 1). 
         APPLY 'entry' TO h-cod-refer.
         RETURN 'NOK'.
      END.
   END.
END.

