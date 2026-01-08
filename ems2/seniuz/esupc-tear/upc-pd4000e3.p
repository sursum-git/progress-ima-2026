/* Programa: upc-pd4000e3.p
** Objetivo: Trigger de 'Entry' para o campo cod-refer (Referˆncia do Item) 
**           Verifica se o item est  Obsoleto ou fora de Linha
**           Sugere Quantidade e Pre‡o do item baseado no £ltimo item digitado
**           Habilita o acondicionamento e o Lote, sugerindo os valores
**           conforme o £ltimo item digitado.
** Autor...: Prodb - Toninho  Mar‡o/2004
*/

DEF INPUT PARAMETER h-fPage6 AS HANDLE.

DEF NEW GLOBAL SHARED VAR wh-acond AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-lote  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-nome-abrev AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-nr-pedcli AS HANDLE NO-UNDO.

DEF VAR h-campo AS WIDGET-HANDLE.
DEF VAR h-item AS WIDGET-HANDLE.

DEF VAR de-qtd-estoq LIKE saldo-estoq.qtidade-atu.

ASSIGN h-campo = h-fPage6:FIRST-CHILD.
ASSIGN h-campo = h-campo:FIRST-CHILD.
DO WHILE VALID-HANDLE(h-campo):
   IF h-campo:NAME = 'it-codigo' THEN DO.
      ASSIGN h-item = h-campo.

      FIND FIRST item-ext WHERE
                 item-ext.it-codigo = h-item:SCREEN-VALUE NO-ERROR.
      IF AVAIL item-ext THEN DO.
         IF item-ext.cod-obsoleto = "1" OR
            item-ext.cod-obsoleto = "3" OR
            item-ext.cod-obsoleto = "4" OR
            item-ext.cod-obsoleto = "5" THEN DO.
            ASSIGN de-qtd-estoq = 0.
            FOR EACH saldo-estoq WHERE
                     saldo-estoq.it-codigo = item-ext.it-codigo NO-LOCK.
                ASSIGN de-qtd-estoq = de-qtd-estoq + saldo-estoq.qtidade-atu.
            END.
            IF de-qtd-estoq <= 0 THEN DO.
               MESSAGE "Item fora de linha. Contacte PCP/Acabado!" VIEW-AS ALERT-BOX.
               APPLY 'entry' TO h-item.
               RETURN NO-APPLY.
            END.
         END.
      END.
   END.

   FIND LAST ped-item WHERE
             ped-item.nome-abrev = h-nome-abrev:SCREEN-VALUE AND 
             ped-item.nr-pedcli = h-nr-pedcli:SCREEN-VALUE AND 
             ped-item.it-codigo = h-item:SCREEN-VALUE NO-LOCK NO-ERROR.

   IF h-campo:NAME = "vl-preori" THEN
      IF AVAIL ped-item THEN
         ASSIGN h-campo:SCREEN-VALUE = STRING(ped-item.vl-preori).

   IF h-campo:NAME = "qt-pedida" THEN
      IF AVAIL ped-item THEN
         ASSIGN h-campo:SCREEN-VALUE = STRING(ped-item.qt-pedida).

   ASSIGN h-campo = h-campo:NEXT-SIBLING.
END.

ASSIGN wh-acond:SENSITIVE = YES
       wh-lote:SENSITIVE = YES.

FIND LAST ped-item-ext WHERE
          ped-item-ext.nome-abrev = h-nome-abrev:SCREEN-VALUE AND
          ped-item-ext.nr-pedcli  = h-nr-pedcli:SCREEN-VALUE NO-LOCK NO-ERROR.

IF AVAIL ped-item-ext THEN 
    ASSIGN wh-acond:SCREEN-VALUE = ped-item-ext.acondicionamento
           wh-lote:SCREEN-VALUE = SUBSTR(ped-item-ext.lote,1,2).

