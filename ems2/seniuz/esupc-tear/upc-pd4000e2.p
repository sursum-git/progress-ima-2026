/* Programa: upc-pd4000e2.p
** Objetivo: Trigger de 'Entry' para o botÆo de c¢pia de Pedido 
**           Salvar o Pedido Original para repassar o acondicionamento ao
**           novo pedido no t‚rmino da c¢pia
** Autor...: Prodb - Toninho  Mar‡o/2004
*/

DEF INPUT PARAMETER h-win AS HANDLE.
DEF VAR h-campo AS WIDGET-HANDLE.

DEF NEW GLOBAL SHARED VAR c-nome-abrev LIKE ped-venda.nome-abrev NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-nr-pedcli  LIKE ped-venda.nr-pedcli  NO-UNDO.

ASSIGN h-campo = h-win:FIRST-CHILD.
ASSIGN h-campo = h-campo:FIRST-CHILD.
DO WHILE VALID-HANDLE(h-campo):
   IF h-campo:NAME = 'nome-abrev' THEN
      ASSIGN c-nome-abrev = h-campo:SCREEN-VALUE.
   IF h-campo:NAME = 'nr-pedcli' THEN
      ASSIGN c-nr-pedcli = h-campo:SCREEN-VALUE.
   ASSIGN h-campo = h-campo:NEXT-SIBLING.
END.

