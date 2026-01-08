/* Programa: upc-pd4000l4.p
** Objetivo: Inser‡Æo dos campos Acondicionamento e lote.
** Autor...: Prodb - Toninho  Mar‡o/2004
*/

DEF INPUT PARAMETER h-fPage6 AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-nome-abrev AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-nr-pedcli AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-acond AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-lote   AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-campo AS WIDGET-HANDLE.

FIND LAST ped-item WHERE
          ped-item.nome-abrev = h-nome-abrev:SCREEN-VALUE AND 
          ped-item.nr-pedcli = h-nr-pedcli:SCREEN-VALUE NO-LOCK NO-ERROR.

ASSIGN h-campo = h-fPage6:FIRST-CHILD.
ASSIGN h-campo = h-campo:FIRST-CHILD.
DO WHILE VALID-HANDLE(h-campo):
   IF h-campo:NAME = 'it-codigo' THEN
      IF AVAIL ped-item THEN
         ASSIGN h-campo:SCREEN-VALUE = ped-item.it-codigo.

   IF h-campo:NAME = 'cod-refer' THEN
      IF AVAIL ped-item THEN
         ASSIGN h-campo:SCREEN-VALUE = ped-item.cod-refer.

   ASSIGN h-campo = h-campo:NEXT-SIBLING.
END.

ASSIGN wh-acond:SENSITIVE = NO
       wh-lote:SENSITIVE = NO.
