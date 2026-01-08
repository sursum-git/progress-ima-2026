/* Programa: upc-cp0324l1.p
** Objetivo: 
** Autor...: Prodb - Toninho  Mar‡o/2004
*/

DEF NEW GLOBAL SHARED VAR h-cod-refer AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-lote-serie AS HANDLE.

IF h-lote-serie:SENSITIVE THEN 
   ASSIGN h-lote-serie:SCREEN-VALUE = "XX" + h-cod-refer:SCREEN-VALUE.
