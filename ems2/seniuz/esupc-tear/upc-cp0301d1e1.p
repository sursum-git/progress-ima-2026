/* Trigger de Entry para o campo Lote (fi-lote-serie) */
/* Toninho - dbNET 18/10/2004 */

DEF NEW GLOBAL SHARED VAR h-cod-refer AS HANDLE.
DEF NEW GLOBAL SHARED VAR h-lote-serie AS HANDLE.

IF h-lote-serie:SCREEN-VALUE = "" THEN
   ASSIGN h-lote-serie:SCREEN-VALUE = "XX" + h-cod-refer:SCREEN-VALUE.

