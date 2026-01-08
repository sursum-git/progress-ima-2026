/* Programa: upc-pd4000e5.p
** Objetivo: Trigger de 'Entry' para o campo cod-priori (Prioridade)
**           Zerar o Campo prioridade 
** Autor...: Prodb - Toninho  Mar‡o/2005
*/

DEF NEW GLOBAL SHARED VAR h-cod-priori AS HANDLE.                      
IF VALID-HANDLE(h-cod-priori) THEN
   ASSIGN h-cod-priori:SCREEN-VALUE = "0".
