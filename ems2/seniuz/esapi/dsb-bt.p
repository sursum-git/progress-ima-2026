/* Programa: i-dsbt.p
   Funcao..: Desabilitar botäes de Programas desenvolvidos em DDK
   Autor...: DBnet - Toninho    Agosto/2005 
*/
DEF INPUT PARAMETER h-button AS HANDLE.
                      
ASSIGN h-button:SENSITIVE = NO.
IF h-button:NAME = "bt-exi" OR
   h-button:NAME = "bt-joi" THEN
   ASSIGN h-button:SENSITIVE = YES.
