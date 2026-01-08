/* Programa: upc-pd4000dr1.p
** Objetivo: Trigger de 'Row-Display' para o browse (br-digita) de pedidos
**           copiados
**           Buscar a Natureza de Opera‡Æo do Cliente informado na primeira
**           coluna do browse, e mostra-la na coluna calculada.
** Autor...: DBNet - Toninho  Mar‡o/2005
*/

DEF INPUT PARAMETER h-query AS HANDLE.
DEF INPUT PARAMETER h-calc-col AS WIDGET-HANDLE.

DEF VAR h-buffer AS HANDLE.
DEF VAR h-nome-abrev AS HANDLE.

ASSIGN h-buffer = h-query:GET-BUFFER-HANDLE(1)
       h-nome-abrev = h-buffer:BUFFER-FIELD(1).

FIND emitente WHERE
     emitente.nome-abrev = h-nome-abrev:BUFFER-VALUE NO-LOCK NO-ERROR.

ASSIGN h-calc-col:SCREEN-VALUE = IF AVAIL emitente 
                                 THEN emitente.nat-operacao
                                 ELSE "".


