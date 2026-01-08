/* Programa: upc-pd4000dt1.p
** Objetivo: Trigger de 'Tab' para o campo nome-abrev no browse de pedidos
**           copiados
**           Buscar a Natureza de Opera‡Æo do Cliente informado no campo
**           nome-abrev e mostra-la na coluna calculada (h-calc-col).
** Autor...: DBNet - Toninho  Mar‡o/2005
*/

DEF INPUT PARAMETER h-nome-abrev AS WIDGET-HANDLE.
DEF INPUT PARAMETER h-calc-col AS WIDGET-HANDLE.

FIND emitente WHERE
     emitente.nome-abrev = h-nome-abrev:SCREEN-VALUE NO-LOCK NO-ERROR.

IF NOT AVAIL emitente THEN
   FIND emitente WHERE
        emitente.cod-emit = INT(h-nome-abrev:SCREEN-VALUE) NO-LOCK NO-ERROR.

ASSIGN h-calc-col:SCREEN-VALUE = IF AVAIL emitente 
                                 THEN emitente.nat-operacao
                                 ELSE "".
        
