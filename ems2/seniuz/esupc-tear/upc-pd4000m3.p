/* Programa: upc-pd4000m3.p
** Objetivo: Executar Programa espec°fico de Implantaá∆o de Pedidos
** Autor...: DbNET - Toninho  Junho/2003
** Observ..: Desabilitamos o programa principal (pd4000) para que o 
**           o programa chamado fique sempre ativo, identico Ö um Dialog,
**           quando o programa chamado for fechado, habilitamos novamente o
**           pd4000, n∆o foi utilizado "custom-dialog" porque o mesmo n∆o
**           permite utilizar Zoom...
*/
DEF INPUT PARAMETER p-wgh-frame AS WIDGET-HANDLE NO-UNDO.

ASSIGN p-wgh-frame:WINDOW:SENSITIVE = NO.
RUN esp/espd4000.p. 
ASSIGN p-wgh-frame:WINDOW:SENSITIVE = YES.
