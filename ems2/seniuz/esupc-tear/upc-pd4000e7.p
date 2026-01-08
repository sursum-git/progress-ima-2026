/* Programa: upc-pd4000e7.p
** Objetivo: Trigger de 'Entry' para o campo nr-pedcli (Pedido)
**           Verificar se o Cliente est† cadastrado ou se no Grupo 99 - Desativado
**           Zerar o Campo prioridade 
** Autor...: Prodb - Toninho  Maráo/2005
*/

DEF NEW GLOBAL SHARED VAR h-nome-abrev AS HANDLE NO-UNDO.

FIND emitente WHERE
     emitente.nome-abrev = h-nome-abrev:SCREEN-VALUE NO-LOCK NO-ERROR.
IF NOT AVAIL emitente THEN DO.
   MESSAGE "Cliente " h-nome-abrev:SCREEN-VALUE " n∆o Cadastrado..." VIEW-AS ALERT-BOX ERROR.
   APPLY 'entry' TO h-nome-abrev.
   RETURN NO-APPLY.
END.

IF emitente.cod-gr-cli = 99 THEN DO.
   MESSAGE "Cliente " h-nome-abrev:SCREEN-VALUE " est† relacionado ao Grupo 99-Inativo..." SKIP
           "Impossivel Implantar Pedido..." SKIP  
           "Procurar setor de Cadastro..." 
           VIEW-AS ALERT-BOX ERROR.
   APPLY 'entry' TO h-nome-abrev.
   RETURN NO-APPLY.
END.


/* Zerar Prioridade */
RUN esupc/upc-pd4000e5.p. 
