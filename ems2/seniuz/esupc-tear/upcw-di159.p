/* Programa: upcw-di159.p
** Objetivo: Atualizar a tabela hist¢rico de altera‡äes de situa‡Æo de avalia‡Æo de 
**           cr‚dito de pedidos de venda (hist-cred-ped), a partir de altera‡äes feitas
**           na tabela da Datasul (ped-venda).
** Autor...: Gilvando Souza Araujo
** Data....: 19/Abr/2005
*/

DEFINE PARAMETER BUFFER p-table FOR ped-venda.
DEFINE PARAMETER BUFFER p-table-old FOR ped-venda.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR c-new-cond AS CHAR FORMAT "x(40)".
DEF VAR c-old-cond AS CHAR FORMAT "x(40)".
DEF VAR c-mensagem AS CHAR.

IF NOT NEW p-table THEN DO.
   FIND param-dis NO-LOCK NO-ERROR.

   IF p-table.cod-sit-aval <> p-table-old.cod-sit-aval THEN DO:
      FIND espec.hist-cred-ped WHERE
           espec.hist-cred-ped.nome-abrev = p-table.nome-abrev AND
           espec.hist-cred-ped.nr-pedcli  = p-table.nr-pedcli
           NO-ERROR.
    
      IF NOT AVAIL espec.hist-cred-ped THEN DO:
          CREATE espec.hist-cred-ped.
          ASSIGN espec.hist-cred-ped.nome-abrev = p-table.nome-abrev 
                 espec.hist-cred-ped.nr-pedcli  = p-table.nr-pedcli
                 espec.hist-cred-ped.ult-alter  = STRING(c-seg-usuario,"x(12)")  +
                                                  STRING(TODAY,"99/99/9999") +
                                                  STRING(TIME,"HH:MM:SS") +
                                                  STRING(p-table-old.cod-sit-aval,"99") +  
                                                  STRING(p-table.cod-sit-aval,"99").
      END.
      ELSE DO:
          ASSIGN espec.hist-cred-ped.pen-alter = espec.hist-cred-ped.ult-alter
                 espec.hist-cred-ped.ult-alter = STRING(c-seg-usuario,"x(12)") +
                                                 STRING(TODAY,"99/99/9999") +
                                                 STRING(TIME,"HH:MM:SS") +
                                                 STRING(p-table-old.cod-sit-aval,"99") +  
                                                 STRING(p-table.cod-sit-aval,"99").
      END.
   END.

   IF p-table.cod-cond-pag <> p-table-old.cod-cond-pag THEN DO:
      ASSIGN c-new-cond = STRING(p-table.cod-cond-pag)
             c-old-cond = STRING(p-table-old.cod-cond-pag).

      FIND cond-pagto WHERE
           cond-pagto.cod-cond-pag = p-table.cod-cond-pag 
           NO-LOCK NO-ERROR.

      ASSIGN c-new-cond = IF AVAIL cond-pagto
                          THEN c-new-cond + " - " + cond-pagto.descricao
                          ELSE c-old-cond + " - ESPECIAL".

      FIND cond-pagto WHERE
           cond-pagto.cod-cond-pag = p-table-old.cod-cond-pag 
           NO-LOCK NO-ERROR.
      ASSIGN c-old-cond = IF AVAIL cond-pagto
                          THEN c-old-cond + " - " + cond-pagto.descricao
                          ELSE c-old-cond + " - ESPECIAL".

      ASSIGN c-mensagem = "Pedido: " + p-table.nr-pedcli + " Cliente: " + p-table.nome-abrev + CHR(13) +
                          "Usu rio: " + c-seg-usuario + " Data: " + STRING(TODAY,"99/99/9999") + " Hora: " + STRING(TIME,"HH:MM") + 
                          CHR(13) + CHR(13) +
                          "Alterou Condi‡Æo de Pagamento   De: " + c-old-cond + 
                          " Para: " + c-new-cond.
      RUN pi-envia-email.
   END.

   IF p-table.dt-entrega  <> p-table-old.dt-entrega THEN DO:
      ASSIGN c-mensagem = "Pedido: " + p-table.nr-pedcli + " Cliente: " + p-table.nome-abrev + CHR(13) +
                          "Usu rio: " + c-seg-usuario + " Data: " + STRING(TODAY,"99/99/9999") + " Hora: " + STRING(TIME,"HH:MM") + 
                          CHR(13) + CHR(13) +
                          "Alterou Data de Entrega do Pedido De: " + STRING(p-table-old.dt-entrega) + 
                          " Para: " + STRING(p-table.dt-entrega).

      RUN pi-envia-email.
   END.
END.

PROCEDURE pi-envia-email.
    RUN esapi/esapi002.p (INPUT "teartextil@teartextil.com.br", /* e-mail remetente */
                          INPUT param-dis.emails-fin, /* e-mail destinat rio */
                          INPUT "Altera‡Æo no Pedido de Venda: " + p-table.nr-pedcli, /* Assunto */
                          INPUT c-mensagem, /* Mensagem */
                          INPUT "", /*arquivo anexo*/
                          INPUT YES). /* Mostra Erros */
END PROCEDURE.
