DEF VAR c-mensagem AS CHAR.
DEF VAR i-qt-fd AS INT INIT 134.
DEF VAR de-peso AS DEC INIT 1025.2.

ASSIGN c-mensagem = "Favor baixar " + STRING(i-qt-fd) + " com o peso de " +
                     STRING(de-peso) + " no EMS" + CHR(13) + CHR(13) +
                    "Obrigado, " + CHR(13) +
                    "Toninho".

RUN esapi/esapi002.p (INPUT "antonio.souza@teartextil.com.br", /* e-mail remetente*/
                      INPUT "fabio.lanza@teartextil.com.br,gilvando@teartextil.com.br", /* e-mail destino */
                      INPUT "Baixa de Fardos de Algod∆o", /* Assunto */
                      INPUT c-mensagem, /* Mensagem */
                      INPUT "",  /*arquivo anexo*/
                      INPUT YES). /* Mostra Erros */
