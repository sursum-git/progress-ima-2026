DEF VAR c-remetente      AS CHAR INITIAL "imatextil@imatextil.com.br".
DEF VAR c-destinatario   LIKE param-dis.destinatario.
DEF VAR c-mensagem AS CHAR.

ASSIGN c-destinatario = 'thiago.cassimiro@imatextil.com.br'.      

ASSIGN c-mensagem = 'TESTE email22'.

RUN esapi/esapi002.p (INPUT c-remetente,                         /* e-mail remetente */
                      INPUT c-destinatario,                      /* e-mail destinat rio */
                      INPUT "TESTE e-mail", /* Assunto */
                      INPUT c-mensagem,                          /* Mensagem */
                      INPUT "",                                  /* Anexo */
                      INPUT YES).                                /* Mostra Erros */ 
