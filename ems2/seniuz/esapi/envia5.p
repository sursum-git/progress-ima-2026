DEF VAR c-mensagem AS CHAR.

ASSIGN c-mensagem = "TESTEANDNO" + CHR(10) + "TESTE 10".

RUN esapi/esapi002.p (INPUT "tss@imatextil.com.br"  , /* e-mail remetente	*/
                      INPUT "ti.seniuz@gmail.com"		  , /* e-mail destino		*/
                      INPUT "TESTE EMAIL DATASULMAIL"	              , /* Assunto			*/
                      INPUT c-mensagem					  , /* Mensagem			*/
                      INPUT "d:\ton\HorasImaNov2019.pdf"	                		  , /* arquivo anexo		*/
                      INPUT YES)						  . /* Mostra Erros		*/
