/* Programa: upc-cd0704l2.p
** Objetivo: 
** Autor...: FµBIO LANZA --> MAIO/2015
*/

DEF NEW GLOBAL SHARED VAR h-endereco-cob AS HANDLE.
DEF VAR c-mensagem AS CHAR.

ASSIGN c-mensagem = "".
FOR EACH emitente WHERE
         emitente.endereco-cob = h-endereco-cob:SCREEN-VALUE NO-LOCK.

    IF emitente.identif <> 2 THEN
       ASSIGN c-mensagem = c-mensagem + "Cliente: " + STRING(emitente.cod-emitente, "999999") + " - " + 
                           emitente.nome-abrev + " Endere‡o: " + emitente.endereco + "  Cidade.: " + TRIM(emitente.cidade) + "/" + emitente.estado + CHR(13).                     
END.
IF c-mensagem <> "" THEN
   MESSAGE "ENCONTREI NESTE ENDERE€O ESTE(s) CLIENTE(s)" SKIP(1)
           c-mensagem
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

