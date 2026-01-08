/* Programa: seek-cep.p
*/

DEF INPUT  PARAMETER p-cep       AS CHAR. 
DEF OUTPUT PARAMETER p-situacao  AS CHAR.
DEF OUTPUT PARAMETER p-endereco  LIKE emitente.endereco.
DEF OUTPUT PARAMETER p-bairro    LIKE emitente.bairro.  
DEF OUTPUT PARAMETER p-cidade    LIKE emitente.cidade.  
DEF OUTPUT PARAMETER p-estado    LIKE emitente.estado. 

DEF TEMP-TABLE tt-cep
    FIELD situacao AS CHAR
    FIELD cep      LIKE emitente.cep
    FIELD endereco LIKE emitente.endereco
    FIELD bairro   LIKE emitente.bairro
    FIELD cidade   LIKE emitente.cidade
    FIELD estado   LIKE emitente.estado.

DEF VAR c-logradouro AS CHAR FORMAT "x(20)".
DEF VAR c-comando AS CHAR FORMAT "x(50)".
DEF VAR c-arq-saida AS CHAR FORMAT "x(30)".
DEF VAR c-cep-arqexe AS CHAR FORMAT "x(60)".

ASSIGN c-arq-saida = SESSION:TEMP-DIRECTORY + "RESULT.CSV".
OS-DELETE VALUE(c-arq-saida).

ASSIGN p-cep = REPLACE(p-cep,"-","").
IF p-cep = "" THEN DO.
   ASSIGN p-situacao = '0'.
   RETURN "ADM-ERROR".
END.

ASSIGN c-cep-arqexe = SEARCH("Seniuz\CepOnline\EXE\ceponline.exe").
FILE-INFO:FILE-NAME = SEARCH(c-cep-arqexe).

ASSIGN c-comando = FILE-INFO:FULL-PATHNAME + ' ' +  p-cep + ' 21126271000168 ' + c-arq-saida. 
/* 03123987000200 */
OS-COMMAND SILENT VALUE(c-comando).

INPUT FROM VALUE(c-arq-saida).
REPEAT:
   CREATE tt-cep.
   IMPORT DELIMITER ";" tt-cep.
END.
INPUT CLOSE.

FIND FIRST tt-cep WHERE
           tt-cep.situacao <> "" NO-LOCK NO-ERROR.

ASSIGN p-situacao = tt-cep.situacao
       p-endereco = ""
       p-bairro   = ""
       p-cidade   = ""
       p-estado   = "".

IF tt-cep.situacao = "1" THEN DO.
   ASSIGN c-logradouro = ENTRY(1,tt-cep.endereco," ").
  /*
   FIND abreviat WHERE
        abreviat.definicao = c-logradouro NO-LOCK NO-ERROR.
   IF AVAIL abreviat THEN
      ASSIGN tt-cep.endereco = REPLACE(tt-cep.endereco,c-logradouro,abreviat.cd-abrev).
  */
  ASSIGN p-endereco = tt-cep.endereco
         p-bairro   = tt-cep.bairro
         p-cidade   = tt-cep.cidade
         p-estado   = tt-cep.estado.
END.



       
