/* Programa: seek-cep.p
*/

DEF INPUT  PARAMETER p-cep       AS CHAR. 
DEF OUTPUT PARAMETER p-situacao  AS CHAR.
DEF OUTPUT PARAMETER p-endereco  LIKE emitente.endereco.
DEF OUTPUT PARAMETER p-bairro    LIKE emitente.bairro.  
DEF OUTPUT PARAMETER p-cidade    LIKE emitente.cidade.  
DEF OUTPUT PARAMETER p-estado    LIKE emitente.estado. 

DEF BUFFER cidade FOR ems2ima.cidade.
DEF BUFFER empresa FOR mgadm.empresa.

DEF TEMP-TABLE tt-cep
    FIELD situacao AS CHAR
    FIELD cep      LIKE emitente.cep
    FIELD endereco LIKE emitente.endereco
    FIELD bairro   LIKE emitente.bairro
    FIELD cidade   LIKE emitente.cidade
    FIELD estado   LIKE emitente.estado
    FIELD ibge     LIKE cidade.int-2.

DEF VAR c-logradouro AS CHAR FORMAT "x(20)".
DEF VAR c-comando AS CHAR FORMAT "x(50)".
DEF VAR c-arq-saida AS CHAR FORMAT "x(30)".

ASSIGN c-arq-saida = SESSION:TEMP-DIRECTORY + "RESULT.CSV".
OS-DELETE VALUE(c-arq-saida).

ASSIGN p-cep = REPLACE(p-cep,"-","").
IF p-cep = "" THEN DO.
   ASSIGN p-situacao = '0'.
   RETURN "ADM-ERROR".
END.

FIND FIRST empresa NO-LOCK NO-ERROR.

ASSIGN c-comando = SEARCH("CepOnline\exe\ceponline.exe") + ' ' +  p-cep + ' ' + mgadm.empresa.cgc + ' ' + c-arq-saida. 

OS-DELETE VALUE(c-arq-saida).
OS-COMMAND SILENT VALUE(c-comando).

/*
IF SEARCH(c-arq-saida) = ? THEN DO.
   ASSIGN p-situacao = '0'.
   RETURN 'ADM-ERROR'.
END.
*/

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
   IF tt-cep.ibge <> 0 THEN DO.
      FIND cidade WHERE
           cidade.estado = tt-cep.estado AND
           cidade.cidade = tt-cep.cidade NO-ERROR.
      IF AVAIL cidade THEN
         ASSIGN cidade.int-2 = tt-cep.ibge.
   END.

   ASSIGN p-endereco = tt-cep.endereco
          p-bairro   = tt-cep.bairro
          p-cidade   = tt-cep.cidade
          p-estado   = tt-cep.estado.
END.



       
