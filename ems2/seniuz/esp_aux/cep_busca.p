/* Programa: cep_busca.p
*/

DEF VAR c-cep AS CHAR FORMAT "99999999".
DEF VAR c-comando AS CHAR FORMAT "x(100)".

DEF VAR c-situacao      AS CHAR.
DEF var c-endereco      LIKE emitente.endereco.
DEF VAR c-bairro        LIKE emitente.bairro.  
DEF VAR c-cidade        LIKE emitente.cidade.  
DEF VAR c-estado        LIKE emitente.estado. 

REPEAT:
   UPDATE c-cep.
   
   RUN esapi/seek-cep.p (INPUT c-cep,
                         OUTPUT c-situacao,
                         OUTPUT c-endereco,
                         OUTPUT c-bairro,
                         OUTPUT c-cidade,
                         OUTPUT c-estado).
   DISPLAY c-situacao
           c-endereco
           c-bairro
           c-cidade
           c-estado WITH SIDE-LABELS 1 COLUMN.
END.

