/* Programa: cep_emitente_bco
**           Verifica validade do Cep do emitente, mostrando os que est∆o
**           com logradouro em branco.
*/

DEF VAR h-acomp         AS HANDLE NO-UNDO.
DEF VAR c-comando       AS CHAR.

DEF VAR c-ender-sn      LIKE emitente.endereco.
DEF VAR c-ender-sn-cob  LIKE emitente.endereco.

DEF VAR c-situacao      AS CHAR.
DEF var c-endereco      LIKE emitente.endereco.
DEF VAR c-bairro        LIKE emitente.bairro.  
DEF VAR c-cidade        LIKE emitente.cidade.  
DEF VAR c-estado        LIKE emitente.estado. 
DEF VAR c-situacao-cob  AS CHAR.
DEF var c-endereco-cob  LIKE emitente.endereco.
DEF VAR c-bairro-cob    LIKE emitente.bairro.  
DEF VAR c-cidade-cob    LIKE emitente.cidade.  
DEF VAR c-estado-cob    LIKE emitente.estado. 

FUNCTION f-numerico RETURN LOGICAL (INPUT c-aux AS CHAR).
   DEF VAR i-cont     AS INT.
   DEF VAR l-numerico AS LOG INIT YES.
   IF c-aux = "" THEN
      ASSIGN l-numerico = NO.
   ELSE
      DO i-cont = 1 TO LENGTH(c-aux):
         IF SUBSTR(c-aux,i-cont,1) < "0" OR
            SUBSTR(c-aux,i-cont,1) > "9" THEN
            ASSIGN l-numerico = NO.
      END.
   RETURN l-numerico.
END.

DEF STREAM sai_erro.

OUTPUT STREAM sai_erro TO c:\temp\cep_rua_bco.csv CONVERT SOURCE "ibm850".

PUT STREAM sai_erro "Emitente;Cep;Endereco;Bairro;Cidade;Estado" SKIP.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Pesquisando_CEPs_por_Emitente *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH emitente WHERE 
        /* emitente.cod-emitente = 1451 AND */
         emitente.identific <> 2 NO-LOCK:

    run pi-acompanhar in h-acomp (input "Emitente: " + STRING(emitente.cod-emitente)). 

    IF INDEX(emitente.endereco,",") <> 0 THEN
       ASSIGN c-ender-sn = SUBSTR(emitente.endereco,1,INDEX(emitente.endereco,",") - 1).
    ELSE
       ASSIGN c-ender-sn = emitente.endereco.
    IF INDEX(emitente.endereco-cob,",") <> 0 THEN
       ASSIGN c-ender-sn-cob = SUBSTR(emitente.endereco-cob,1,INDEX(emitente.endereco-cob,",") - 1).
    ELSE
       ASSIGN c-ender-sn-cob = emitente.endereco-cob.
    IF f-numerico(emitente.cep) = YES THEN
       RUN esapi/seek-cep.p (INPUT emitente.cep,
                             OUTPUT c-situacao,
                             OUTPUT c-endereco,
                             OUTPUT c-bairro,
                             OUTPUT c-cidade,
                             OUTPUT c-estado).
       
    IF f-numerico(emitente.cep-cob) = YES THEN
       RUN esapi/seek-cep.p (INPUT emitente.cep-cob,
                             OUTPUT c-situacao-cob,
                             OUTPUT c-endereco-cob,
                             OUTPUT c-bairro-cob,
                             OUTPUT c-cidade-cob,
                             OUTPUT c-estado-cob).
    IF c-situacao = "1" THEN DO:
       IF c-endereco = " " OR c-endereco = "" THEN
          PUT STREAM sai_erro
              emitente.cod-emitente ";"
              emitente.cep ";"
              c-endereco ";"
              c-bairro ";"
              c-cidade ";"
              c-estado
              SKIP.
    END.
    IF c-situacao-cob = "1" THEN DO:
       IF c-endereco-cob = " " OR c-endereco-cob = "" THEN
          PUT STREAM sai_erro 
              emitente.cod-emitente ";"
              emitente.cep-cob ";"
              c-endereco-cob ";"
              c-bairro-cob ";"
              c-cidade-cob ";"
              c-estado-cob
              SKIP.
    END.
END.
OUTPUT STREAM sai_erro CLOSE.
