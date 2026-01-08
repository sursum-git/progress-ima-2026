/* Programa: cep_fornecedor.p
**           Verifica validade do Cep do fornecedor
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
         IF c-aux = "" OR 
            SUBSTR(c-aux,i-cont,1) < "0" OR
            SUBSTR(c-aux,i-cont,1) > "9" THEN
            ASSIGN l-numerico = NO.
      END.
   RETURN l-numerico.
END.

DEF STREAM sai_erro.
DEF STREAM sai_ok.

OUTPUT STREAM sai_erro TO c:\temp\cep_inv_forn.csv CONVERT SOURCE "ibm850".
OUTPUT STREAM sai_ok   TO c:\temp\cep_val_forn.csv CONVERT SOURCE "ibm850".

PUT STREAM sai_erro "Emitente;CNPJ;Nome;Cep" SKIP.
PUT STREAM sai_ok   "Emitente;CNPJ;Nome;Cep;Endereco;Bairro;Cidade;UF;Telefone-1;Telefone-2;Telefax;E-Mail" SKIP.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Pesquisando_CEPs_por_Emitente *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH emitente WHERE emitente.identific =  2 NO-LOCK:

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
    ELSE
       ASSIGN c-situacao = "0". /* Cep inv lido */
       
    IF emitente.cep-cob <> emitente.cep THEN DO:
       IF f-numerico(emitente.cep-cob) = YES THEN
          RUN esapi/seek-cep.p (INPUT emitente.cep-cob,
                                OUTPUT c-situacao-cob,
                                OUTPUT c-endereco-cob,
                                OUTPUT c-bairro-cob,
                                OUTPUT c-cidade-cob,
                                OUTPUT c-estado-cob).
       ELSE
          ASSIGN c-situacao-cob = "0". /* Cep inv lido */
    END.
    ELSE DO:
       ASSIGN c-situacao-cob = c-situacao
              c-endereco-cob = c-endereco
              c-bairro-cob   = c-bairro
              c-cidade-cob   = c-cidade
              c-estado-cob   = c-estado.
    END.

    ASSIGN c-endereco = REPLACE(c-endereco,"AVENIDA ","AV.").
    ASSIGN c-endereco = REPLACE(c-endereco,"ALAMEDA ","ALAM.").
    ASSIGN c-endereco = REPLACE(c-endereco,"RODOVIA ","ROD.").
    ASSIGN c-endereco = REPLACE(c-endereco,"TRAVESSA ","TRAV.").
    ASSIGN c-endereco = REPLACE(c-endereco,"CORONEL ","CEL.").
    ASSIGN c-endereco = REPLACE(c-endereco,"BRIGADEIRO ","BRIG.").
    ASSIGN c-endereco = REPLACE(c-endereco,"ACADEMICO ","ACAD.").
    ASSIGN c-endereco = REPLACE(c-endereco,"CONSELHEIRO ","CONS.").
    ASSIGN c-endereco = REPLACE(c-endereco,"MONSENHOR ","MONS.").
    ASSIGN c-endereco = REPLACE(c-endereco,"SENADOR ","SEN.").
    ASSIGN c-endereco = REPLACE(c-endereco,"DEPUTADO ","DEP.").
    ASSIGN c-endereco = REPLACE(c-endereco,"ALMIRANTE ","ALM.").
    ASSIGN c-endereco = REPLACE(c-endereco,"GENERAL ","GAL.").
    ASSIGN c-endereco = REPLACE(c-endereco,"TENENTE ","TEN.").
    ASSIGN c-endereco = REPLACE(c-endereco,"SARGENTO ","SARG.").
    ASSIGN c-endereco = REPLACE(c-endereco,"CAPITAO ","CAP.").
    ASSIGN c-endereco = REPLACE(c-endereco,"MARECHAL ","MAL.").
    ASSIGN c-endereco = REPLACE(c-endereco,"PREFEITO ","PREF.").
    ASSIGN c-endereco = REPLACE(c-endereco,"GOVERNADOR ","GOV.").
    ASSIGN c-endereco = REPLACE(c-endereco,"SENHORA ","SRA.").
    ASSIGN c-endereco = REPLACE(c-endereco,"PROFESSOR ","PROF.").
    ASSIGN c-endereco = REPLACE(c-endereco,"PRESIDENTE ","PRES.").
    ASSIGN c-endereco = REPLACE(c-endereco,"DESEMBARGADOR ","DESEMB.").
    ASSIGN c-endereco = REPLACE(c-endereco,"COMENDADOR ","COMEND.").
    ASSIGN c-endereco = REPLACE(c-endereco,"DOUTOR ","DR.").
    ASSIGN c-endereco = REPLACE(c-endereco,"ENGENHEIRO ","ENGO.").
    ASSIGN c-endereco = REPLACE(c-endereco,"VEREADOR ","VER.").

    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"AVENIDA ","AV.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"ALAMEDA ","ALAM.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"RODOVIA ","ROD.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"TRAVESSA ","TRAV.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"CORONEL ","CEL.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"BRIGADEIRO ","BRIG.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"ACADEMICO ","ACAD.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"CONSELHEIRO ","CONS.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"MONSENHOR ","MONS.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"SENADOR ","SEN.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"DEPUTADO ","DEP.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"ALMIRANTE ","ALM.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"GENERAL ","GAL.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"TENENTE ","TEN.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"SARGENTO ","SARG.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"CAPITAO ","CAP.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"MARECHAL ","MAL.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"PREFEITO ","PREF.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"GOVERNADOR ","GOV.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"SENHORA ","SRA.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"PROFESSOR ","PROF.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"PRESIDENTE ","PRES.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"DESEMBARGADOR ","DESEMB.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"COMENDADOR ","COMEND.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"DOUTOR ","DR.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"ENGENHEIRO ","ENGO.").
    ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"VEREADOR ","VER.").

    IF c-situacao = "0" OR c-situacao-cob = "0" THEN
       PUT STREAM sai_erro 
           emitente.cod-emitente ";"
           emitente.cgc ";"
           emitente.nome-emit ";"
           emitente.cep FORMAT "99999-999"
           SKIP.
       ELSE
       IF c-ender-sn          <> c-endereco     OR
          emitente.bairro     <> c-bairro       OR
          emitente.cidade     <> c-cidade       OR
          emitente.estado     <> c-estado       OR 
          c-ender-sn-cob      <> c-endereco-cob OR 
          emitente.bairro-cob <> c-bairro-cob   OR 
          emitente.cidade-cob <> c-cidade-cob   OR
          emitente.estado-cob <> c-estado-cob   THEN
          PUT STREAM sai_ok UNFORMAT
              emitente.cod-emitente ";"
              emitente.cgc ";"
              emitente.nome-emit ";"
              emitente.cep FORMAT "99999-999" ";"

              IF c-ender-sn      <> c-endereco THEN c-ender-sn      ELSE "Ok" ";"
              IF emitente.bairro <> c-bairro   THEN emitente.bairro ELSE "Ok" ";"
              IF emitente.cidade <> c-cidade   THEN emitente.cidade ELSE "Ok" ";"
              IF emitente.estado <> c-estado   THEN emitente.estado ELSE "Ok" ";"
                  
              emitente.telefone[1] ";"
              emitente.telefone[2] ";"
              emitente.telefax ";"
              emitente.e-mail SKIP

              emitente.cod-emitente ";;;;"
              IF c-endereco <> c-ender-sn      THEN c-endereco ELSE "Ok" ";"
              IF c-bairro   <> emitente.bairro THEN c-bairro   ELSE "Ok" ";"
              IF c-cidade   <> emitente.cidade THEN c-cidade   ELSE "Ok" ";"
              IF c-estado   <> emitente.estado THEN c-estado   ELSE "Ok" SKIP

              emitente.cod-emitente ";;;;"
              IF c-ender-sn-cob      <> c-endereco-cob THEN c-ender-sn-cob      ELSE "Ok" ";"
              IF emitente.bairro-cob <> c-bairro-cob   THEN emitente.bairro-cob ELSE "Ok" ";"
              IF emitente.cidade-cob <> c-cidade-cob   THEN emitente.cidade-cob ELSE "Ok" ";"
              IF emitente.estado-cob <> c-estado-cob   THEN emitente.estado-cob ELSE "Ok" SKIP

              emitente.cod-emitente ";;;;"
              IF c-endereco-cob <> c-ender-sn-cob      THEN c-endereco-cob ELSE "Ok" ";"
              IF c-bairro-cob   <> emitente.bairro-cob THEN c-bairro-cob   ELSE "Ok" ";"
              IF c-cidade-cob   <> emitente.cidade-cob THEN c-cidade-cob   ELSE "Ok" ";"
              IF c-estado-cob   <> emitente.estado-cob THEN c-estado-cob   ELSE "Ok" SKIP.
END.
OUTPUT STREAM sai_erro CLOSE.
OUTPUT STREAM sai_ok CLOSE.
