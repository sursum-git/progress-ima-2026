/* Programa: cep_transportador.p
**           Verifica validade do Cep do emitente
*/

DEF VAR h-acomp         AS HANDLE NO-UNDO.
DEF VAR c-comando       AS CHAR.

DEF VAR c-ender-sn      LIKE transporte.endereco.

DEF VAR c-situacao      AS CHAR.
DEF var c-endereco      LIKE transporte.endereco.
DEF VAR c-bairro        LIKE transporte.bairro.  
DEF VAR c-cidade        LIKE transporte.cidade.  
DEF VAR c-estado        LIKE transporte.estado. 

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

OUTPUT STREAM sai_erro TO c:\temp\cep_inv_transp.csv CONVERT SOURCE "ibm850".
OUTPUT STREAM sai_ok   TO c:\temp\cep_val_transp.csv CONVERT SOURCE "ibm850".

PUT STREAM sai_erro "Codigo;CNPJ;Nome;Cep;Cidade;Estado" SKIP.
PUT STREAM sai_ok   "Codigo;CNPJ;Nome;Cep;Endereco;Bairro;Cidade;UF;Telefone;Telefax;E-Mail" SKIP.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Pesquisando_CEPs_por_transpecedor *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH transporte NO-LOCK:

    run pi-acompanhar in h-acomp (input "Transportador: " + STRING(transporte.cod-transp)). 

    IF INDEX(transporte.endereco,",") <> 0 THEN
       ASSIGN c-ender-sn = SUBSTR(transporte.endereco,1,INDEX(transporte.endereco,",") - 1).
    ELSE
       ASSIGN c-ender-sn = transporte.endereco.

    IF f-numerico(transporte.cep) = YES THEN
       RUN esapi/seek-cep.p (INPUT transporte.cep,
                             OUTPUT c-situacao,
                             OUTPUT c-endereco,
                             OUTPUT c-bairro,
                             OUTPUT c-cidade,
                             OUTPUT c-estado).
    ELSE
       ASSIGN c-situacao = "0". /* Cep inv lido */

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
    ASSIGN c-endereco = REPLACE(c-endereco,"ENGENHEIRO ","ENG.").
    ASSIGN c-endereco = REPLACE(c-endereco,"VEREADOR ","VER.").

    IF c-situacao = "0" THEN
       PUT STREAM sai_erro 
           transporte.cod-transp ";"
           transporte.cgc ";"
           transporte.nome ";"
           transporte.cep FORMAT "99999-999" ";"
           transporte.cidade ";"
           transporte.estado
           SKIP.
       ELSE
       IF c-ender-sn        <> c-endereco OR
          transporte.bairro <> c-bairro OR
          transporte.cidade <> c-cidade OR
          transporte.estado <> c-estado THEN
          PUT STREAM sai_ok UNFORMAT
              transporte.cod-transp ";"
              transporte.cgc ";"
              transporte.nome ";"
              transporte.cep FORMAT "99999-999" ";"

              IF c-ender-sn        <> c-endereco THEN c-ender-sn        ELSE "Ok" ";"
              IF transporte.bairro <> c-bairro   THEN transporte.bairro ELSE "Ok" ";"
              IF transporte.cidade <> c-cidade   THEN transporte.cidade ELSE "Ok" ";"
              IF transporte.estado <> c-estado   THEN transporte.estado ELSE "Ok" ";"
                  
              transporte.telefone ";"
              transporte.telefax ";"
              transporte.e-mail SKIP

              ";;;;"
              IF c-endereco <> c-ender-sn      THEN c-endereco ELSE "Ok" ";"
              IF c-bairro   <> transporte.bairro THEN c-bairro   ELSE "Ok" ";"
              IF c-cidade   <> transporte.cidade THEN c-cidade   ELSE "Ok" ";"
              IF c-estado   <> transporte.estado THEN c-estado   ELSE "Ok" SKIP.
END.
OUTPUT STREAM sai_erro CLOSE.
OUTPUT STREAM sai_ok CLOSE.
