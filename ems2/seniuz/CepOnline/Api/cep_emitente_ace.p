/* Programa: cep_emitente_ace.p
** Objetivo: Acerta dados de endere‡os para emitentes, para posterior importa‡Æo. 
*/

DEF VAR i-seq        AS INT.
DEF VAR i-cont       AS INT.
DEF VAR c-numero     AS CHAR.
DEF VAR c-numero-cob AS CHAR.
DEF VAR c-ender-num     LIKE emitente.endereco.
DEF VAR c-ender-num-cob LIKE emitente.endereco.
DEF VAR i-cod-emitente  LIKE emitente.cod-emitente.
DEF var c-endereco      LIKE emitente.endereco.
DEF VAR c-bairro        LIKE emitente.bairro.  
DEF VAR c-cidade        LIKE emitente.cidade.  
DEF VAR c-estado        LIKE emitente.estado. 
DEF VAR c-situacao-cob  AS CHAR.
DEF var c-endereco-cob  LIKE emitente.endereco.
DEF VAR c-bairro-cob    LIKE emitente.bairro.  
DEF VAR c-cidade-cob    LIKE emitente.cidade.  
DEF VAR c-estado-cob    LIKE emitente.estado. 

DEF TEMP-TABLE tt-emitente
    FIELD cod-emitente LIKE emitente.cod-emitente
    FIELD cgc          LIKE emitente.cgc
    FIELD nome-emit    LIKE emitente.nome-emit
    FIELD cep          LIKE emitente.cep
    FIELD endereco     LIKE emitente.endereco
    FIELD bairro       LIKE emitente.bairro
    FIELD cidade       LIKE emitente.cidade
    FIELD estado       LIKE emitente.estado
    FIELD situacao     AS CHAR.
    
input from "c:/temp/cep_valido_1.csv".
SET ^.

repeat:
   create tt-emitente.
   import delimiter ";" tt-emitente.
end.
input close.

ASSIGN i-seq = 0.

OUTPUT TO c:/temp/cep_valido_2.csv.
PUT "Cod;Endereco-------------------------------x;Bairro;Cidade;Estado;End-Cob--------------------------------x;Bai-Cob;Cid-Cob;Est-Cob" SKIP.

FOR EACH tt-emitente.
    ASSIGN i-seq = i-seq + 1.
    IF i-seq = 1 THEN DO:
       ASSIGN i-cod-emitente = tt-emitente.cod-emitente.
    END.
    IF i-seq = 2 THEN DO:
       ASSIGN c-endereco     = tt-emitente.endereco
              c-bairro       = tt-emitente.bairro
              c-cidade       = tt-emitente.cidade
              c-estado       = tt-emitente.estado.
    END.
    IF i-seq = 4 THEN DO:
       ASSIGN c-endereco-cob = tt-emitente.endereco
              c-bairro-cob   = tt-emitente.bairro
              c-cidade-cob   = tt-emitente.cidade
              c-estado-cob   = tt-emitente.estado.
    END.
    IF i-seq = 4 THEN DO:
       ASSIGN c-numero     = ""
              c-numero-cob = "".
       FIND emitente WHERE emitente.cod-emitente = i-cod-emitente NO-LOCK.
       IF INDEX(emitente.endereco,",") <> 0 THEN
          ASSIGN c-numero = SUBSTR(emitente.endereco,INDEX(emitente.endereco,","),LENGTH(emitente.endereco) - INDEX(emitente.endereco,",") + 1).
       IF INDEX(emitente.endereco-cob,",") <> 0 THEN
          ASSIGN c-numero-cob = SUBSTR(emitente.endereco-cob,INDEX(emitente.endereco-cob,","),LENGTH(emitente.endereco-cob) - INDEX(emitente.endereco-cob,",") + 1).
       
       IF c-endereco <> "Ok" THEN DO:
          ASSIGN c-ender-num = TRIM(c-endereco) + c-numero.
       END.
       
       IF c-endereco-cob <> "Ok" THEN DO:
          ASSIGN c-ender-num-cob = TRIM(c-endereco-cob) + c-numero-cob.
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
       ASSIGN c-endereco = REPLACE(c-endereco,"ENGENHEIRO ","ENG.").
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
       ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"ENGENHEIRO ","ENG.").
       ASSIGN c-endereco-cob = REPLACE(c-endereco-cob,"VEREADOR ","VER.").

       PUT UNFORMAT 
           emitente.cod-emitente ";"
           IF c-endereco     = "Ok" THEN emitente.endereco     ELSE c-ender-num ";"
           IF c-bairro       = "Ok" THEN emitente.bairro       ELSE c-bairro ";"
           IF c-cidade       = "Ok" THEN emitente.cidade       ELSE c-cidade ";"
           IF c-estado       = "Ok" THEN emitente.estado       ELSE c-estado ";"
           IF c-endereco-cob = "Ok" THEN emitente.endereco-cob ELSE c-ender-num-cob ";"
           IF c-bairro-cob   = "Ok" THEN emitente.bairro-cob   ELSE c-bairro-cob ";"
           IF c-cidade-cob   = "Ok" THEN emitente.cidade-cob   ELSE c-cidade-cob ";"
           IF c-estado-cob   = "Ok" THEN emitente.estado-cob   ELSE c-estado-cob
           SKIP.

       ASSIGN i-seq = 0.
    END.
END.
OUTPUT CLOSE.

