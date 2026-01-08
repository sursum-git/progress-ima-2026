/* Programa: cep_transportador_ace.p
** Objetivo: Acerta dados de endere‡os para transportadores, para posterior importa‡Æo. 
*/

DEF VAR i-seq           AS INT.
DEF VAR i-cont          AS INT.
DEF VAR c-numero        AS CHAR.
DEF VAR c-ender-num     LIKE transporte.endereco.
DEF VAR i-cod-transp    LIKE transporte.cod-transp.
DEF var c-endereco      LIKE transporte.endereco.
DEF VAR c-bairro        LIKE transporte.bairro.  
DEF VAR c-cidade        LIKE transporte.cidade.  
DEF VAR c-estado        LIKE transporte.estado. 

DEF TEMP-TABLE tt-transporte
    FIELD cod-transp   LIKE transporte.cod-transp
    FIELD cgc          LIKE transporte.cgc
    FIELD nome         LIKE transporte.nome
    FIELD cep          LIKE transporte.cep
    FIELD endereco     LIKE transporte.endereco
    FIELD bairro       LIKE transporte.bairro
    FIELD cidade       LIKE transporte.cidade
    FIELD estado       LIKE transporte.estado
    FIELD situacao     AS CHAR.
    
input from "c:/temp/cep_val_transp_1.csv".
SET ^.

repeat:
   create tt-transporte.
   import delimiter ";" tt-transporte.
end.
input close.

ASSIGN i-seq = 0.

OUTPUT TO c:/temp/cep_val_transp_2.csv CONVERT SOURCE "ibm850".
PUT "Cod;Endereco;Bairro;Cidade;Estado" SKIP.
/*PUT "Cod;Endereco;Endereco-Anterior;Bairro;Bairro-Anterior;Cidade;Cidade-Anterior;UF;UF-Ant" SKIP.*/

FOR EACH tt-transporte.
    ASSIGN i-seq = i-seq + 1.
    IF i-seq = 1 THEN DO:
       ASSIGN i-cod-transp = tt-transporte.cod-transp.
    END.
    IF i-seq = 2 THEN DO:
       ASSIGN c-endereco = tt-transporte.endereco
              c-bairro   = tt-transporte.bairro
              c-cidade   = tt-transporte.cidade
              c-estado   = tt-transporte.estado.
    END.
    IF i-seq = 2 THEN DO:
       ASSIGN c-numero = "".
       FIND transporte WHERE transporte.cod-transp = i-cod-transp NO-LOCK.
       IF INDEX(transporte.endereco,",") <> 0 THEN
          ASSIGN c-numero = SUBSTR(transporte.endereco,INDEX(transporte.endereco,","),LENGTH(transporte.endereco) - INDEX(transporte.endereco,",") + 1).
       
       IF c-endereco <> "Ok" THEN DO:
          ASSIGN c-ender-num = UPPER(TRIM(c-endereco) + c-numero).
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

       PUT UNFORMAT 
           transporte.cod-transp ";"
       /* transporte.endereco ";" */
           IF c-endereco = "Ok" THEN transporte.endereco ELSE c-ender-num ";"
       /* transporte.bairro ";" */
           IF c-bairro   = "Ok" THEN transporte.bairro   ELSE c-bairro ";"
       /* transporte.cidade ";" */
           IF c-cidade   = "Ok" THEN transporte.cidade   ELSE c-cidade ";"
       /* transporte.estado ";" */
           IF c-estado   = "Ok" THEN transporte.estado   ELSE c-estado
           SKIP.

       ASSIGN i-seq = 0.
    END.
END.
OUTPUT CLOSE.

