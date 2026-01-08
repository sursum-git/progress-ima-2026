/* Programa: imp-his.p
** Objetivo: Importar hist¢ricos de movimento de epi. 
*/

DEF VAR i-cont-err AS INT.

DEF TEMP-TABLE tt-hist
    FIELD matr     AS CHAR
    FIELD equipto  AS CHAR
    FIELD dtentr   AS CHAR 
    FIELD dtvcto   AS CHAR
    FIELD motdevol AS CHAR
    FIELD dtdevol  AS CHAR
    FIELD preco    AS CHAR
    FIELD qtd      AS CHAR.
    
input from "c:/temp/seguranáa/hist.csv".
SET ^.

repeat:
   create tt-hist.
   import delimiter ";" tt-hist.
end.
input close.

OUTPUT TO c:/temp/lixo.txt.
FOR EACH tt-hist:
    FIND LAST funcionario WHERE funcionario.cdn_funcionario = INT(tt-hist.matr) NO-LOCK NO-ERROR.
    FIND eqpto_protec_indual WHERE eqpto_protec_indual.cdn_empresa = 3
                               AND eqpto_protec_indual.cod_epi = tt-hist.equipto NO-LOCK NO-ERROR.
    IF NOT AVAIL funcionario /*OR NOT AVAIL eqpto_protec_indual*/ THEN
    DISP tt-hist.matr
         AVAIL funcionario
        /*
         tt-hist.equipto
         AVAIL eqpto_protec_indual*/.
END.
OUTPUT CLOSE.

OUTPUT TO c:/temp/seguranáa/hist.txt.
FOR EACH tt-hist:
    FIND LAST funcionario WHERE funcionario.cdn_funcionario = INT(tt-hist.matr) NO-LOCK NO-ERROR.
    FIND eqpto_protec_indual WHERE eqpto_protec_indual.cdn_empresa = 3
                               AND eqpto_protec_indual.cod_epi = tt-hist.equipto NO-LOCK NO-ERROR.
    IF AVAIL funcionario AND AVAIL eqpto_protec_indual THEN
       PUT UNFORMAT 
           funcionario.cdn_empresa
           funcionario.cdn_estab
           funcionario.cdn_funcionario
           eqpto_protec_indual.cod_epi
           "Un        "
           tt-hist.dtentr
           tt-hist.dtdevol
           fill(" ",8)
           "000100"
           IF tt-hist.dtdevol <> "" THEN "000100" ELSE "000000"
           IF tt-hist.dtdevol <> "" THEN "S" ELSE "N"
           "000000076"
           SKIP.
END.
OUTPUT CLOSE.
/*
/* Verifica grupos de clientes */
FOR EACH tt-emitente:
    IF int(tt-emitente.cod-emitente) = 0 OR int(tt-emitente.grupo-inf) = 0 THEN NEXT.
    FIND gr-cli WHERE gr-cli.cod-gr-cli = INT(tt-emitente.grupo-inf) NO-LOCK NO-ERROR.
    IF NOT AVAIL gr-cli THEN
       ASSIGN i-cont-err = i-cont-err + 1.
END.
IF i-cont-err > 0 THEN
   MESSAGE "Foram encontrados " + TRIM(STRING(i-cont-err)) + " grupos de clientes n∆o cadastrados." SKIP
           "Acerte e rode novamente o programa."
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
ELSE DO:
   FOR EACH tt-emitente:
       IF int(tt-emitente.cod-emitente) = 0 OR int(tt-emitente.grupo-inf) = 0 THEN NEXT.
       FIND gr-cli WHERE gr-cli.cod-gr-cli = INT(tt-emitente.grupo-inf) NO-LOCK NO-ERROR.
       IF NOT AVAIL gr-cli THEN
          ASSIGN i-cont-err = i-cont-err + 1.
       FIND emitente WHERE emitente.cod-emitente = int(tt-emitente.cod-emitente).
       /*
       IF AVAIL emitente THEN
          ASSIGN emitente.cod-gr-cli = INT(tt-emitente.grupo-inf).
       */
       ACCUMULATE tt-emitente.cod-emitente(COUNT).
   END.
   DISPLAY (ACCUM COUNT tt-emitente.cod-emitente).
END.
*/
