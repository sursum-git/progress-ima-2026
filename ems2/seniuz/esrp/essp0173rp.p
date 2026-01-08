/* Programa: ESSP0173.P
** Sistema.: EMS 2.04 da DATASUL S/A.
** Modulo..: Obrigaá‰es Fiscais
** Objetivo: Acerto de Dados gerados pelo Sistema da Avanáo, para o SINTEGRA.
** Autor...: Gilvando de Souza Araujo - Fevereiro/2008
** Obs.....: Especifico da TEAR T“XTIL INDÈSTRIA E COMêRCIO LTDA.
**
*/

/* include de controle de vers∆o */
{include/i-prgvrs.i ESSP0173RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino      as integer
       field arquivo      as char format "x(35)"
       field usuario      as char format "x(12)"
       field data-exec    as date
       field hora-exec    as integer
       FIELD arq-ent      AS CHAR FORMAT "x(45)"
       FIELD arq-sai      AS CHAR FORMAT "x(45)"
       FIELD imp-param    AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

DEFINE input parameter raw-param as raw no-undo.
DEFINE input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

DEF VAR c-mascara AS CHAR.
DEF VAR c-item    LIKE ITEM.it-codigo.
DEF VAR c-aux     LIKE ITEM.it-codigo.
DEF VAR l-erro    AS LOG.
DEF VAR l-reg_60  AS LOG.
DEF VAR i-cont-50 AS INT.
DEF VAR i-cont-51 AS INT.
DEF VAR i-cont-54 AS INT.
DEF VAR i-cont-ger AS INT.
DEF VAR l-delete  AS LOG.

DEF TEMP-TABLE tt-sintegra
    FIELD registro AS CHAR.

DEFINE BUFFER b-tt-sintegra FOR tt-sintegra.

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

/* definiá∆o de vari†veis  */
DEFINE var h-acomp    as handle no-undo.

DEF STREAM entrada.
DEF STREAM saida.
DEF STREAM lixo.

form
    "*------- ParÉmetros/Seleá∆o -------*" SKIP
    tt-param.arq-ent LABEL "Arq.Entrada" AT 1
    tt-param.arq-sai LABEL "Arq.Sa°da.." AT 1
    with no-box side-labels width 132 stream-io frame f-param.

/* include padr∆o para output de relat¢rios */
{include/i-rpout.i}

/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").
{utp/ut-liter.i OBRIGACOES_FISCAIS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Acerto_de_Dados_da_Avanáo_para_SINTEGRA * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

INPUT STREAM entrada FROM VALUE(tt-param.arq-ent).
REPEAT:
    CREATE tt-sintegra.
    IMPORT STREAM entrada DELIMITER "#&@" tt-sintegra.
END.

OUTPUT STREAM saida TO value(tt-param.arq-sai).
OUTPUT STREAM lixo TO c:/Temp/erros_sintegra.txt.

FIND FIRST tt-sintegra NO-ERROR.
IF AVAIL tt-sintegra THEN
   IF  tt-sintegra.registro BEGINS "60" THEN
       ASSIGN l-reg_60 = YES.

/* --- Elminaá∆o dos registros digitados pela Carmen --- */
FOR EACH tt-sintegra:
    ASSIGN l-delete = NO.
    IF tt-sintegra.registro BEGINS "50" THEN DO:
       IF SUBSTR(tt-sintegra.registro,43,3)  = "CF " OR
          SUBSTR(tt-sintegra.registro,43,3)  = "ECF" OR
          SUBSTR(tt-sintegra.registro,43,3)  = "FC " OR
          SUBSTR(tt-sintegra.registro,43,3)  = "EFC" OR 
          SUBSTR(tt-sintegra.registro,43,3)  = "CT " THEN
          ASSIGN l-delete = YES.
    END.
    IF tt-sintegra.registro BEGINS "51" THEN DO:
       IF SUBSTR(tt-sintegra.registro,41,3)  = "CF " OR
          SUBSTR(tt-sintegra.registro,41,3)  = "ECF" OR
          SUBSTR(tt-sintegra.registro,41,3)  = "FC " OR
          SUBSTR(tt-sintegra.registro,41,3)  = "EFC" OR
          SUBSTR(tt-sintegra.registro,41,3)  = "CT " THEN
          ASSIGN l-delete = YES.
    END.
    IF tt-sintegra.registro BEGINS "54" THEN DO:
       IF SUBSTR(tt-sintegra.registro,19,3)  = "CF " OR
          SUBSTR(tt-sintegra.registro,19,3)  = "ECF" OR
          SUBSTR(tt-sintegra.registro,19,3)  = "FC " OR
          SUBSTR(tt-sintegra.registro,19,3)  = "EFC" OR
          SUBSTR(tt-sintegra.registro,19,3)  = "CT " THEN
          ASSIGN l-delete = YES.
    END.
    IF l-delete THEN
       DELETE tt-sintegra.
END.

/* --- Elminaá∆o dos registros 75 sem correspondentes 54,60D,60I,74,77,86 --- */
FOR EACH tt-sintegra WHERE tt-sintegra.registro BEGINS "75":
    ASSIGN l-delete = NO.

    FIND FIRST b-tt-sintegra 
         WHERE (b-tt-sintegra.registro BEGINS "54"  AND SUBSTR(b-tt-sintegra.registro,19,14) = SUBSTR(tt-sintegra.registro,19,14)) OR
               (b-tt-sintegra.registro BEGINS "60D" AND SUBSTR(b-tt-sintegra.registro,32,14) = SUBSTR(tt-sintegra.registro,19,14)) OR
               (b-tt-sintegra.registro BEGINS "60I" AND SUBSTR(b-tt-sintegra.registro,43,14) = SUBSTR(tt-sintegra.registro,19,14)) OR
               (b-tt-sintegra.registro BEGINS "60R" AND SUBSTR(b-tt-sintegra.registro,10,14) = SUBSTR(tt-sintegra.registro,19,14)) OR
               (b-tt-sintegra.registro BEGINS "74"  AND SUBSTR(b-tt-sintegra.registro,19,14) = SUBSTR(tt-sintegra.registro,19,14)) OR
               (b-tt-sintegra.registro BEGINS "77"  AND SUBSTR(b-tt-sintegra.registro,19,14) = SUBSTR(tt-sintegra.registro,19,14)) OR
               (b-tt-sintegra.registro BEGINS "86"  AND SUBSTR(b-tt-sintegra.registro,19,14) = SUBSTR(tt-sintegra.registro,19,14))
         NO-LOCK NO-ERROR.
         IF NOT AVAIL b-tt-sintegra THEN
            ASSIGN l-delete = YES.
    IF l-delete THEN
      /* DELETE tt-sintegra. - Comentado porque n∆o ficou 100% - Gilvando */
END.

/* --- Acerto das quantidades de registros --- */
FOR EACH tt-sintegra:
    IF tt-sintegra.registro BEGINS "50" THEN
       ASSIGN i-cont-50 = i-cont-50 + 1.
    IF tt-sintegra.registro BEGINS "51" THEN
       ASSIGN i-cont-51 = i-cont-51 + 1.
    IF tt-sintegra.registro BEGINS "54" THEN
       ASSIGN i-cont-54 = i-cont-54 + 1.
    
    ASSIGN i-cont-ger = i-cont-ger + 1.
    
    IF tt-sintegra.registro BEGINS "90" THEN DO:
       IF SUBSTR(tt-sintegra.registro,31,2)   = "50" THEN
          OVERLAY(tt-sintegra.registro,33,8)  = STRING(i-cont-50,"99999999").
       IF SUBSTR(tt-sintegra.registro,41,2)   = "50" THEN
          OVERLAY(tt-sintegra.registro,43,8)  = STRING(i-cont-50,"99999999").
       IF SUBSTR(tt-sintegra.registro,51,2)   = "50" THEN
          OVERLAY(tt-sintegra.registro,53,8)  = STRING(i-cont-50,"99999999").
       IF SUBSTR(tt-sintegra.registro,61,2)   = "50" THEN
          OVERLAY(tt-sintegra.registro,63,8)  = STRING(i-cont-50,"99999999").
       IF SUBSTR(tt-sintegra.registro,71,2)   = "50" THEN
          OVERLAY(tt-sintegra.registro,73,8)  = STRING(i-cont-50,"99999999").
       IF SUBSTR(tt-sintegra.registro,81,2)   = "50" THEN
          OVERLAY(tt-sintegra.registro,83,8)  = STRING(i-cont-50,"99999999").
       IF SUBSTR(tt-sintegra.registro,91,2)   = "50" THEN
          OVERLAY(tt-sintegra.registro,93,8)  = STRING(i-cont-50,"99999999").
       IF SUBSTR(tt-sintegra.registro,101,2)  = "50" THEN
          OVERLAY(tt-sintegra.registro,103,8) = STRING(i-cont-50,"99999999").
       IF SUBSTR(tt-sintegra.registro,111,2)  = "50" THEN
          OVERLAY(tt-sintegra.registro,113,8) = STRING(i-cont-50,"99999999").

       IF SUBSTR(tt-sintegra.registro,31,2)   = "51" THEN
          OVERLAY(tt-sintegra.registro,33,8)  = STRING(i-cont-51,"99999999").
       IF SUBSTR(tt-sintegra.registro,41,2)   = "51" THEN
          OVERLAY(tt-sintegra.registro,43,8)  = STRING(i-cont-51,"99999999").
       IF SUBSTR(tt-sintegra.registro,51,2)   = "51" THEN
          OVERLAY(tt-sintegra.registro,53,8)  = STRING(i-cont-51,"99999999").
       IF SUBSTR(tt-sintegra.registro,61,2)   = "51" THEN
          OVERLAY(tt-sintegra.registro,63,8)  = STRING(i-cont-51,"99999999").
       IF SUBSTR(tt-sintegra.registro,71,2)   = "51" THEN
          OVERLAY(tt-sintegra.registro,73,8)  = STRING(i-cont-51,"99999999").
       IF SUBSTR(tt-sintegra.registro,81,2)   = "51" THEN
          OVERLAY(tt-sintegra.registro,83,8)  = STRING(i-cont-51,"99999999").
       IF SUBSTR(tt-sintegra.registro,91,2)   = "51" THEN
          OVERLAY(tt-sintegra.registro,93,8)  = STRING(i-cont-51,"99999999").
       IF SUBSTR(tt-sintegra.registro,101,2)  = "51" THEN
          OVERLAY(tt-sintegra.registro,103,8) = STRING(i-cont-51,"99999999").
       IF SUBSTR(tt-sintegra.registro,111,2)  = "51" THEN
          OVERLAY(tt-sintegra.registro,113,8) = STRING(i-cont-51,"99999999").

       IF SUBSTR(tt-sintegra.registro,31,2)   = "54" THEN
          OVERLAY(tt-sintegra.registro,33,8)  = STRING(i-cont-54,"99999999").
       IF SUBSTR(tt-sintegra.registro,41,2)   = "54" THEN
          OVERLAY(tt-sintegra.registro,43,8)  = STRING(i-cont-54,"99999999").
       IF SUBSTR(tt-sintegra.registro,51,2)   = "54" THEN
          OVERLAY(tt-sintegra.registro,53,8)  = STRING(i-cont-54,"99999999").
       IF SUBSTR(tt-sintegra.registro,61,2)   = "54" THEN
          OVERLAY(tt-sintegra.registro,63,8)  = STRING(i-cont-54,"99999999").
       IF SUBSTR(tt-sintegra.registro,71,2)   = "54" THEN
          OVERLAY(tt-sintegra.registro,73,8)  = STRING(i-cont-54,"99999999").
       IF SUBSTR(tt-sintegra.registro,81,2)   = "54" THEN
          OVERLAY(tt-sintegra.registro,83,8)  = STRING(i-cont-54,"99999999").
       IF SUBSTR(tt-sintegra.registro,91,2)   = "54" THEN
          OVERLAY(tt-sintegra.registro,93,8)  = STRING(i-cont-54,"99999999").
       IF SUBSTR(tt-sintegra.registro,101,2)  = "54" THEN
          OVERLAY(tt-sintegra.registro,103,8) = STRING(i-cont-54,"99999999").
       IF SUBSTR(tt-sintegra.registro,111,2)  = "54" THEN
          OVERLAY(tt-sintegra.registro,113,8) = STRING(i-cont-54,"99999999").

       IF SUBSTR(tt-sintegra.registro,31,2)   = "99" THEN
          OVERLAY(tt-sintegra.registro,33,8)  = STRING(i-cont-ger,"99999999").
       IF SUBSTR(tt-sintegra.registro,41,2)   = "99" THEN
          OVERLAY(tt-sintegra.registro,43,8)  = STRING(i-cont-ger,"99999999").
       IF SUBSTR(tt-sintegra.registro,51,2)   = "99" THEN
          OVERLAY(tt-sintegra.registro,53,8)  = STRING(i-cont-ger,"99999999").
       IF SUBSTR(tt-sintegra.registro,61,2)   = "99" THEN
          OVERLAY(tt-sintegra.registro,63,8)  = STRING(i-cont-ger,"99999999").
       IF SUBSTR(tt-sintegra.registro,71,2)   = "99" THEN
          OVERLAY(tt-sintegra.registro,73,8)  = STRING(i-cont-ger,"99999999").
       IF SUBSTR(tt-sintegra.registro,81,2)   = "99" THEN
          OVERLAY(tt-sintegra.registro,83,8)  = STRING(i-cont-ger,"99999999").
       IF SUBSTR(tt-sintegra.registro,91,2)   = "99" THEN
          OVERLAY(tt-sintegra.registro,93,8)  = STRING(i-cont-ger,"99999999").
       IF SUBSTR(tt-sintegra.registro,101,2)  = "99" THEN
          OVERLAY(tt-sintegra.registro,103,8) = STRING(i-cont-ger,"99999999").
       IF SUBSTR(tt-sintegra.registro,111,2)  = "99" THEN
          OVERLAY(tt-sintegra.registro,113,8) = STRING(i-cont-ger,"99999999").
    END.
END.

FOR EACH tt-sintegra:
    IF tt-sintegra.registro = "" THEN NEXT.

    IF tt-sintegra.registro BEGINS "50" THEN DO:
       IF SUBSTR(tt-sintegra.registro,43,3)  = "L  " THEN
          OVERLAY(tt-sintegra.registro,43,3) = "   ".
       IF SUBSTR(tt-sintegra.registro,43,3)  = "CF " THEN
          OVERLAY(tt-sintegra.registro,43,3) = "1  ".
       IF SUBSTR(tt-sintegra.registro,43,3)  = "ECF" THEN
          OVERLAY(tt-sintegra.registro,43,3) = "2  ".
       IF SUBSTR(tt-sintegra.registro,43,3)  = "FC " THEN
          OVERLAY(tt-sintegra.registro,43,3) = "3  ".
       IF SUBSTR(tt-sintegra.registro,43,3)  = "EFC" THEN
          OVERLAY(tt-sintegra.registro,43,3) = "4  ".
       IF SUBSTR(tt-sintegra.registro,43,3)  = "CT " THEN
          OVERLAY(tt-sintegra.registro,43,3) = "5  ".
       IF SUBSTR(tt-sintegra.registro,43,3)  = "CR " THEN
          OVERLAY(tt-sintegra.registro,43,3) = "6  ".
       IF SUBSTR(tt-sintegra.registro,43,3)  = "CMR" THEN
          OVERLAY(tt-sintegra.registro,43,3) = "7  ".
    END.
    IF tt-sintegra.registro BEGINS "51" THEN DO:
       IF SUBSTR(tt-sintegra.registro,41,3)  = "L  " THEN
          OVERLAY(tt-sintegra.registro,41,3) = "   ".
       IF SUBSTR(tt-sintegra.registro,41,3)  = "CF " THEN
          OVERLAY(tt-sintegra.registro,41,3) = "1  ".
       IF SUBSTR(tt-sintegra.registro,41,3)  = "ECF" THEN
          OVERLAY(tt-sintegra.registro,41,3) = "2  ".
       IF SUBSTR(tt-sintegra.registro,41,3)  = "FC " THEN
          OVERLAY(tt-sintegra.registro,41,3) = "3  ".
       IF SUBSTR(tt-sintegra.registro,41,3)  = "EFC" THEN
          OVERLAY(tt-sintegra.registro,41,3) = "4  ".
       IF SUBSTR(tt-sintegra.registro,41,3)  = "CT " THEN
          OVERLAY(tt-sintegra.registro,41,3) = "5  ".
       IF SUBSTR(tt-sintegra.registro,41,3)  = "CR " THEN
          OVERLAY(tt-sintegra.registro,41,3) = "6  ".
       IF SUBSTR(tt-sintegra.registro,41,3)  = "CMR" THEN
          OVERLAY(tt-sintegra.registro,41,3) = "7  ".
    END.
    IF tt-sintegra.registro BEGINS "54" THEN DO:
       IF SUBSTR(tt-sintegra.registro,19,3)  = "L  " THEN
          OVERLAY(tt-sintegra.registro,19,3) = "   ".
       IF SUBSTR(tt-sintegra.registro,19,3)  = "CF " THEN
          OVERLAY(tt-sintegra.registro,19,3) = "1  ".
       IF SUBSTR(tt-sintegra.registro,19,3)  = "ECF" THEN
          OVERLAY(tt-sintegra.registro,19,3) = "2  ".
       IF SUBSTR(tt-sintegra.registro,19,3)  = "FC " THEN
          OVERLAY(tt-sintegra.registro,19,3) = "3  ".
       IF SUBSTR(tt-sintegra.registro,19,3)  = "EFC" THEN
          OVERLAY(tt-sintegra.registro,19,3) = "4  ".
       IF SUBSTR(tt-sintegra.registro,19,3)  = "CT " THEN
          OVERLAY(tt-sintegra.registro,19,3) = "5  ".
       IF SUBSTR(tt-sintegra.registro,19,3)  = "CR " THEN
          OVERLAY(tt-sintegra.registro,19,3) = "6  ".
       IF SUBSTR(tt-sintegra.registro,19,3)  = "CMR" THEN
          OVERLAY(tt-sintegra.registro,19,3) = "7  ".
    END.

    IF l-reg_60 THEN DO:
       IF tt-sintegra.registro BEGINS "60M" OR
          tt-sintegra.registro BEGINS "60A" OR
          tt-sintegra.registro BEGINS "60D" OR
          tt-sintegra.registro BEGINS "60I" THEN DO:
          OVERLAY(tt-sintegra.registro,4,8) = SUBSTR(tt-sintegra.registro,10,2) +
                                              SUBSTR(tt-sintegra.registro,8,2) +
                                              SUBSTR(tt-sintegra.registro,4,4).
       END.
       IF tt-sintegra.registro BEGINS "60D" THEN DO:
          ASSIGN c-aux = TRIM(STRING(INT(SUBSTR(tt-sintegra.registro,32,14)))).
          OVERLAY(tt-sintegra.registro,32,14) = c-aux + FILL(" ",14 - LENGTH(c-aux)).
          IF SUBSTR(tt-sintegra.registro,32,9) = "999999999" THEN
             OVERLAY(tt-sintegra.registro,32,9) = "410580   ".
          IF SUBSTR(tt-sintegra.registro,32,8) = "99999999" THEN
             OVERLAY(tt-sintegra.registro,32,8) = "410580  ".
          IF SUBSTR(tt-sintegra.registro,32,6) = "999958" THEN
             OVERLAY(tt-sintegra.registro,32,6) = "410031".
          IF SUBSTR(tt-sintegra.registro,32,8) = "98989898" THEN
             OVERLAY(tt-sintegra.registro,32,8) = "410340  ".
          ASSIGN c-item = TRIM(STRING(INT(SUBSTR(tt-sintegra.registro,32,14)))).
       END.
       IF tt-sintegra.registro BEGINS "60I" THEN DO:
          ASSIGN c-aux = TRIM(STRING(INT(SUBSTR(tt-sintegra.registro,43,14)))).
          OVERLAY(tt-sintegra.registro,43,14) = c-aux + FILL(" ",14 - LENGTH(c-aux)).
          IF SUBSTR(tt-sintegra.registro,43,9) = "999999999" THEN
             OVERLAY(tt-sintegra.registro,43,9) = "410580   ".
          IF SUBSTR(tt-sintegra.registro,43,8) = "99999999" THEN
             OVERLAY(tt-sintegra.registro,43,8) = "410580  ".
          IF SUBSTR(tt-sintegra.registro,43,6) = "999958" THEN
             OVERLAY(tt-sintegra.registro,43,6) = "410031".
          IF SUBSTR(tt-sintegra.registro,43,8) = "98989898" THEN
             OVERLAY(tt-sintegra.registro,43,8) = "410340  ".
          ASSIGN c-item = TRIM(STRING(INT(SUBSTR(tt-sintegra.registro,43,14)))).
       END.
       IF tt-sintegra.registro BEGINS "60R" THEN DO:
          ASSIGN c-aux = TRIM(STRING(INT(SUBSTR(tt-sintegra.registro,10,14)))).
          OVERLAY(tt-sintegra.registro,10,14) = c-aux + FILL(" ",14 - LENGTH(c-aux)).
          IF SUBSTR(tt-sintegra.registro,10,9) = "999999999" THEN
             OVERLAY(tt-sintegra.registro,10,9) = "410580   ".
          IF SUBSTR(tt-sintegra.registro,10,8) = "99999999" THEN
             OVERLAY(tt-sintegra.registro,10,8) = "410580  ".
          IF SUBSTR(tt-sintegra.registro,10,6) = "999958" THEN
             OVERLAY(tt-sintegra.registro,10,6) = "410031".
          IF SUBSTR(tt-sintegra.registro,10,8) = "98989898" THEN
             OVERLAY(tt-sintegra.registro,10,8) = "410340  ".
          ASSIGN c-item = TRIM(STRING(INT(SUBSTR(tt-sintegra.registro,10,14)))).
       END.
       
       IF tt-sintegra.registro BEGINS "60D" OR
          tt-sintegra.registro BEGINS "60I" OR
          tt-sintegra.registro BEGINS "60R" THEN DO:
       
          FIND ITEM WHERE ITEM.it-codigo = c-item NO-LOCK NO-ERROR.
          IF NOT AVAIL ITEM THEN DO:
             PUT STREAM lixo UNFORMAT 
                 "Item: " c-item 
                 " Tipo Reg: " SUBSTR(tt-sintegra.registro,1,3)
                 SKIP.
             ASSIGN l-erro = YES.
          END.
       END.
    END.
END.

FOR EACH tt-sintegra.
    IF tt-sintegra.registro = "" THEN NEXT.
    IF tt-sintegra.registro BEGINS "75" AND l-reg_60 = YES THEN NEXT.
    ASSIGN c-mascara = "x(" + STRING(LENGTH(tt-sintegra.registro),"9999") + ")".
    PUT STREAM saida
        tt-sintegra.registro FORMAT c-mascara SKIP.
END.

INPUT STREAM entrada CLOSE.
OUTPUT STREAM saida CLOSE.
OUTPUT STREAM lixo CLOSE.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.arq-ent    
           tt-param.arq-sai    
           with frame f-param.
END.

IF l-erro THEN
   MESSAGE "H† itens n∆o cadastrados." SKIP
           "Veja o arquivo C:/Temp/erros_sintegra.txt"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
 
/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.
