/* Programa: ESSP0091RP.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Especificos
** Objetivo: Gerar o relatorio de Movimento de Estoque por Tipo de Defeito
** Autor...: Gilvando de Souza Araujo - Setembro/2004
** Obs.....: Especifico da TEAR TÒXTIL INDéSTRIA E COMRCIO LTDA.
**
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESP ESFT0091RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       FIELD data-mov-ini     LIKE mov-est-acbd.data-mov
       FIELD data-mov-fin     LIKE mov-est-acbd.data-mov
       FIELD arq-movimento    AS CHAR FORMAT "x(45)"
       FIELD tp-tecelagem     AS CHAR
       FIELD impr-param       AS LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var de-perf as dec format ">>>,>>>,>>9.99".
def var de-def  as dec format ">>>,>>>,>>9.99".
def var de-sob  as dec format ">>>,>>>,>>9.99".
def var de-ld   as dec format ">>>,>>>,>>9.99".
def var de-rt   as dec format ">>>,>>>,>>9.99".
def var de-rg   as dec format ">>>,>>>,>>9.99".

DEF STREAM saida.

form
    tt-param.data-mov-ini  LABEL "Data de Movimento" AT  1
    "a"                                              AT 31
    tt-param.data-mov-fin  NO-LABELS
    tt-param.tp-tecelagem  LABEL "Tipo Tecelagem..." FORMAT "x(50)" AT 1
    tt-param.arq-movimento LABEL "Arq.saida Todos.." AT  1
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i FATURAMENTO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Movimento_de_Estoque_por_Tipo_de_Defeito * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

output STREAM saida to value(tt-param.arq-movimento).
PUT STREAM saida 
           "Item;"
           "Refer;"
           "Perfeito;"
           "Defeituoso;"
           "Sobra;"
           "LeveDefeito;"
           "Retalho;"
           "Regular"
           SKIP.

FOR EACH mov-est-acbm WHERE mov-est-acbm.data-mov >= tt-param.data-mov-ini
                        AND mov-est-acbm.data-mov <= tt-param.data-mov-fin
                        AND INDEX(tt-param.tp-tecelagem,mov-est-acbm.tipo-tear) <> 0
                      NO-LOCK
                      BREAK BY mov-est-acbm.it-codigo
                            BY mov-est-acbm.cod-refer:

    run pi-acompanhar in h-acomp (input "Item/Refer: " + mov-est-acbm.it-codigo + " " +
                                                         mov-est-acbm.cod-refer).

    assign de-perf = de-perf + mov-est-acbm.qtd-tot-perf
           de-def  = de-def  + mov-est-acbm.qtd-tot-def
           de-sob  = de-sob  + mov-est-acbm.qtd-tot-sob.

    for each mov-est-acbd OF mov-est-acbm NO-LOCK:
        if mov-est-acbd.classific = "Rg" then
           assign de-rg = de-rg + mov-est-acbd.qtd-defeit.
        else
        if mov-est-acbd.classific = "Rt" then
           assign de-rt = de-rt + mov-est-acbd.qtd-defeit.
        else
           assign de-ld = de-ld + mov-est-acbd.qtd-defeit.
    end.    
        
    if last-of(mov-est-acbm.cod-refer) then do:
       put STREAM saida
           mov-est-acbm.it-codigo ";"
           mov-est-acbm.cod-refer ";"
           de-perf ";"
           de-def ";"
           de-sob ";"
           de-ld ";"
           de-rt ";"
           de-rg 
           SKIP.

       assign de-perf = 0
              de-def  = 0
              de-sob  = 0
              de-ld   = 0
              de-rt   = 0
              de-rg   = 0.
    end.
end.

output STREAM saida close.

IF tt-param.impr-param THEN DO:
   PAGE.
   PUT "*****----- PAR¶METROS ------*****"
       SKIP(1).

   DISPLAY tt-param.data-mov-ini    
           tt-param.data-mov-fin   
           tt-param.tp-tecelagem
           tt-param.arq-movimento
           WITH FRAME f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.



