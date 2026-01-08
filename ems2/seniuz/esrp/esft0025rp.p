/* Programa: ESFT0025.P
** Sistema.: EMS da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Exporta‡Æo de Notas com 50% de AlgodÆo.
** Autor...: Gilvando de Souza Araujo - Fevereiro/2007
** Obs.....: Especifico da TEAR TÒXTIL INDéSTRIA E COMRCIO LTDA.
**
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0025RP 2.06.00.000}

define temp-table tt-param no-undo
       field destino             as integer
       field arquivo             as char format "x(35)"
       field usuario             as char format "x(12)"
       field data-exec           as date
       field hora-exec           as integer
       FIELD cod-estabel-ini     LIKE nota-fiscal.cod-estabel
       FIELD cod-estabel-fin     LIKE nota-fiscal.cod-estabel
       FIELD serie-ini           LIKE nota-fiscal.serie
       FIELD serie-fin           LIKE nota-fiscal.serie
       FIELD dt-emissao-ini      LIKE nota-fiscal.dt-emis-nota
       FIELD dt-emissao-fin      LIKE nota-fiscal.dt-emis-nota
       FIELD arq-saida           AS CHAR FORMAT "x(45)"
       FIELD composicoes         AS CHAR
       FIELD imp-param           AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

DEFINE input parameter raw-param as raw no-undo.
DEFINE input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include para remover acentua»’o de strings */
{include/i-freeac.i}

/* defini‡Æo de vari veis  */
DEFINE var h-acomp    as handle no-undo.

define var de-tot-valor as decimal format ">>>,>>>,>>9.99".

FORM
    "*--------- Parƒmetros/Sele‡Æo ---------*"  SKIP
    tt-param.cod-estabel-ini  LABEL "Estabelecimento"
    "A"  AT 29
    tt-param.cod-estabel-fin  NO-LABELS               SKIP
    tt-param.serie-ini        LABEL "S‚rie.........."
    "A"  AT 29
    tt-param.serie-fin        NO-LABELS               SKIP
    tt-param.dt-emissao-ini   LABEL "Data EmissÆo..."
    "A"  AT 29              
    tt-param.dt-emissao-fin   NO-LABELS               SKIP
    tt-param.arq-saida        LABEL "Arquivo Sa¡da.." SKIP
    tt-param.composicoes      LABEL "Composi‡äes...." FORMAT "x(100)" SKIP(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

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
{utp/ut-liter.i Notas_Fiscais_c/Itens_compostos_p/at‚_50%_AlgodÆo * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

DEF STREAM saida.
OUTPUT STREAM saida to VALUE(tt-param.arq-saida) CONVERT SOURCE "ibm850".

PUT STREAM saida
    "Nr.Nota;Dt.EmissÆo;Cliente;CC;Composi‡Æo;Item;Descri‡Æo;Valor Item;Valor ICMS;%ICMS"
    SKIP.

FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel  >= tt-param.cod-estabel-ini
                       AND nota-fiscal.cod-estabel  <= tt-param.cod-estabel-fin
                       AND nota-fiscal.serie        >= tt-param.serie-ini
                       AND nota-fiscal.serie        <= tt-param.serie-fin
                       AND nota-fiscal.dt-emis-nota >= tt-param.dt-emissao-ini
                       AND nota-fiscal.dt-emis-nota <= tt-param.dt-emissao-fin
                       AND nota-fiscal.dt-cancela   =  ?
                     NO-LOCK,
    EACH it-nota-fisc OF nota-fiscal NO-LOCK,
    EACH item-ext WHERE item-ext.it-codigo = it-nota-fisc.it-codigo 
                    AND LOOKUP(item-ext.cod-composi,tt-param.composicoes) <> 0
                  NO-LOCK:
    
    run pi-acompanhar in h-acomp (input "Nota Fiscal: " + nota-fiscal.nr-nota-fis).

    FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK.
    FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK.
    FIND composi WHERE composi.cod-composi = item-ext.cod-composi NO-LOCK.

    PUT STREAM saida
        nota-fiscal.nr-nota-fis ";"
        nota-fiscal.dt-emis-nota ";"
        emitente.nome-emit ";"
        item-ext.cod-composi ";"
        composi.descricao ";"
        it-nota-fisc.it-codigo ";"
        ITEM.desc-item FORMAT "x(36)" ";"
        it-nota-fisc.vl-tot-item ";"
        it-nota-fisc.vl-icms-it ";"
        IF it-nota-fisc.vl-tot-item <> 0 THEN
           ROUND(it-nota-fisc.vl-icms-it / it-nota-fisc.vl-tot-item * 100, 0)
        ELSE
           0
        SKIP.
END.
OUTPUT STREAM saida CLOSE.

IF tt-param.imp-param THEN DO:
   display tt-param.cod-estabel-ini 
           tt-param.cod-estabel-fin 
           tt-param.serie-ini       
           tt-param.serie-fin      
           tt-param.dt-emissao-ini  
           tt-param.dt-emissao-fin  
           tt-param.arq-saida       
           tt-param.composicoes     
           with frame f-param.
END.
 
/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.
