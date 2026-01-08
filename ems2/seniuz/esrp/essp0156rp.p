/* Programa: ESSP0156.W
** Modulo..: Controle de Expediá∆o
** Objetivo: Listar Etiquetas sem reporte no EMS
** Autor...: Gilvando Souza Araujo - Maio/2007
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
*/

/* include de controle de vers∆o */
{include/i-prgvrs.i ESSP0156RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino          AS INTEGER
       FIELD arquivo          AS CHAR FORMAT "x(35)"
       FIELD usuario          AS CHAR FORMAT "x(12)"
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       FIELD classifica       AS INTEGER
       FIELD desc-classifica  AS CHAR FORMAT "x(40)"
       FIELD cod-estabel      AS CHAR
       FIELD ini-dt-emissao   LIKE ob-etiqueta.dt-emissao
       FIELD fin-dt-emissao   LIKE ob-etiqueta.dt-emissao
       FIELD lote-todos       AS LOG
       FIELD lote-pp          AS LOG
       FIELD lote-pd          AS LOG
       FIELD lote-rp          AS LOG
       FIELD lote-rd          AS LOG
       FIELD lote-sc          AS LOG
       FIELD lote-ca          AS LOG
       FIELD etq-todas        AS LOG
       FIELD impressa         AS LOG
       FIELD em-producao      AS LOG
       FIELD em-estoque       AS LOG
       FIELD reservada        AS LOG
       FIELD faturada         AS LOG
       FIELD em-reproc        AS LOG
       FIELD corte            AS LOG
       FIELD imp-param        AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

/* definiá∆o de vari†veis  */
DEF VAR h-acomp    AS HANDLE NO-UNDO.
DEF VAR c-situacao AS CHAR FORMAT "x(13)".
DEF VAR c-lotes    AS CHAR.
DEF VAR c-sit      AS CHAR.

FORM
    "*-------------- ParÉmetros/Seleá∆o --------------*" SKIP
    tt-param.ini-dt-emissao   LABEL "Data Emiss∆o" AT 1
    "a"  AT 32               
    tt-param.fin-dt-emissao   NO-LABELS
    tt-param.desc-classifica  LABEL "Classificacao" AT 1
    WITH NO-BOX SIDE-LABELS WIDTH 132 STREAM-IO FRAME f-param.

FORM
    ob-etiqueta.nr-ob                        LABEL "Numero-OB"
    ob-etiqueta.dt-emissao                   LABEL "Dt-Emiss∆o"
    c-situacao                               LABEL "Situaá∆o"
    ob-etiqueta.quantidade                   LABEL "Quantidade"
    ob-etiqueta.localizacao FORMAT "XXX/XXX" LABEL "Doca"
    ob-etiqueta.it-codigo                    LABEL "Item"
    ob-etiqueta.cod-refer                    LABEL "Referencia"
    ob-etiqueta.nr-lote                      LABEL "Lote"
    ob-etiqueta.num-etiqueta                 LABEL "Etiqueta"
    WITH NO-BOX NO-LABEL 55 DOWN WIDTH 134 STREAM-IO FRAME f-detalhe.

/* include padr∆o para output de relat¢rios */
{include/i-rpout.i}

/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i REVIS«O DE TECIDOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Etiquetas_n∆o_Reportadas_no_EMS * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).


ASSIGN c-lotes = "".
IF tt-param.lote-todos = YES THEN
   ASSIGN c-lotes = "pp pd rp rd sc ca ".
ELSE DO:
   ASSIGN c-lotes = c-lotes + IF tt-param.lote-pp = YES THEN "pp " ELSE "   ".
   ASSIGN c-lotes = c-lotes + IF tt-param.lote-pd = YES THEN "pd " ELSE "   ".
   ASSIGN c-lotes = c-lotes + IF tt-param.lote-rp = YES THEN "rp " ELSE "   ".
   ASSIGN c-lotes = c-lotes + IF tt-param.lote-rd = YES THEN "rd " ELSE "   ".
   ASSIGN c-lotes = c-lotes + IF tt-param.lote-sc = YES THEN "sc " ELSE "   ".
   ASSIGN c-lotes = c-lotes + IF tt-param.lote-ca = YES THEN "ca " ELSE "   ".
END.

ASSIGN c-sit = "".
IF tt-param.etq-todas = YES THEN
   ASSIGN c-sit = "1,2,3,4,5,6,7".
ELSE DO:
   ASSIGN c-sit = c-sit + IF tt-param.impressa    = YES THEN "1," ELSE "  ".
   ASSIGN c-sit = c-sit + IF tt-param.em-producao = YES THEN "2," ELSE "  ".
   ASSIGN c-sit = c-sit + IF tt-param.em-estoque  = YES THEN "3," ELSE "  ".
   ASSIGN c-sit = c-sit + IF tt-param.reservada   = YES THEN "4," ELSE "  ".
   ASSIGN c-sit = c-sit + IF tt-param.faturada    = YES THEN "5," ELSE "  ".
   ASSIGN c-sit = c-sit + IF tt-param.em-reproc   = YES THEN "6," ELSE "  ".
   ASSIGN c-sit = c-sit + IF tt-param.corte       = YES THEN "7," ELSE "  ".
END.

FOR EACH ob-etiqueta WHERE
         INDEX(c-sit,STRING(ob-etiqueta.situacao)) <> 0 AND
         ob-etiqueta.cod-estabel = tt-param.cod-estabel AND
         ob-etiqueta.nr-reporte = 0 AND  
         ob-etiqueta.tipo-ordem = 1 AND 
         ob-etiqueta.quantidade > 0 AND 
/*         ob-etiqueta.situacao   = 2 AND  */
         ob-etiqueta.dt-emissao >= tt-param.ini-dt-emissao AND 
         ob-etiqueta.dt-emissao <= tt-param.fin-dt-emissao AND 
         INDEX(c-lotes,ob-etiqueta.nr-lote) <> 0
         NO-LOCK
    BY IF tt-param.classifica = 1 THEN STRING(YEAR(ob-etiqueta.dt-emissao), "9999") +
                                       STRING(MONTH(ob-etiqueta.dt-emissao), "99") +
                                       STRING(DAY(ob-etiqueta.dt-emissao), "99") +
                                       ob-etiqueta.it-codigo +
                                       ob-etiqueta.cod-refer +
                                       STRING(ob-etiqueta.nr-ob) +
                                       STRING(ob-etiqueta.num-etiqueta)
                                  ELSE ob-etiqueta.it-codigo:
    
    RUN pi-acompanhar in h-acomp (input "OB: " + string(ob-etiqueta.nr-ob)).
    
    {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao} 
    
    DISPLAY ob-etiqueta.nr-ob      
            ob-etiqueta.dt-emissao 
            c-situacao   
            ob-etiqueta.quantidade
            ob-etiqueta.localizacao
            ob-etiqueta.it-codigo
            ob-etiqueta.cod-refer
            ob-etiqueta.nr-lote
            ob-etiqueta.num-etiqueta
            WITH FRAME f-detalhe.
    DOWN WITH FRAME f-detalhe.

    ACCUMULATE ob-etiqueta.quantidade (TOTAL).

END.
IF (ACCUM TOTAL ob-etiqueta.quantidade) <> 0 THEN DO: 
    DISPLAY "----------" @ ob-etiqueta.quantidade
            WITH FRAME f-detalhe.
    DOWN WITH FRAME f-detalhe.

    DISPLAY "TOTAL GERAL"  @ c-situacao      
            ACCUM TOTAL ob-etiqueta.quantidad @ ob-etiqueta.quantidade
            WITH FRAME f-detalhe.
   DOWN WITH FRAME f-detalhe.
END.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.ini-dt-emissao
           tt-param.fin-dt-emissao
           tt-param.desc-classifica
           with frame f-param.
END.
 
/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

