/* Programa: ESPD0024.P
** Sistema.: EMS 2.04 da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Gerar o relatorio se os Desenhos possuem itens em aberto
**           em algum pedido de venda.
** Autor...: Gilvando de Souza Araujo - Maráo/2006
** Obs.....: Especifico da TEAR T“XTIL INDÈSTRIA E COMêRCIO LTDA.
*/

/* include de controle de vers∆o */
{include/i-prgvrs.i ESPD0024RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino        AS INTEGER
       FIELD arquivo        AS CHAR format "x(35)"
       FIELD usuario        AS CHAR format "x(12)"
       FIELD data-exec      AS DATE
       FIELD hora-exec      AS INTEGER
       FIELD classifica     AS INTEGER.

define temp-table tt-digita no-undo
       field desenho  AS CHAR FORMAT "x(4)"
       index id desenho.

define temp-table tt-raw-digita
       field raw-digita as raw.

def TEMP-TABLE tt-work 
    field cod-desenho AS CHAR FORMAT "x(4)"
    field situacao    AS LOG  FORMAT "Sim/N∆o"
    INDEX id cod-desenho.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

/* definiá∆o de vari†veis  */
def var h-acomp as handle no-undo.

FOR EACH tt-raw-digita.
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
    CREATE tt-work.
    ASSIGN tt-work.cod-desenho = tt-digita.desenho.
END.

form
    tt-work.cod-desenho    label "Desenho" 
    tt-work.situacao       LABEL "Ativo"
    with NO-LABEL no-box 55 down width 132 STREAM-IO frame f-detalhe.

/* include padr∆o para output de relat¢rios */
{include/i-rpout.i}

/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i PEDIDOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Situaá∆o_de_Desenhos_x_Pedidos_em_Aberto * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH tt-work:
    FIND FIRST ped-item WHERE (ped-item.cod-sit-item < 3 OR ped-item.cod-sit-item = 5)
                          AND SUBSTR(ped-item.cod-refer,3,4) = tt-work.cod-desenho
                        NO-LOCK NO-ERROR.
    ASSIGN tt-work.situacao = AVAIL ped-item.
END.

FOR EACH tt-work:
    DISPLAY tt-work.cod-desenho
            tt-work.situacao
            WITH FRAME f-detalhe.
    DOWN WITH FRAME f-detalhe.
END.

FOR EACH tt-work.
    DELETE tt-work.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.


