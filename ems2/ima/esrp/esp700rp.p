
/* Programa: ESP700rp.p
** Modulo..: Especifico
** Objetivo: Apagar os dados das tabelas WP
**           
** Autor...: Tadeu - 02/2023
*/
{include/i-prgvrs.i ESP700rp  2.06.00.001}


{esp/esp700.i}
DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.  
 
CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.
{utp/ut-glob.i}

/* defini‡Æo de vari veis  */
DEFINE VARIABLE h-acomp AS HANDLE      NO-UNDO.
{include/i-rpvar.i}   
/* bloco principal do programa */
ASSIGN  c-programa 	    = "esp700rp"
	  c-versao	    = "1.00"
	  c-revisao	    = ".00.000"
	  c-empresa    = "Grupo IMA"
	  c-sistema	    = "Datasul"
	  c-titulo-relat  = "Limpar Tabelas WP".

{include/i-rpcab.i  &stream = "str-rp"}
{include/i-rpcb80.i &stream = "str-rp"}
{include/i-rpout.i &stream = "stream str-rp"}
VIEW STREAM str-rp FRAME f-cabec.
VIEW STREAM str-rp FRAME f-rodape.


RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Apagando *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

output to value(SESSION:TEMP-DIRECTORY + "prgapagarwp" + STRING(TIME) + ".log").

RUN pi-acompanhar IN h-acomp('Tabela WP').
put substring(string(now),01,10)           format "x(10)" " as "
    substring(string(now),12,10)           format "x(08)" " - "
	"Apagando registro da tabela wp" format "x(51)"
    skip.
for each wp exclusive-lock:
    delete wp.
end.
RUN pi-acompanhar IN h-acomp('Tabela de Pedidos').
put substring(string(now),01,10)                     format "x(10)" " as "
    substring(string(now),12,10)                     format "x(08)" " - "
    "Apagando registro da tabela wp_ped_venda" format "x(51)"
    skip.
for each wp_ped_venda exclusive-lock:
    delete wp_ped_venda.
end.

RUN pi-acompanhar IN h-acomp('Tabela de NFs').
put substring(string(now),01,10)					   format "x(10)" " as "
    substring(string(now),12,10)					   format "x(08)" " - "
    "Apagando registro da tabela wp_nota_fiscal" format "x(51)"
    skip.
for each wp_nota_fiscal exclusive-lock:
    delete wp_nota_fiscal.
end.

RUN pi-acompanhar IN h-acomp('Tabela de Estoque').

put substring(string(now),01,10)						 format "x(10)" " as "
    substring(string(now),12,10)						 format "x(08)" " - "
	"Apagando registro da tabela wp_estoque_preco" format "x(51)"
    skip.
for each wp_estoque_preco exclusive-lock:
    delete wp_estoque_preco.
end.

put substring(string(now),01,10)						 format "x(10)" " as "
    substring(string(now),12,10)						 format "x(08)" " - "
	"--------------------------------------------------" format "x(51)"
    skip.

output close.

RUN pi-finalizar IN h-acomp.


{include/i-rpclo.i &stream = "stream str-rp"}

