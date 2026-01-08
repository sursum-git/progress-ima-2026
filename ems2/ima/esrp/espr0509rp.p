{esp/util.i}
{esrp/espr0509.i}
{esapi/consControlePreco.i}
/* Parametros de entrada logica obrigatoria */
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita                   AS RAW.


DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.       

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEFINE VARIABLE c-prg-obj AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-prg-vrs AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-acomp   AS HANDLE      NO-UNDO.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

{include/i-rpvar.i} 

/* include padr∆o para output de relat¢rios */
{include/i-rpout.i &STREAM="stream str-rp"}

/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
{include/i-rpcab.i &STREAM="str-rp"}


/* bloco principal do programa */
ASSIGN  c-programa 	    = "ESPR0509RP"
        c-prg-obj       = c-Programa
	    c-versao	    = "1.00"
        c-prg-vrs       = c-versao
	    c-revisao	    = ".00.000"
	    c-empresa    = "Grupo IMA"
	    c-sistema	    = "Datasul"
	    c-titulo-relat  = "Preáos por Item/Referància".


/* para n∆o visualizar cabeáalho/rodapÇ em sa°da RTF */
IF tt-param.l-habilitaRTF <> YES THEN DO:
    VIEW STREAM str-rp FRAME f-cabec.
    VIEW STREAM str-rp FRAME f-rodape.
END.




RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Buscando Preáos *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

RUN pi-acompanhar IN h-acomp("Aguarde...").



RUN esapi/consControlePreco.p(tt-param.it-codigo,
                              tt-param.cod-refer,
                              OUTPUT TABLE ttITem).

OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "preco.txt").

FOR EACH ttItem:
    EXPORT DELIMITER "|" ttItem.
END.

OUTPUT CLOSE.

//{esp/exportarTabelaCsv.i ttItem}

IF SEARCH('excel\espr0509.xlsx') <> ? THEN DO:
   OS-COMMAND SILENT VALUE('start excel /t ' + SEARCH('excel\espr0509.xlsx') ).
   PUT STREAM str-rp "Dados gerados em excel" SKIP.
END.
ELSE  DO:
  PUT STREAM str-rp "ERRO:Arquivo excel\espr0509.xlsx n∆o encontrado." SKIP.
END.









RUN pi-finalizar IN h-acomp.




/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RETURN "OK":U.
