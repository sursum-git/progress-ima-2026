/* Programa: ESPD004rpV2.p
** Modulo..: Fat Repres
** Objetivo: 
Gerar dados para analises de faturamento em diversas tabelas sumarizadas

** Autor...: Tadeu Silva Parreiras - FEV/2022
**
01/2023 - tadeu - acrescimo da tabela fats_repres_cliente_prod_data 
                - acrescimo dos campos dt_ini_mes e dt_fim_mes nas tabela que tem os campos ano e mes  
02/2024 - acrescimo da tabelas fats_04 para armazenamento de quantidade e pre‡o totais para analise de pre‡o m‚dio por 
produto, referencia, tabela, estab e data
03/2024 - inclusÆo dos valores de devolu‡Æo da tabela fats_04      
05/2024 - versÆo v2 com as seguintes altera‡äes:
        tsp1- inclusÆo da tabela fats_05
        tsp2- modifica‡Æo do processo de dele‡Æo para depois do processo de carga
        tsp3- inclusÆo de procedimento que salva as transa‡äes que precisam ser apagadas 
        tsp4- modifica‡Æo das transa‡äes salvas para situa‡Æo de deletada
        tsp5- inclusÆo do controle de atualiza‡Æo a partir da tabela fats_99           
*/

/* Parametros de entrada logica obrigatoria */
{esp/util.i}
{esinc/espd004a.i}
{utp/ut-glob.i}
{include/i-prgvrs.i espd004Arp 1.00.00.000}


DEFINE TEMP-TABLE ttDatas NO-UNDO LIKE fats_99.

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita                   AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.  


DEFINE VARIABLE hBoMsg          AS HANDLE      NO-UNDO.
DEFINE VARIABLE cErros          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEspd004aut     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-acomp         AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoAtuFats      AS HANDLE      NO-UNDO.
DEFINE VARIABLE hParam          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoFats99       AS HANDLE      NO-UNDO.
DEFINE VARIABLE idTransacao     AS INT64       NO-UNDO.
RUN utp\ut-acomp.p PERSISTENT SET h-acomp.

RUN pi-inicializar IN h-acomp("Buscando Dados...").

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.






{include/i-rpvar.i}   
{include/i-rpout.i &STREAM="stream str-rp"}
/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i &STREAM="str-rp"}


/* bloco principal do programa */
ASSIGN  c-programa 	    = "espd004Arp.p"
	    c-versao	    = "1.00"
	    c-revisao	    = ".00.000"
	    c-empresa       = "Grupo IMA"
	    c-sistema	    = "TOTVS 12"
	    c-titulo-relat  = "Relat¢rio - Atualiza‡Æo de Tabelas Customizadas de Faturamento ".

/* para nÆo visualizar cabe‡alho/rodap‚ em sa¡da RTF */
IF tt-param.l-habilitaRTF <> YES THEN DO:
    VIEW STREAM str-rp FRAME f-cabec.
    VIEW STREAM str-rp FRAME f-rodape.
END.

RUN esbo/boConsParam.p PERSIST SET hParam.
RUN getEspd004aAut IN hParam(OUTPUT cEspd004aut).
IF cEspd004Aut <> '1' AND i-num-ped-exec-rpw > 0 THEN DO:
   PUT STREAM str-rp UNFORM "Parƒmetro De Execu‡Æo Automatica Desligado(espd004a_automatico) ." SKIP.    
END.

IF tt-param.logDtPends  THEN DO:
   PUT STREAM str-rp UNFORM "Selecionada a atualiza‡Æo de datas Pendentes" SKIP.
   PUT STREAM str-rp  UNFORM "In¡cio:" STRING(NOW) SKIP.
   RUN pi-acompanhar IN h-acomp('Atualizando Datas Pendentes').
   RUN  esbo/boFats99.P PERSIST SET hBoFats99.
   RUN iniciar           IN hBoFats99.
   RUN getDatasPendentes IN hBoFats99('faturamento,devolucao,meta', OUTPUT table ttDatas) .
   FOR EACH ttDatas:
       RUN esbo/boAtuFats.p PERSIST SET hBoAtuFats .
       RUN iniciar IN hBoAtuFats.
       RUN setIdPedidoExecucao IN hBoAtuFats(i-num-ped-exec-rpw).
       RUN pi-acompanhar IN h-acomp ('Data:' + string(ttDatas.data) ).
       RUN setId IN hBoFats99(ttDatas.fat_99_id).
       RUN setSituacao IN hBoFats99(1, //situacao
                             0 //transacao
                             ).

        //procedures internas desta BO
        RUN setInterValDatas IN hBoAtuFats(ttDatas.data,ttDatas.data).
        RUN exec             IN hBoAtuFats.
        //fim
        RUN getIdTransacao  IN  hBoAtuFats(OUTPUT idTransacao).
        RUN setSituacao      IN hBoFats99(2, //situacao
                                        idTransacao //transacao
                                         ).
        RUN finalizar        IN hBoAtuFats.

    END.
    RUN finalizar IN hBoFats99.

   PUT STREAM str-rp  UNFORM "Fim:" STRING(NOW) SKIP.

END.              
ELSE DO:
    PUT STREAM str-rp  UNFORM "Selecionado o intervalo de datas:"  tt-param.dtInicial  " at‚ "  tt-param.dtFinal SKIP.
    PUT STREAM str-rp  UNFORM "In¡cio:" STRING(NOW) SKIP.
    RUN esbo/boAtuFats.p PERSIST SET hBoAtuFats .
    RUN iniciar IN hBoAtuFats.
    RUN setIdPedidoExecucao IN hBoAtuFats(i-num-ped-exec-rpw).
    RUN pi-acompanhar IN h-acomp('Atualiz. Dt:' + STRING(tt-param.dtInicial) + " At‚:" + STRING(tt-param.dtFinal)).
    RUN setIntervalDatas IN hBoAtuFats(tt-param.dtInicial,tt-param.dtFinal).
    RUN exec             IN hBoAtuFats.
    RUN getHBoMsg        IN hBoAtuFats(OUTPUT hBoMsg).
    RUN getErros         IN hBoMsg(OUTPUT cErros).
    IF cErros <> '' THEN DO:
       PUT STREAM str-rp UNFORM "Erros:" cErros SKIP.
    END.
    PUT STREAM str-rp  UNFORM "Fim:" STRING(NOW) SKIP.
    RUN finalizar        IN hBoAtuFats.


END.
RUN pi-finalizar IN h-acomp.




/*fechamento do output do relat¢rio*/
{include/i-rpclo.i &STREAM="stream str-rp"}


RETURN "OK":U.
