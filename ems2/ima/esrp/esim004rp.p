/******************************************************************************
** PROGRAMA: ESIM004.P                                                       **
** DATA    : ABRIL/2018                                                      **
** AUTOR   : Toninho                                                         **
** OBJETIVO: Executar Diversas Progamas de Ajustes das Bses                  **
******************************************************************************/
DISABLE TRIGGERS FOR LOAD OF emitente.

DEF BUFFER empresa FOR mgcad.empresa.
/* Programa de controle de versao e seguran»a do Datasul EMS */

{include/i-prgvrs.i ESIM004 2.04.00.001}

/* Definicao de variaveis globais comuns a todos os programas do EMS */
{utp/ut-glob.i}

DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER
    FIELD arquivo          AS CHAR FORMAT "x(35)"
    FIELD usuario          AS CHAR FORMAT "x(12)"
    FIELD data-exec        AS DATE
    FIELD hora-exec        AS INTEGER
    FIELD classifica       AS INTEGER
    FIELD desc-classifica  AS CHAR FORMAT "x(40)"
    FIELD modelo-rtf       AS CHAR FORMAT "x(35)"
    FIELD l-habilitaRtf    AS LOG
    FIELD dt-datini        AS DATE FORMAT "99/99/9999"
    FIELD dt-datfim        AS DATE FORMAT "99/99/9999".

/* Parametros de entrada logica obrigatoria */
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita                   AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.       

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.
FIND FIRST tt-param.

DEFINE VARIABLE c-saida           AS CHARACTER NO-UNDO.

DEFINE VARIABLE i-cod-rep-ima     AS INT  INITIAL 0 NO-UNDO.
DEFINE VARIABLE i-cod-rep-med     AS INT  INITIAL 0 NO-UNDO.
DEFINE VARIABLE i-cdn_repres-100  AS INT  INITIAL 0 NO-UNDO.
DEFINE VARIABLE i-cdn_repres-500  AS INT  INITIAL 0 NO-UNDO.

DEF TEMP-TABLE tt-ped-venda
    FIELD cod-estabel  LIKE movadm.ped-venda.cod-estabel
    FIELD cod-emitente LIKE movadm.ped-venda.cod-emitente
    FIELD no-ab-reppri LIKE movadm.ped-venda.no-ab-reppri
    FIELD nr-pedcli    LIKE movadm.ped-venda.nr-pedcli
    FIELD dt-implant   LIKE movadm.ped-venda.dt-implant.


PROCEDURE pi-altera-repres.
    ASSIGN c-saida = IF OPSYS = "win32" THEN "c:\temp\" ELSE "/log/ped-venda/".
    ASSIGN c-saida = c-saida + "prgvalida_ped-venda_" + STRING(TODAY,"99-99-99") + "_" + REPLACE(STRING(TIME,"hh:mm"),":","h") + ".csv".
    
    OS-DELETE VALUE(c-saida).
    OUTPUT TO VALUE(c-saida).
    
    PUT UNFORMAT "ped-venda.cod-estabel;ped-venda.cod-emitente;ped-venda.no-ab-reppri;ped-venda.nr-pedcli;ped-venda.dt-implant;emitente.cod-emitente;emitente.nome-abrev;emitente.cod-rep;i-cod-rep-ima;i-cod-rep-med;repres.cod-rep;repres.nome-abrev;clien_financ.cod_empresa;clien_financ.cdn_cliente;clien_financ.cdn_repres;i-cdn_repres-100;i-cdn_repres-500" SKIP.
    
    FOR EACH movadm.ped-venda WHERE 
             movadm.ped-venda.no-ab-reppri <> "ima" AND
             movadm.ped-venda.cod-emitente <> 1     AND
             movadm.ped-venda.dt-implant   >= tt-param.dt-datini AND
             movadm.ped-venda.dt-implant   <= tt-param.dt-datini NO-LOCK.
    
        CREATE tt-ped-venda.
        ASSIGN tt-ped-venda.cod-estabel  = movadm.ped-venda.cod-estabel 
               tt-ped-venda.cod-emitente = movadm.ped-venda.cod-emitente
               tt-ped-venda.no-ab-reppri = movadm.ped-venda.no-ab-reppri
               tt-ped-venda.nr-pedcli    = movadm.ped-venda.nr-pedcli   
               tt-ped-venda.dt-implant   = movadm.ped-venda.dt-implant.
    END.
    
    FOR EACH dbaux.ped-venda WHERE 
             dbaux.ped-venda.no-ab-reppri <> "ima" AND
             dbaux.ped-venda.cod-emitente <> 1     AND
             dbaux.ped-venda.dt-implant   >= tt-param.dt-datini AND
             dbaux.ped-venda.dt-implant   <= tt-param.dt-datini NO-LOCK.
    
        CREATE tt-ped-venda.
        ASSIGN tt-ped-venda.cod-estabel  = dbaux.ped-venda.cod-estabel 
               tt-ped-venda.cod-emitente = dbaux.ped-venda.cod-emitente
               tt-ped-venda.no-ab-reppri = dbaux.ped-venda.no-ab-reppri
               tt-ped-venda.nr-pedcli    = dbaux.ped-venda.nr-pedcli   
               tt-ped-venda.dt-implant   = dbaux.ped-venda.dt-implant.
    END.
    
    FOR EACH tt-ped-venda NO-LOCK.
        FIND emitente WHERE
             emitente.cod-emitente = tt-ped-venda.cod-emitente SHARE-LOCK,
        FIND repres WHERE
             repres.nome-abrev = tt-ped-venda.no-ab-reppri NO-LOCK.
    
        FOR EACH clien_financ WHERE
                 clien_financ.cdn_cliente = tt-ped-venda.cod-emitente AND
                 (clien_financ.cod_empresa = "100" OR clien_financ.cod_empresa = "500") 
                 SHARE-LOCK.
    
            IF emitente.cod-rep = 1 OR 
               clien_financ.cdn_repres = 1 THEN DO:
    
               ASSIGN i-cod-rep-ima    = 0
                      i-cod-rep-med    = 0
                      i-cdn_repres-100 = 0
                      i-cdn_repres-500 = 0.
    
               IF clien_financ.cod_empresa = "100" THEN DO.
                  ASSIGN i-cdn_repres-100   = clien_financ.cdn_repres
                         i-cod-rep-ima      = emitente.cod-rep.
    
                  ASSIGN emitente.cod-rep = repres.cod-rep
                         clien_financ.cdn_repres = repres.cod-rep.
               END.
               ELSE IF clien_financ.cod_empresa = "500" THEN DO.
                  ASSIGN i-cdn_repres-500       = clien_financ.cdn_repres
                         i-cod-rep-med          = emitente.cod-rep.
    
                  ASSIGN emitente.cod-rep     = repres.cod-rep
                         clien_financ.cdn_repres = repres.cod-rep.
               END.
    
               PUT UNFORMAT
                   tt-ped-venda.cod-estabel      ";"
                   tt-ped-venda.cod-emitente     ";"
                   tt-ped-venda.no-ab-reppri     ";"
                   tt-ped-venda.nr-pedcli        ";"
                   tt-ped-venda.dt-implant       ";"
                   emitente.cod-emitente         ";"
                   emitente.nome-abrev           ";"
                   emitente.cod-rep              ";"
                   i-cod-rep-ima                 ";"
                   i-cod-rep-med                 ";"
                   repres.cod-rep                ";"
                   repres.nome-abrev             ";"
                   clien_financ.cod_empresa      ";"
                   clien_financ.cdn_cliente      ";"
                   clien_financ.cdn_repres       ";"
                   i-cdn_repres-100              ";"
                   i-cdn_repres-500
                  SKIP.
            END.
        END.
    END.
    
    OUTPUT CLOSE.
END PROCEDURE.


PROCEDURE pi-agaga-rwp.

    output to "/log/prgapagarwp.log" append.
    
    put substring(string(now),01,10)           format "x(10)" " as "
        substring(string(now),12,10)           format "x(08)" " - "
    	"Apagando registro da tabela wp" format "x(51)"
        skip.
    for each wp exclusive-lock:
        delete wp.
    end.
    
    put substring(string(now),01,10)                     format "x(10)" " as "
        substring(string(now),12,10)                     format "x(08)" " - "
        "Apagando registro da tabela wp_ped_venda" format "x(51)"
        skip.
    for each wp_ped_venda exclusive-lock:
        delete wp_ped_venda.
    end.
    
    put substring(string(now),01,10)					   format "x(10)" " as "
        substring(string(now),12,10)					   format "x(08)" " - "
        "Apagando registro da tabela wp_nota_fiscal" format "x(51)"
        skip.
    for each wp_nota_fiscal exclusive-lock:
        delete wp_nota_fiscal.
    end.
    
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
END PROCEDURE.
