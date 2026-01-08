/* include de controle de versÊo */
{include/i-prgvrs.i ES001RP 1.00.00.000}

{cdp/cd0666.i}
{cep/ceapi001.i}
 DEFINE BUFFER empresa FOR mgcad.empresa.
/* defini?Êo das temp-tables para recebimento de par?metros */
define temp-table tt-param no-undo
        field destino          as integer
        field arquivo          as char format "x(35)"
        field usuario          as char format "x(12)"
        field data-exec        as date
        field hora-exec        as integer
        field classifica       as integer
        field desc-classifica  as char format "x(40)"
        FIELD cod-estabel        AS CHAR
        FIELD cod-emitente     AS INTEGER
        FIELD serie-docto      AS CHARACTER FORMAT "x(05)"
        FIELD nro-docto        AS CHARACTER FORMAT "x(16)"
        FIELD cod-depos-sai    AS CHARACTER FORMAT "x(03)"
        FIELD cod-depos-ent    AS CHARACTER FORMAT "x(03)"
        FIELD tipo-nota        AS INTEGER.
    
def temp-table tt-raw-digita
   	field raw-digita	as raw.

DEFINE TEMP-TABLE tt-itm-erro NO-UNDO
    FIELD it-codigo AS CHARACTER FORMAT "x(16)"
    FIELD mensagem  AS CHARACTER FORMAT "x(100)".


/* recebimento de par?metros */
def input parameter raw-param as raw no-undo.
def input parameter TABLE for tt-raw-digita.

DEFINE SHARED VARIABLE c-seg-usuario AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE lg-sucesso AS LOGICAL.

FORM tt-itm-erro.it-codigo     COLUMN-LABEL "Item"
     tt-itm-erro.mensagem      COLUMN-LABEL "Mensagem de Erro"
     WITH 55 DOWN STREAM-IO WIDTH 132 NO-BOX FRAME f-detalhe.

create tt-param.
raw-transfer raw-param to tt-param.
FIND FIRST tt-param.

/* include padrÊo para vari˜veis de relat«rio  */
{include/i-rpvar.i}

/* defini?Êo de vari˜veis  */
def var h-acomp as handle no-undo.


/* defini?Êo de frames do relat«rio */

/* include padrÊo para output de relat«rios */
{include/i-rpout.i}


FIND FIRST empresa NO-LOCK.

/* bloco principal do programa */
assign c-programa     = "ES001RP"
       c-versao       = "2.06"
       c-revisao      = ".00.001"
       c-empresa      = empresa.razao-social
       c-sistema      = "Espec¡fico"
       c-titulo-relat = "Erros nas Transferˆncias".

{include/i-rpcab.i}
VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp ("Gerando Transferˆncias").

FIND FIRST param-estoq NO-LOCK NO-ERROR.

IF tt-param.tipo-nota = 1 THEN 
   RUN pi-nota-saida.
ELSE
   RUN pi-nota-entrada.

IF NOT lg-sucesso THEN DO.
   RUN pi-acompanhar IN h-acomp(INPUT "Imprimindo erros").

    FOR EACH tt-itm-erro:
        DISPLAY tt-itm-erro.it-codigo
                tt-itm-erro.mensagem
                WITH FRAME f-detalhe.
        DOWN WITH FRAME f-detalhe.
    END.
END.

/* fechamento do output do relat«rio  */
{include/i-rpclo.i}
RUN pi-finalizar in h-acomp.
RETURN "OK":U.


/* Processa Envio */

PROCEDURE pi-nota-saida.
    FIND nota-fiscal WHERE
         nota-fiscal.cod-estabel =  tt-param.cod-estabel AND 
         nota-fiscal.serie       =  tt-param.serie-docto AND
         nota-fiscal.nr-nota-fis =  tt-param.nro-docto   
         NO-LOCK NO-ERROR.

    ASSIGN lg-sucesso = YES.

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.

        RUN pi-acompanhar IN h-acomp(INPUT "Transferindo " + it-nota-fisc.it-codigo + " " + it-nota-fisc.cod-refer).

        FOR EACH tt-movto.
            DELETE tt-movto.
        END.

        FIND FIRST ITEM WHERE 
                   ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.

        GERA_MOVTO:
        DO TRANSACTION:
            CREATE tt-movto.
            ASSIGN tt-movto.cod-versao-integracao   = 1                       /* acrescentado*/
                   tt-movto.cod-prog-orig           = PROGRAM-NAME(2)         /* acrescentado*/
                   tt-movto.usuario                 = c-seg-usuario           /* acrescentado*/
                   tt-movto.i-sequen                = 1
                   tt-movto.num-sequen              = 0
                   tt-movto.num-ord-inv             = 0
                   tt-movto.tipo-valor              = 2
                   tt-movto.cod-estabel             = nota-fiscal.cod-estabel
                   tt-movto.ct-codigo               = param-estoq.ct-tr-transf
                   tt-movto.sc-codigo               = param-estoq.sc-tr-transf
                   tt-movto.conta-contabil          = param-estoq.conta-transf
                   tt-movto.esp-docto               = 33                      /*  33 = TRA  */
                   tt-movto.tipo-trans              = 2
                   tt-movto.cod-depos               = tt-param.cod-depos-sai
                   tt-movto.dt-trans                = TODAY
                   tt-movto.it-codigo               = it-nota-fisc.it-codigo
                   tt-movto.cod-refer               = it-nota-fisc.cod-refer
                   tt-movto.lote                    = it-nota-fisc.cod-refer
                   tt-movto.cod-localiz             = ""
                   tt-movto.quantidade              = it-nota-fisc.qt-faturada[1]
                   tt-movto.un                      = ITEM.un
                   tt-movto.dt-vali-lote            = DATE("31/12/9999")
                   tt-movto.descricao-db            = "Sa¡da para a NF " + nota-fiscal.nr-nota-fis + " - S‚rie " + nota-fiscal.serie + " - Emitente " + STRING(nota-fiscal.cod-estabel).


            RUN cep/ceapi001.p (INPUT-OUTPUT TABLE tt-movto,
                                INPUT-OUTPUT TABLE tt-erro,
                                INPUT YES). /* Deleta erros? */

            FIND FIRST tt-erro NO-LOCK NO-ERROR.



            IF AVAIL tt-erro THEN DO:
                ASSIGN lg-sucesso = NO.

                FOR EACH tt-erro:
                    CREATE tt-itm-erro.
                    ASSIGN tt-itm-erro.it-codigo = it-nota-fisc.it-codigo
                           tt-itm-erro.mensagem  = tt-erro.mensagem.
                END.

                UNDO GERA_MOVTO, LEAVE GERA_MOVTO.
            END.

            FOR EACH tt-movto.
                DELETE tt-movto.
            END.

            CREATE tt-movto.
            ASSIGN tt-movto.cod-versao-integracao   = 1                       /* acrescentado*/
                   tt-movto.cod-prog-orig           = PROGRAM-NAME(2)         /* acrescentado*/
                   tt-movto.usuario                 = c-seg-usuario           /* acrescentado*/
                   tt-movto.i-sequen                = 1
                   tt-movto.num-sequen              = 0
                   tt-movto.num-ord-inv             = 0
                   tt-movto.tipo-valor              = 2
                   tt-movto.cod-estabel             = nota-fiscal.cod-estabel
                   tt-movto.ct-codigo               = param-estoq.sc-tr-transf
                   tt-movto.sc-codigo               = param-estoq.sc-tr-transf
                   tt-movto.conta-contabil          = param-estoq.conta-transf
                   tt-movto.esp-docto               = 33                      /*  33 = TRA  */
                   tt-movto.tipo-trans              = 1
                   tt-movto.cod-depos               = tt-param.cod-depos-ent
                   tt-movto.dt-trans                = TODAY
                   tt-movto.it-codigo               = it-nota-fisc.it-codigo
                   tt-movto.cod-refer               = it-nota-fisc.cod-refer
                   tt-movto.lote                    = it-nota-fisc.cod-refer
                   tt-movto.cod-localiz             = ""
                   tt-movto.quantidade              = it-nota-fisc.qt-faturada[1]
                   tt-movto.un                      = ITEM.un
                   tt-movto.dt-vali-lote            = DATE("31/12/9999")
                   tt-movto.descricao-db            = "Sa¡da para a NF " + nota-fiscal.nr-nota-fis + " - S‚rie " + nota-fiscal.serie + " - Estab " + STRING(nota-fiscal.cod-estabel).

            RUN cep/ceapi001.p (INPUT-OUTPUT TABLE tt-movto,
                                INPUT-OUTPUT TABLE tt-erro,
                                INPUT YES). /* Deleta erros? */


            FIND FIRST tt-erro
                NO-LOCK NO-ERROR.

            IF AVAIL tt-erro THEN DO:
                ASSIGN lg-sucesso = NO.

                FOR EACH tt-erro:
                    CREATE tt-itm-erro.
                    ASSIGN tt-itm-erro.it-codigo = it-nota-fisc.it-codigo
                           tt-itm-erro.mensagem  = tt-erro.mensagem.
                END.

                UNDO GERA_MOVTO, LEAVE GERA_MOVTO.
            END.
        END.
    END.
END PROCEDURE.


/*Processa Entrada */

PROCEDURE pi-nota-entrada.
    FIND FIRST docum-est
        WHERE docum-est.cod-emitente = tt-param.cod-emitente AND
              docum-est.serie-docto  = tt-param.serie-docto  AND
              docum-est.nro-docto    = tt-param.nro-docto
        NO-LOCK NO-ERROR.
    
    
    ASSIGN lg-sucesso = YES.
    
    FOR EACH item-doc-est OF docum-est NO-LOCK:
    
        RUN pi-acompanhar IN h-acomp(INPUT "Transferindo " + item-doc-est.it-codigo).
    
        FOR EACH tt-movto.
            DELETE tt-movto.
        END.
    
        FIND FIRST ITEM WHERE 
                   ITEM.it-codigo = item-doc-est.it-codigo
                   NO-LOCK NO-ERROR.
    
        GERA_MOVTO:
        DO TRANSACTION:
            CREATE tt-movto.
            ASSIGN tt-movto.cod-versao-integracao   = 1                       /* acrescentado*/
                   tt-movto.cod-prog-orig           = PROGRAM-NAME(2)         /* acrescentado*/
                   tt-movto.usuario                 = c-seg-usuario           /* acrescentado*/
                   tt-movto.i-sequen                = 1
                   tt-movto.num-sequen              = 0
                   tt-movto.num-ord-inv             = 0
                   tt-movto.tipo-valor              = 2
                   tt-movto.cod-estabel             = docum-est.cod-estabel
                   tt-movto.ct-codigo               = param-estoq.sc-tr-transf
                   tt-movto.sc-codigo               = param-estoq.sc-tr-transf
                   tt-movto.conta-contabil          = param-estoq.conta-transf
                   tt-movto.esp-docto               = 33                      /*  33 = TRA  */
                   tt-movto.tipo-trans              = 2
                   tt-movto.cod-depos               = tt-param.cod-depos-sai
                   tt-movto.dt-trans                = TODAY
                   tt-movto.it-codigo               = item-doc-est.it-codigo
                   tt-movto.cod-refer               = item-doc-est.cod-refer
                   tt-movto.lote                    = item-doc-est.cod-refer
                   tt-movto.cod-localiz             = ""
                   tt-movto.quantidade              = item-doc-est.quantidade
                   tt-movto.un                      = ITEM.un
                   tt-movto.dt-vali-lote            = DATE("31/12/9999")
                   tt-movto.descricao-db            = "Sa¡da para a NF " + docum-est.nro-docto + " - S‚rie " + docum-est.serie-docto + " - Emitente " + STRING(docum-est.cod-emitente)
                   tt-movto.nro-docto               = docum-est.nro-docto.
    
            RUN cep/ceapi001.p (INPUT-OUTPUT TABLE tt-movto,
                                INPUT-OUTPUT TABLE tt-erro,
                                INPUT YES). /* Deleta erros? */
    
            FIND FIRST tt-erro NO-LOCK NO-ERROR.
    
            IF AVAIL tt-erro THEN DO:
                ASSIGN lg-sucesso = NO.
    
                FOR EACH tt-erro:
                    CREATE tt-itm-erro.
                    ASSIGN tt-itm-erro.it-codigo = item-doc-est.it-codigo
                           tt-itm-erro.mensagem  = tt-erro.mensagem.
                END.
    
                UNDO GERA_MOVTO, LEAVE GERA_MOVTO.
            END.
    
            FOR EACH tt-movto.
                DELETE tt-movto.
            END.
    
            CREATE tt-movto.
            ASSIGN tt-movto.cod-versao-integracao   = 1                       /* acrescentado*/
                   tt-movto.cod-prog-orig           = PROGRAM-NAME(2)         /* acrescentado*/
                   tt-movto.usuario                 = c-seg-usuario           /* acrescentado*/
                   tt-movto.i-sequen                = 1
                   tt-movto.num-sequen              = 0
                   tt-movto.num-ord-inv             = 0
                   tt-movto.tipo-valor              = 2
                   tt-movto.cod-estabel             = docum-est.cod-estabel
                   tt-movto.ct-codigo               = param-estoq.sc-tr-transf
                   tt-movto.sc-codigo               = param-estoq.sc-tr-transf
                   tt-movto.conta-contabil          = param-estoq.conta-transf
                   tt-movto.esp-docto               = 33                      /*  33 = TRA  */
                   tt-movto.tipo-trans              = 1
                   tt-movto.cod-depos               = tt-param.cod-depos-ent
                   tt-movto.dt-trans                = TODAY
                   tt-movto.it-codigo               = item-doc-est.it-codigo
                   tt-movto.cod-refer               = item-doc-est.cod-refer
                   tt-movto.lote                    = item-doc-est.cod-refer
                   tt-movto.cod-localiz             = ""
                   tt-movto.quantidade              = item-doc-est.quantidade
                   tt-movto.un                      = ITEM.un
                   tt-movto.dt-vali-lote            = DATE("31/12/9999")
                   tt-movto.descricao-db            = "Entrada da NF " + docum-est.nro-docto + " - S‚rie " + docum-est.serie-docto + " - Emitente " + STRING(docum-est.cod-emitente)
                   tt-movto.nro-docto               = docum-est.nro-docto.
    
            RUN cep/ceapi001.p (INPUT-OUTPUT TABLE tt-movto,
                                INPUT-OUTPUT TABLE tt-erro,
                                INPUT YES). /* Deleta erros? */
    
    
            FIND FIRST tt-erro
                NO-LOCK NO-ERROR.
    
            IF AVAIL tt-erro THEN DO:
                ASSIGN lg-sucesso = NO.
    
                FOR EACH tt-erro:
                    CREATE tt-itm-erro.
                    ASSIGN tt-itm-erro.it-codigo = item-doc-est.it-codigo
                           tt-itm-erro.mensagem  = tt-erro.mensagem.
                END.
    
                UNDO GERA_MOVTO, LEAVE GERA_MOVTO.
            END.
        END.
    END.
END PROCEDURE.
