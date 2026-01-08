/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0104RP 2.04.00.000}

{cdp/cd0666.i}
{cep/ceapi001.i}

DEFINE TEMP-TABLE tt-param no-undo
    FIELD destino          as integer
    FIELD arquivo          as char format "x(35)"
    FIELD usuario          as char format "x(12)"
    FIELD data-exec        as date
    FIELD hora-exec        as integer
    FIELD cod-estab        AS CHAR
    FIELD dt-inicio        AS DATE FORMAT "99/99/9999"
    FIELD dt-termino       AS DATE FORMAT "99/99/9999".

DEFINE TEMP-TABLE tt-digita NO-UNDO
    FIELD it-codigo         AS CHAR FORMAT "x(8)"
    FIELD desc-item         AS CHAR FORMAT "x(30)" 
    FIELD qtd-consumida     AS DEC FORMAT "->,>>>,>>9.9999"
    FIELD qtd-real          AS DEC FORMAT "->,>>>,>>9.9999"
    INDEX id it-codigo.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

FOR EACH tt-raw-digita.
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
DEF TEMP-TABLE tt-ordens
    FIELD nr-ord-prod     LIKE movto-estoq.nr-ord-prod
    FIELD qt-consumida    LIKE movto-estoq.quantidade
    FIELD qt-consumir     LIKE movto-estoq.quantidade
    INDEX ordem IS PRIMARY nr-ord-prod.

DEF TEMP-TABLE tt-rateio
    FIELD nr-ord-prod     LIKE movto-estoq.nr-ord-prod
    FIELD dt-trans        LIKE movto-estoq.dt-trans
    FIELD qt-consumida    LIKE movto-estoq.quantidade
    FIELD qt-consumir     LIKE movto-estoq.quantidade
    INDEX ordem2 IS PRIMARY nr-ord-prod dt-trans.

def var h-acomp as handle no-undo.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i MATERIAIS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Reservas_por_Ordem_de_Produ‡Æo/Item * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

RUN utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH tt-ordens.
    DELETE tt-ordens.
END.

FOR EACH tt-rateio.
    DELETE tt-rateio.
END.

FOR EACH tt-digita NO-LOCK.
    FOR EACH tt-ordens.
        DELETE tt-ordens.
    END.

    FOR EACH tt-rateio.
        DELETE tt-rateio.
    END.

    RUN pi-acompanhar in h-acomp (input tt-digita.it-codigo).

    FOR EACH movto-estoq WHERE
             movto-estoq.dt-trans >= tt-param.dt-inicio AND
             movto-estoq.dt-trans <= tt-param.dt-termino AND
             movto-estoq.it-codigo = tt-digita.it-codigo AND
             movto-estoq.cod-estabel = tt-param.cod-estab AND
            (movto-estoq.esp-docto = 28 OR
             movto-estoq.esp-docto = 31) NO-LOCK.
    
        RUN pi-acompanhar in h-acomp (input "Lendo Movimentos:" + STRING(movto-estoq.dt-trans)).

        FIND ord-prod WHERE 
             ord-prod.nr-ord-prod = movto-estoq.nr-ord-prod
             NO-ERROR.

        IF NOT AVAIL ord-prod OR
           ord-prod.nr-linha = 4 THEN NEXT.
    
        IF ord-prod.estado = 8 THEN
           ASSIGN ord-prod.estado = 6.  /* Reabre Ordem de Producao */

        /* Grava total da Data */
        FIND tt-rateio WHERE
             tt-rateio.nr-ord-prod = movto-estoq.nr-ord-prod AND
             tt-rateio.dt-trans = movto-estoq.dt-trans NO-ERROR.
    
        IF NOT AVAIL tt-rateio THEN DO.
           CREATE tt-rateio.
           ASSIGN tt-rateio.nr-ord-prod = movto-estoq.nr-ord-prod
                  tt-rateio.dt-trans = movto-estoq.dt-trans.
        END.
    
        IF movto-estoq.esp-docto = 28 THEN
           ASSIGN tt-rateio.qt-consumida = tt-rateio.qt-consumida + movto-estoq.quantidade.
        ELSE
           ASSIGN tt-rateio.qt-consumida = tt-rateio.qt-consumida - movto-estoq.quantidade.
    
        /* Grava total da Ordem */
        FIND tt-ordens WHERE
             tt-ordens.nr-ord-prod = movto-estoq.nr-ord-prod 
             NO-LOCK NO-ERROR.
    
        IF NOT AVAIL tt-ordens THEN DO.
           CREATE tt-ordens.
           ASSIGN tt-ordens.nr-ord-prod = movto-estoq.nr-ord-prod.
        END.
        IF movto-estoq.esp-docto = 28 THEN
           ASSIGN tt-ordens.qt-consumida = tt-ordens.qt-consumida + movto-estoq.quantidade.
        ELSE
           ASSIGN tt-ordens.qt-consumida = tt-ordens.qt-consumida - movto-estoq.quantidade.
    END.


    RUN pi-acompanhar in h-acomp (input "Gerando Rateio das Quantidades").
    FOR EACH tt-ordens WHERE 
             tt-ordens.qt-consumida > 0 NO-LOCK.
        /* Rateia quantiade a acertar do item entre as ordens */
        ASSIGN tt-ordens.qt-consumir = (tt-ordens.qt-consumida / tt-digita.qtd-consumida) * 
                                        ABS(tt-digita.qtd-real - tt-digita.qtd-consumida).

        FOR EACH tt-rateio WHERE
                 tt-rateio.nr-ord-prod = tt-ordens.nr-ord-prod EXCLUSIVE-LOCK.
            /* Rateia quantiade acertar da ordem entre as datas */
            ASSIGN tt-rateio.qt-consumir = (tt-rateio.qt-consumida / tt-ordens.qt-consumida) *
                                           (tt-ordens.qt-consumir).
        END.
    END.

    RUN pi-acompanhar in h-acomp (input "Imprimindo Relat¢rio").
    FOR EACH tt-rateio WHERE
             tt-rateio.qt-consumir <> 0 NO-LOCK.

        PUT tt-rateio.nr-ord-prod " " 
            tt-digita.it-codigo " " 
            tt-rateio.dt-trans " " 
            tt-rateio.qt-consumida " "
            tt-rateio.qt-consumir " "
            IF tt-digita.qtd-real - tt-digita.qtd-consumida > 0
               THEN "REQ" ELSE "RRQ"
            SKIP.
    END.

    RUN pi-acompanhar in h-acomp (input "Gerando Movimentos de Reporte").
    FOR EACH tt-rateio WHERE
             tt-rateio.qt-consumir <> 0 NO-LOCK.

        FOR EACH tt-movto.
            DELETE tt-movto.
        END.

        FIND FIRST movto-estoq WHERE
                   movto-estoq.nr-ord-produ = tt-rateio.nr-ord-prod AND
                   movto-estoq.it-codigo = tt-digita.it-codigo
                   NO-LOCK NO-ERROR.

        RUN pi-acompanhar in h-acomp (input "Reportando: " + STRING(tt-rateio.dt-trans) + " " +
                                            STRING(tt-rateio.nr-ord-prod)).

        CREATE tt-movto.
        ASSIGN tt-movto.cod-versao-integracao   = 1                          /* acrescentado*/
               tt-movto.cod-prog-orig           = "ESSP0119"                 /* acrescentado*/
               tt-movto.usuario                 = tt-param.usuario           /* acrescentado*/
               tt-movto.cod-estabel             = movto-estoq.cod-estabel
               tt-movto.ct-codigo               = movto-estoq.ct-codigo
               tt-movto.sc-codigo               = movto-estoq.sc-codigo
               tt-movto.esp-docto               = IF tt-digita.qtd-real - tt-digita.qtd-consumida > 0
                                                  THEN 28 ELSE 31
               tt-movto.tipo-trans              = IF tt-digita.qtd-real - tt-digita.qtd-consumida > 0
                                                  THEN 2 ELSE 1
               tt-movto.cod-depos               = movto-estoq.cod-depos
               tt-movto.serie                   = movto-estoq.serie-docto
               tt-movto.dt-trans                = tt-rateio.dt-trans
               tt-movto.it-codigo               = movto-estoq.it-codigo 
               tt-movto.nro-docto               = movto-estoq.nro-docto
               tt-movto.nr-ord-produ            = movto-estoq.nr-ord-prod
               tt-movto.quantidade              = ABS(tt-rateio.qt-consumir)
               tt-movto.un                      = movto-estoq.un.

        RUN cep/ceapi001.p (INPUT-OUTPUT TABLE tt-movto,
                            INPUT-OUTPUT TABLE tt-erro,
                            INPUT YES). /* Deleta erros? */

        FIND FIRST tt-erro NO-LOCK NO-ERROR.
        IF AVAIL tt-erro THEN DO.
           FOR EACH tt-erro.
               PUT "Erro ao reportar o item " tt-digita.it-codigo SKIP
                    tt-erro.mensagem
                    SKIP.
           END.
        END.
    END.
END.

FOR EACH tt-ordens.
    FIND ord-prod WHERE
         ord-prod.nr-ord-prod = tt-ordens.nr-ord-prod NO-ERROR.

    ASSIGN ord-prod.estado = 8.  /* Termina Ordem de Producao */
END.


/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

