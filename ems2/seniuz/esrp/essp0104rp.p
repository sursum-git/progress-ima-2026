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
    FIELD cod-estabel      AS CHAR
    FIELD dt-inicio        AS DATE FORMAT "99/99/9999"
    FIELD dt-termino       AS DATE FORMAT "99/99/9999".

DEFINE TEMP-TABLE tt-digita NO-UNDO
    FIELD it-codigo         AS CHAR FORMAT "x(8)"
    FIELD desc-item         AS CHAR FORMAT "x(30)" 
    FIELD qtd-produzida     AS DEC FORMAT ">>>,>>9.9999"
    FIELD qtd-real          AS DEC FORMAT ">>>,>>9.9999"
    FIELD fator             LIKE item-ext.fator
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
    FIELD qt-op-reportada LIKE movto-estoq.quantidade
    FIELD qt-op-reportar  LIKE movto-estoq.quantidade
    INDEX ordem IS PRIMARY nr-ord-prod.

DEF TEMP-TABLE tt-rateio
    FIELD nr-ord-prod     LIKE movto-estoq.nr-ord-prod
    FIELD dt-trans        LIKE movto-estoq.dt-trans
    FIELD qt-dt-reportada LIKE movto-estoq.quantidade
    FIELD qt-dt-reportar  LIKE movto-estoq.quantidade
    FIELD nr-reporte      LIKE movto-estoq.nr-reporte
    INDEX ordem2 IS PRIMARY nr-ord-prod dt-trans.

def var h-acomp as handle no-undo.
DEF VAR i-qtd-acbd AS INT.
DEF VAR de-qtd-acerto AS DEC.

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

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH tt-digita NO-LOCK.
    FOR EACH tt-ordens.
        DELETE tt-ordens.
    END.

    FOR EACH tt-rateio.
        DELETE tt-rateio.
    END.

    FOR EACH tt-movto.
        DELETE tt-movto.
    END.

    FOR EACH movto-estoq WHERE
             movto-estoq.cod-estabel = tt-param.cod-estabel AND
             movto-estoq.dt-trans   >= tt-param.dt-inicio AND
             movto-estoq.dt-trans   <= tt-param.dt-termino AND
             movto-estoq.it-codigo   = tt-digita.it-codigo AND
             movto-estoq.esp-docto   = 35 NO-LOCK 
             BREAK BY movto-estoq.dt-trans.

        FIND ord-prod WHERE 
             ord-prod.nr-ord-prod = movto-estoq.nr-ord-prod
             NO-ERROR.

        IF ord-prod.estado = 8 THEN
           ASSIGN ord-prod.estado = 6.  /* Reabre Ordem de Producao */
    
        IF NOT AVAIL ord-prod OR
           ord-prod.nr-linha = 4 THEN NEXT.

        FIND tt-rateio WHERE
             tt-rateio.nr-ord-prod = movto-estoq.nr-ord-prod AND
             tt-rateio.dt-trans = movto-estoq.dt-trans NO-ERROR.

        IF NOT AVAIL tt-rateio THEN DO.
           CREATE tt-rateio.
           ASSIGN tt-rateio.nr-ord-prod = movto-estoq.nr-ord-prod
                  tt-rateio.dt-trans = movto-estoq.dt-trans.
        END.
        IF movto-estoq.tipo-trans = 1 THEN
           ASSIGN tt-rateio.qt-dt-reportada = tt-rateio.qt-dt-reportada + movto-estoq.quantidade.
        ELSE
           ASSIGN tt-rateio.qt-dt-reportada = tt-rateio.qt-dt-reportada - movto-estoq.quantidade.

        FIND tt-ordens WHERE
             tt-ordens.nr-ord-prod = movto-estoq.nr-ord-prod 
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-ordens THEN DO.
           CREATE tt-ordens.
           ASSIGN tt-ordens.nr-ord-prod = movto-estoq.nr-ord-prod.
        END.
        IF movto-estoq.tipo-trans = 1 THEN
           ASSIGN tt-ordens.qt-op-reportada = tt-ordens.qt-op-reportada + movto-estoq.quantidade.
        ELSE
           ASSIGN tt-ordens.qt-op-reportada = tt-ordens.qt-op-reportada - movto-estoq.quantidade.
    END.

    FOR EACH tt-ordens NO-LOCK.
        /* Rateia quantiade a acertar do item entre as ordens */
        ASSIGN tt-ordens.qt-op-reportar = (tt-ordens.qt-op-reportada / tt-digita.qtd-produzida) *
                                          (tt-digita.qtd-real - tt-digita.qtd-produzida).

        FOR EACH tt-rateio WHERE
                 tt-rateio.nr-ord-prod = tt-ordens.nr-ord-prod EXCLUSIVE-LOCK.
            /* Rateia quantiade acertar da ordem entre as datas */
            ASSIGN tt-rateio.qt-dt-reportar = (tt-rateio.qt-dt-reportada / tt-ordens.qt-op-reportada) *
                                              (tt-ordens.qt-op-reportar).
        END.
    END.

    FOR EACH tt-rateio WHERE
             tt-rateio.qt-dt-reportar <> 0 NO-LOCK.
        FIND FIRST movto-estoq WHERE
                   movto-estoq.nr-ord-produ = tt-rateio.nr-ord-prod AND
                   movto-estoq.it-codigo = tt-digita.it-codigo
                   NO-LOCK NO-ERROR.

        CREATE tt-movto.
        ASSIGN tt-movto.cod-versao-integracao   = 1                      /* acrescentado*/
               tt-movto.cod-prog-orig           = "ESSP0104"             /* acrescentado*/
               tt-movto.usuario                 = tt-param.usuario       /* acrescentado*/
               tt-movto.cod-estabel             = movto-estoq.cod-estabel
               tt-movto.ct-codigo               = movto-estoq.ct-codigo
               tt-movto.sc-codigo               = movto-estoq.sc-codigo
               tt-movto.conta-contab            = movto-estoq.ct-codigo + movto-estoq.sc-codigo
               tt-movto.esp-docto               = 35
               tt-movto.tipo-trans              = IF tt-rateio.qt-dt-reportar > 0
                                                  THEN 1 ELSE 2
               tt-movto.cod-depos               = movto-estoq.cod-depos
               tt-movto.serie                   = movto-estoq.serie-docto
               tt-movto.dt-trans                = tt-rateio.dt-trans
               tt-movto.it-codigo               = movto-estoq.it-codigo       
               tt-movto.nro-docto               = movto-estoq.nro-docto
               tt-movto.nr-ord-produ            = movto-estoq.nr-ord-prod
               tt-movto.quantidade              = ABS(tt-rateio.qt-dt-reportar)
               tt-movto.un                      = movto-estoq.un.
    END.

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
    ELSE DO.
        /*
        FOR EACH tt-rateio WHERE
                 tt-rateio.nr-reporte <> 0 NO-LOCK.

            FIND ob-etiqueta WHERE
                 ob-etiqueta.nr-reporte = tt-rateio.nr-reporte
                 NO-LOCK NO-ERROR.

            FIND item WHERE
                 item.it-codigo = ob-etiqueta.it-codigo NO-LOCK.
            
            IF NOT AVAIL ITEM OR
               ITEM.peso-liquido = 0 THEN NEXT.

            ASSIGN de-qtd-acerto = tt-rateio.qt-dt-reportar / item.peso-liquido.
                 
            ASSIGN i-qtd-acbd = 0.
            FOR EACH mov-est-acbd WHERE
                     mov-est-acbd.data-mov = ob-etiqueta.dt-emissao AND
                     mov-est-acbd.num-lote = ob-etiqueta.nr-ob AND
                     mov-est-acbd.nr-carro = ob-etiqueta.nr-carro AND
                     mov-est-acbd.acondic  = ob-etiqueta.acondic AND
                     mov-est-acbd.nr-sequencia = ob-etiqueta.nr-sequencia AND
                     mov-est-acbd.classif = "RT" NO-LOCK.
                ASSIGN i-qtd-acbd = i-qtd-acbd + 1.
            END.

            FOR EACH mov-est-acbd WHERE
                     mov-est-acbd.data-mov = ob-etiqueta.dt-emissao AND
                     mov-est-acbd.num-lote = ob-etiqueta.nr-ob AND
                     mov-est-acbd.nr-carro = ob-etiqueta.nr-carro AND
                     mov-est-acbd.acondic  = ob-etiqueta.acondic AND
                     mov-est-acbd.nr-sequencia = ob-etiqueta.nr-sequencia AND
                     mov-est-acbd.classif = "RT" NO-LOCK.
                ASSIGN mov-est-acbd.qtd-defeit = mov-est-acbd.qtd-defeit +
                                                 (de-qtd-acerto / i-qtd-acbd).
            END.
        END.
        */
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

