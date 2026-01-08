/* Programa: ESFT0032RP.P
** Autor...: Gilvando Souza Araujo
** Data....: 24/01/2004
** Observ..: Chamado pelo programa ESFT0032.W
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0032RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino         as integer
       field arquivo         as char format "x(35)"
       field usuario         as char format "x(12)"
       field data-exec       as date
       field hora-exec       as integer
       field classifica      as integer
       FIELD dt-emissao-ini  LIKE ped-venda.dt-emissao
       FIELD dt-emissao-fin  LIKE ped-venda.dt-emissao
       FIELD ge-codigo-ini   LIKE ITEM.ge-codigo
       FIELD ge-codigo-fin   LIKE ITEM.ge-codigo
       FIELD impr-param      AS   LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

def TEMP-TABLE tt-work  /* Itens sem fator de conversao */
    field it-codigo like item.it-codigo
    INDEX ch-work it-codigo.

DEF TEMP-TABLE tt-work1
    FIELD unidade    AS CHAR FORMAT "x(2)"
    FIELD quant-orig AS DEC  FORMAT ">>>,>>>,>>9.99"
    FIELD quant-conv AS DEC  FORMAT ">>>,>>>,>>9.99"
    INDEX ch-work1 unidade.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var de-quantidade    as dec.
def var de-quantidadea   as dec.
def var de-qtd-conv      as dec.
def var de-qtd-fat-d     as dec format "zz,zzz,zz9.99" label "- Faturamento".
def var de-vl-fat-d      as dec format "zzz,zzz,zz9.99".
def var de-qtd-cart-d    as dec format "zz,zzz,zz9.99" label "     Carteira".
def var de-vl-cart-d     as dec format "zzz,zzz,zz9.99".
def var de-qtd-reserva-d as dec format "zz,zzz,zz9.99" label "     Reservas".
def var de-vl-reserva-d  as dec format "zzz,zzz,zz9.99".
def var de-qtd-fat-p     as dec format "zz,zzz,zz9.99" label "- Faturamento".
def var de-vl-fat-p      as dec format "zzz,zzz,zz9.99".
def var de-qtd-cart-p    as dec format "zz,zzz,zz9.99" label "     Carteira".
def var de-vl-cart-p     as dec format "zzz,zzz,zz9.99".
def var de-qtd-cart-ac   as dec format "zz,zzz,zz9.99" label "Carteira Acumulada".
def var de-vl-cart-ac    as dec format "zzz,zzz,zz9.99".
def var de-qtd-reserva-p as dec format "zz,zzz,zz9.99" label "Reserva Acumulada".
def var de-vl-reserva-p  as dec format "zzz,zzz,zz9.99".
DEF VAR da-hoje          AS DATE FORMAT 99/99/9999 INIT TODAY.
DEF VAR l-falta-fator    AS LOG.

form
    "*--------- Parƒmetros/Sele‡Æo ----------*" SKIP
    tt-param.dt-emissao-ini  LABEL "Data de Emissao." 
    "a" AT 30
    tt-param.dt-emissao-fin  NO-LABELS SKIP
    tt-param.ge-codigo-ini   LABEL "Grupo de Estoque"
    "a" AT 30
    tt-param.ge-codigo-fin   NO-LABELS SKIP
    with no-box side-labels width 132 STREAM-IO frame f-param.

form
    "Situa‡Æo no dia ("              at  1
    da-hoje                 no-label at 18
    ")......."                       at 28
    de-qtd-fat-d                     at 37
    de-vl-fat-d             no-label at 66 skip
    de-qtd-cart-d                    at 37
    de-vl-cart-d            no-label at 66 skip
    de-qtd-reserva-d                 at 37
    de-vl-reserva-d         no-label at 66 skip(1)
    "Situa‡Æo de"                    at  1 
    tt-param.dt-emissao-ini no-label at 13
    "a"                              at 24
    tt-param.dt-emissao-fin no-label at 26 
    de-qtd-fat-p                     at 37
    de-vl-fat-p             no-label at 66 skip
    de-qtd-cart-p                    at 37
    de-vl-cart-p            no-label at 66 skip(1)
    de-qtd-cart-ac                   at 32
    de-vl-cart-ac           no-label at 66 skip
    de-qtd-reserva-p                 at 33
    de-vl-reserva-p         no-label at 66
    header
    "P e r ¡ o d o                         Descri‡Æo       Quantidade          Valor"
    with side-labels row 6 NO-BOX STREAM-IO frame f-detalhe.

FORM
   tt-work1.unidade    LABEL "Unidade"
   tt-work1.quant-orig LABEL "Quant-Original"
   tt-work1.quant-conv LABEL "Quant-Convert"
   with no-box 55 down width 132 STREAM-IO frame f-detalhe1 no-label.

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
{utp/ut-liter.i Resumo_do_Faturamento,_Carteira_e_Reserva * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each nota-fiscal where 
         (nota-fiscal.dt-emis-nota >= tt-param.dt-emissao-ini and  
          nota-fiscal.dt-emis-nota <= tt-param.dt-emissao-fin or  
          nota-fiscal.dt-emis-nota  = da-hoje) and  
          nota-fiscal.ind-sit-nota <> 4 and  
          nota-fiscal.dt-cancela    = ? and
          nota-fiscal.emite-duplic  = YES no-lock:

    run pi-acompanhar in h-acomp (input "Nota Fiscal: " + nota-fiscal.nr-nota-fis).

    IF nota-fiscal.cod-estabel <> "2" OR
       nota-fiscal.serie       <> "1" THEN NEXT.

    for each it-nota-fisc of nota-fiscal no-lock:
        find natur-oper where natur-oper.nat-operacao =
             it-nota-fisc.nat-operacao no-lock no-error.
        if not avail natur-oper
        or natur-oper.emite-duplic = no then next.

        find item where item.it-codigo = it-nota-fisc.it-codigo
             no-lock no-error.
        
        if avail item then
           if (item.ge-codigo < tt-param.ge-codigo-ini OR
               item.ge-codigo > tt-param.ge-codigo-fin) AND
               item.ge-codigo <> 13 then next.
        
        /*------ Conversao de M para Kg ------- */
        IF item.un <> "m" THEN DO:
           FIND item-ext WHERE ITEM-ext.it-codigo = it-nota-fisc.it-codigo
                         NO-LOCK NO-ERROR.
           IF AVAIL item-ext AND item-ext.fator-conv <> 0 THEN
              ASSIGN de-quantidade = it-nota-fisc.qt-faturada[1] * item-ext.fator-conv.
           ELSE DO:
              IF ITEM.peso-liquido <> 0 THEN
                 ASSIGN de-quantidade = it-nota-fisc.qt-faturada[1] * (1 / ITEM.peso-liquido).
              ELSE DO:
                 ASSIGN l-falta-fator = YES
                        de-quantidade = it-nota-fisc.qt-faturada[1].
                 FIND FIRST tt-work WHERE tt-work.it-codigo = it-nota-fisc.it-codigo
                                    NO-LOCK NO-ERROR.
                 IF NOT AVAIL tt-work THEN DO:
                    CREATE tt-work.
                    ASSIGN tt-work.it-codigo = it-nota-fisc.it-codigo.
                 END.
              END.
           END.   
        END.
        ELSE
           ASSIGN de-quantidade = it-nota-fisc.qt-faturada[1].
          
        IF nota-fiscal.dt-emis-nota >= tt-param.dt-emissao-ini AND
           nota-fiscal.dt-emis-nota <= tt-param.dt-emissao-fin THEN DO:
           FIND FIRST tt-work1 WHERE tt-work1.unidade = ITEM.un NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-work1 THEN DO:
              CREATE tt-work1.
              ASSIGN tt-work1.unidade = ITEM.un.
           END.
           ASSIGN tt-work1.quant-orig = tt-work1.quant-orig + it-nota-fisc.qt-faturada[1]
                  tt-work1.quant-conv = tt-work1.quant-conv + de-quantidade.
        END.

        if it-nota-fisc.dt-emis-nota = da-hoje then
           assign de-qtd-fat-d = de-qtd-fat-d + de-quantidade
                  de-vl-fat-d  = de-vl-fat-d  + it-nota-fisc.vl-tot-item.
        
        if  it-nota-fisc.dt-emis-nota >= tt-param.dt-emissao-ini
        and it-nota-fisc.dt-emis-nota <= tt-param.dt-emissao-fin then    
            assign de-qtd-conv  = de-qtd-conv + it-nota-fisc.qt-faturada[1]
                   de-qtd-fat-p = de-qtd-fat-p + de-quantidade
                   de-vl-fat-p  = de-vl-fat-p  + it-nota-fisc.vl-tot-item.
    end.
end.
                        

for each ped-venda where ped-venda.cod-sit-ped < 3   
                      or ped-venda.cod-sit-ped = 5
                   no-lock:
    for each ped-item of ped-venda where
                 (ped-item.cod-sit-item < 3
               or ped-item.cod-sit-item = 5) no-lock:

        run pi-acompanhar in h-acomp (input "Pedido: " + ped-venda.nr-pedcli).

        find item where item.it-codigo = ped-item.it-codigo
                        no-lock no-error.
        assign de-quantidadea = ped-item.qt-pedida -
                                ped-item.qt-pendente -
                                ped-item.qt-atendida + 
                                ped-item.qt-devolvida.
        assign de-quantidade  = de-quantidadea.

        /*------ Conversao de M para Kg ------- */
        IF item.un <> "m" THEN DO:
           FIND item-ext WHERE ITEM-ext.it-codigo = ITEM.it-codigo
                         NO-LOCK NO-ERROR.
           IF AVAIL item-ext AND item-ext.fator-conv <> 0 THEN
              assign de-quantidade = de-quantidadea * item-ext.fator-conv.
           ELSE DO:
              IF ITEM.peso-liquido <> 0 THEN
                 ASSIGN de-quantidade = de-quantidadea * (1 / ITEM.peso-liquido).
              ELSE DO:
                 ASSIGN l-falta-fator = YES
                        de-quantidade = de-quantidadea.
                 FIND FIRST tt-work WHERE
                            tt-work.it-codigo = item.it-codigo
                            NO-LOCK NO-ERROR.
                 IF NOT AVAIL tt-work THEN DO:
                    CREATE tt-work.
                    ASSIGN tt-work.it-codigo = item.it-codigo.
                 END.
              END.
           END.   
        END.
        ELSE
           ASSIGN de-quantidade = de-quantidadea.

        if  ped-venda.dt-emissao >= tt-param.dt-emissao-ini
        and ped-venda.dt-emissao <= tt-param.dt-emissao-fin then
            assign de-qtd-cart-p = de-qtd-cart-p + de-quantidade
                   de-vl-cart-p  = de-vl-cart-p + de-quantidadea * ped-item.vl-preori.
                                   
        if ped-venda.dt-emissao = da-hoje then
            assign de-qtd-cart-d = de-qtd-cart-d + de-quantidade
                   de-vl-cart-d  = de-vl-cart-d + 
                                   de-quantidadea * ped-item.vl-preori.

        assign de-qtd-cart-ac = de-qtd-cart-ac + de-quantidade
               de-vl-cart-ac  = de-vl-cart-ac + de-quantidadea *
                                                ped-item.vl-preori.
    end.
end.

for each ped-item-res where ped-item-res.faturado = no
                      no-lock:

    run pi-acompanhar in h-acomp (input "Reserva: " + ped-item-res.nr-pedcli).

    find item where item.it-codigo = ped-item-res.it-codigo
              no-lock no-error.
    if avail ITEM THEN
       if (item.ge-codigo < tt-param.ge-codigo-ini OR
           item.ge-codigo > tt-param.ge-codigo-fin) AND
           item.ge-codigo <> 13 then next.
    
    find ped-item where ped-item.nome-abrev = ped-item-res.nome-abrev
                    and ped-item.nr-pedcli  = ped-item-res.nr-pedcli
                    and ped-item.nr-seq     = ped-item-res.nr-sequencia
                    and ped-item.it-codigo  = ped-item-res.it-codigo
                    and ped-item.cod-refer  = ped-item-res.cod-refer
                        no-lock no-error.
    if not avail ped-item then next.

    assign de-quantidadea = ped-item-res.qt-pedida
           de-quantidade  = ped-item-res.qt-pedida.

    /*------ Conversao de M para Kg ------- */
    IF item.un <> "m" THEN DO:
       FIND item-ext WHERE ITEM-ext.it-codigo = ITEM.it-codigo
                     NO-LOCK NO-ERROR.
       IF AVAIL item-ext AND item-ext.fator-conv <> 0 THEN
          assign de-quantidade = de-quantidadea * item-ext.fator-conv.
       ELSE DO:
          IF ITEM.peso-liquido <> 0 THEN
             ASSIGN de-quantidade = de-quantidadea * (1 / ITEM.peso-liquido).
          ELSE DO:
             ASSIGN l-falta-fator = YES
                    de-quantidade = de-quantidadea.
             FIND FIRST tt-work WHERE
                        tt-work.it-codigo = item.it-codigo
                        NO-LOCK NO-ERROR.
             IF NOT AVAIL tt-work THEN DO:
                CREATE tt-work.
                ASSIGN tt-work.it-codigo = item.it-codigo.
             END.
          END.
       END.   
    END.
    ELSE
       ASSIGN de-quantidade = de-quantidadea.

    assign de-qtd-reserva-p = de-qtd-reserva-p + de-quantidade
           de-vl-reserva-p  = de-vl-reserva-p  + ped-item.vl-preori * de-quantidadea.

    if ped-item-res.dt-trans = da-hoje then
        assign de-qtd-reserva-d = de-qtd-reserva-d + de-quantidade
               de-vl-reserva-d  = de-vl-reserva-d  + (ped-item.vl-preori *
                                                      de-quantidadea).
end.

                          
                          
display da-hoje
        tt-param.dt-emissao-ini
        tt-param.dt-emissao-fin
        de-qtd-fat-d
        de-vl-fat-d
      /*  de-qtd-cart-d*/
      /*  de-vl-cart-d*/
      /*  de-qtd-reserva-d*/
      /*  de-vl-reserva-d*/
        de-qtd-fat-p
        de-vl-fat-p
       /* de-qtd-cart-p*/
       /* de-vl-cart-p*/
       /* de-qtd-cart-ac*/
       /* de-vl-cart-ac*/
       /* de-qtd-reserva-p*/
       /* de-vl-reserva-p*/
        with frame f-detalhe.
PUT SKIP(1).
FOR EACH tt-work1:
    DISP tt-work1.unidade
         tt-work1.quant-orig
         tt-work1.quant-conv
         WITH FRAME f-detalhe1.
    DOWN WITH FRAME f-detalhe1.
    ACCUMULATE tt-work1.quant-conv(TOTAL).
END.
DISPLAY "Total" @ tt-work1.unidade
        (ACCUM TOTAL tt-work1.quant-conv) @ tt-work1.quant-conv
        WITH FRAME f-detalhe1.

if l-falta-fator then do:
   page.
   put "Atencao ! - Ha itens sem fator de conversao e sem peso l¡quido:"
       skip(1).
   for each tt-work:
       FIND ITEM WHERE ITEM.it-codigo = tt-work.it-codigo
                 NO-LOCK NO-ERROR.
       IF AVAIL ITEM THEN
          PUT tt-work.it-codigo
              ITEM.desc-item
              SKIP.
       ELSE
          PUT tt-work.it-codigo
              SKIP.
   end.
   for each tt-work.
       delete tt-work.
   end.
end.  

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.dt-emissao-ini
           tt-param.dt-emissao-fin
           tt-param.ge-codigo-ini
           tt-param.ge-codigo-fin
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

