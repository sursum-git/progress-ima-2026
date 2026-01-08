/* Programa: ESPD041.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Pedidos
** Objetivo: Gerar o relatorio de Programacao por Periodos
** Autor...: Gilvando de Souza Araujo - Novembro/96
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL / SANTA ELISABETH
**
** Conversao para EMS 2.04:
**   Programa: ESPD041.P  =>  ESPD0011RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 09/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESPD0011RP 2.04.00.000}

def TEMP-TABLE w-work
    field it-codigo  like item.it-codigo
    field referencia like saldo-estoq.cod-refer
    INDEX ch-work it-codigo
                  referencia.

def TEMP-TABLE w-aux
    field it-codigo  like item.it-codigo
    field cod-refer  like ped-item-res.cod-refer
    field quantidade as dec
    INDEX ch-aux it-codigo
                 cod-refer.

define temp-table tt-param no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       FIELD nr-pedcli-ini     LIKE ped-item.nr-pedcli 
       FIELD nr-pedcli-fin     LIKE ped-item.nr-pedcli
       FIELD it-codigo-ini     LIKE ped-item.it-codigo
       FIELD it-codigo-fin     LIKE ped-item.it-codigo
       FIELD cod-refer-ini     LIKE saldo-estoq.cod-refer
       FIELD cod-refer-fin     LIKE saldo-estoq.cod-refer
       FIELD dt-entrega1-ini   LIKE ped-item.dt-entrega
       FIELD dt-entrega1-fin   LIKE ped-item.dt-entrega
       FIELD dt-entrega2-ini   LIKE ped-item.dt-entrega
       FIELD dt-entrega2-fin   LIKE ped-item.dt-entrega
       FIELD dt-entrega3-ini   LIKE ped-item.dt-entrega
       FIELD dt-entrega3-fin   LIKE ped-item.dt-entrega
       FIELD cond-credito      AS INT
       FIELD desc-credito      AS CHAR FORMAT "x(12)"
       FIELD so-proc-pron      AS LOG FORMAT "Sim/NÆo"
       FIELD tipo-rel          AS INT
       FIELD desc-tipo-rel     AS CHAR FORMAT "x(9)"
       FIELD impr-param        AS LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var l-passou-saldo as log.
def var l-prim-vez     as log.
def var l-prim-vez1    as log.
def var l-peca         as log.
def var l-item         as log.
def var l-rolo         as log.
def var l-imp-item      as log.
def var c-refer        as char.

def var c-desenho        as char format "x(5)".
def var c-obsoleto       as char format "x(2)".
def var c-acondic        as char format "x(4)".
def var de-estoque       as dec  format "->>,>>>,>>9".
def var de-carteira1     as dec  format "->>>>,>>9".
def var de-diferenc1     as dec  format "->>>>,>>9".
def var de-carteira2     as dec  format "->>>>,>>9".
def var de-diferenc2     as dec  format "->>>>,>>9".
def var de-carteira3     as dec  format "->>>>,>>9".
def var de-diferenc3     as dec  format "->>>>,>>9".
def var de-disponivel    as dec  format "->>,>>>,>>9".
def var i-programado     as int  format ">>>,>>9".
def var i-processo       as int  format ">>>,>>9".
def var i-pronto         as int  format ">>>,>>9".
def var de-tot-estoque   as dec  format "->>,>>>,>>9".
def var de-tot-carteira1 as dec  format "->>>>,>>9".
def var de-tot-carteira2 as dec  format "->>>>,>>9".
def var de-tot-carteira3 as dec  format "->>>>,>>9".
def var de-quantidade    as dec  format ">>>>,>>9".
def var de-reservada           like de-quantidade.
def var de-tot-reservada       like de-quantidade.
def var de-tot-reservada-ger   like de-quantidade.
def var de-tot-reservada-item  like de-quantidade.
def var de-tot-disponivel      like de-disponivel.
def var de-tot-estoque-ger     like de-estoque.
def var de-tot-estoque-item    like de-estoque.
def var de-tot-carteira-ger1   like de-carteira1.
def var de-tot-carteira-ger2   like de-carteira1.
def var de-tot-carteira-ger3   LIKE de-carteira1.
def var de-tot-carteira-item1  like de-carteira1.
def var de-tot-carteira-item2  like de-carteira1.
def var de-tot-carteira-item3  like de-carteira1.
def var de-tot-disponivel-ger  like de-disponivel.
def var de-tot-disponivel-item like de-disponivel.
def var i-tot-programado-item  like i-programado.
def var i-tot-programado-ger   like i-programado.
def var i-tot-processo-item    like i-processo.
def var i-tot-processo-ger     like i-processo.
def var i-tot-pronto-item      like i-pronto.
def var i-tot-pronto-ger       like i-pronto.

form
    "*-------------- Parƒmetros/Sele‡Æo ---------------*" SKIP
    tt-param.nr-pedcli-ini    LABEL "Pedido Cliente." 
    "a" AT 34
    tt-param.nr-pedcli-fin    NO-LABELS SKIP
    tt-param.it-codigo-ini    LABEL "Item..........."
    "a" AT 34
    tt-param.it-codigo-fin    NO-LABELS SKIP
    tt-param.dt-entrega1-ini  LABEL "1¦ Data Entrega" FORMAT "99/99/9999"             
    "a" AT 34
    tt-param.dt-entrega1-fin  NO-LABELS FORMAT "99/99/9999" SKIP 
    tt-param.dt-entrega2-ini  LABEL "2¦ Data Entrega" FORMAT "99/99/9999"          
    "a" AT 34
    tt-param.dt-entrega2-fin  NO-LABELS FORMAT "99/99/9999" SKIP
    tt-param.dt-entrega3-ini  LABEL "3¦ Data Entrega" FORMAT "99/99/9999"          
    "a" AT 34
    tt-param.dt-entrega3-fin  NO-LABELS FORMAT "99/99/9999" SKIP 
    tt-param.desc-credito     LABEL "Cr‚dito........" SKIP
    tt-param.desc-tipo-rel    LABEL "Tipo Relat¢rio."
    with no-box side-labels width 132 STREAM-IO frame f-param.

form
    c-desenho        at   1
    c-obsoleto       at   7
    c-acondic        at  10
    de-estoque       at  15
    de-carteira1     at  27
    de-diferenc1     at  37
    de-carteira2     at  47
    de-diferenc2     at  57
    de-carteira3     at  67
    de-diferenc3     at  77
    de-reservada     at  87
    i-programado     at  97
    i-processo       at 106
    i-pronto         at 114
    de-disponivel    at 122
    with no-box 55 down width 136 with STREAM-IO frame f-detalhe NO-LABEL.

form header
    "Cod.  Cd Tipo"                                                        at  1
    tt-param.dt-entrega1-ini format "99/99/99"                             at 27
    "a" tt-param.dt-entrega1-fin format "99/99/99" 
    tt-param.dt-entrega2-ini format "99/99/99"                             at 47
    "a" tt-param.dt-entrega2-fin format "99/99/99"       
    tt-param.dt-entrega3-ini format "99/99/99"                                          at 67
    "a" tt-param.dt-entrega3-fin format "99/99/99" 
    "Des/V Ob Acnd     Estoque  Carteira Diferenca  Carteira Diferenca"    at  1
    "  Carteira Diferenca Reservado Program Processo  Pronto  Disponivel"  at 66
    "----- -- ---- ----------- --------- --------- --------- ---------"    at  1
    " --------- --------- --------- ------- -------- ------- -----------"  at 66
    with no-labels no-attr-space no-box page-top width 136 1 DOWN STREAM-IO
         frame f-cab-dados.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i PEDIDOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Programa‡Æo_de_Produ‡Æo_por_Per¡odos * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.
VIEW FRAME f-cab-dados.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each w-work:
    delete w-work.
end.

assign l-peca = no
       l-rolo = no.

for each ped-item where ped-item.nr-pedcli     >= tt-param.nr-pedcli-ini
                    and ped-item.nr-pedcli     <= tt-param.nr-pedcli-fin
                    and ped-item.it-codigo     >= tt-param.it-codigo-ini
                    and ped-item.it-codigo     <= tt-param.it-codigo-fin
                    and ped-item.dt-entrega    >= tt-param.dt-entrega1-ini
                    and ped-item.dt-entrega    <= tt-param.dt-entrega3-fin
                    and (ped-item.cod-sit-item < 3 or
                         ped-item.cod-sit-item = 5)
                  no-lock,
    first ped-item-ext 
          where ped-item-ext.nome-abrev   = ped-item.nome-abrev
            and ped-item-ext.nr-pedcli    = ped-item.nr-pedcli
            and ped-item-ext.nr-sequencia = ped-item.nr-sequencia
            and ped-item-ext.it-codigo    = ped-item.it-codigo
            and ped-item-ext.cod-refer    = ped-item.cod-refer
          no-lock,
    each ped-venda where ped-venda.nome-abrev = ped-item.nome-abrev
                     and ped-venda.nr-pedcli  = ped-item.nr-pedcli
                     and ((tt-param.cond-credito = 1 AND (ped-venda.cod-sit-aval = 2 OR ped-venda.cod-sit-aval = 3)) OR
                          (tt-param.cond-credito = 2 AND (ped-venda.cod-sit-aval = 1 OR ped-venda.cod-sit-aval > 3)) OR
                          (tt-param.cond-credito = 3) OR
                          (ped-venda.cod-cond-pag > 0 AND ped-venda.cod-cond-pag < 4))
                   no-lock,
    FIRST item where item.it-codigo = ped-item.it-codigo
              no-lock
              break by item.it-codigo
                    by ped-item.cod-refer
                    by ped-item-ext.acondicionamento:
    
    run pi-acompanhar in h-acomp (input "Pedido: " + ped-item.nr-pedcli + "Item: " + ped-item.it-codigo).
    
    find item-ext where item-ext.it-codigo = item.it-codigo
                  no-lock no-error.
    find item-prog where item-prog.it-codigo = ped-item.it-codigo
                   no-lock no-error.
    if tt-param.so-proc-pron then
       if not avail item-prog
       or (item-prog.processo = 0 and
           item-prog.pronto   = 0) then next.
          
    assign de-quantidade = 0.
   
    for each ped-item-res
       where ped-item-res.it-codigo    = ped-item.it-codigo
         and ped-item-res.nr-sequencia = ped-item.nr-sequencia
         and ped-item-res.nr-pedcli    = ped-item.nr-pedcli
         and ped-item-res.nome-abrev   = ped-item.nome-abrev
       NO-LOCK:
       IF ped-item-res.faturado = no THEN
          assign de-quantidade = de-quantidade + ped-item-res.qt-pedida.
    end.

    assign c-refer = ped-item.cod-refer.

    if  ped-item.dt-entrega >= tt-param.dt-entrega1-ini
    and ped-item.dt-entrega <= tt-param.dt-entrega1-fin then do:
        assign de-carteira1 = de-carteira1 +
                              ped-item.qt-pedida -
                              ped-item.qt-atendida -
                              ped-item.qt-pendente -
                              de-quantidade
               de-tot-carteira1 = de-tot-carteira1 +
                                  ped-item.qt-pedida -
                                  ped-item.qt-atendida -
                                  ped-item.qt-pendente -
                                  de-quantidade
               de-tot-carteira-ger1 = de-tot-carteira-ger1 +
                                      ped-item.qt-pedida -
                                      ped-item.qt-atendida -
                                      ped-item.qt-pendente -
                                      de-quantidade
               de-tot-carteira-item1 = de-tot-carteira-item1 +
                                       ped-item.qt-pedida -
                                       ped-item.qt-atendida -
                                       ped-item.qt-pendente -
                                       de-quantidade.
    end.
    if  ped-item.dt-entrega >= tt-param.dt-entrega2-ini
    and ped-item.dt-entrega <= tt-param.dt-entrega2-fin then do:
        assign de-carteira2 = de-carteira2 +
                              ped-item.qt-pedida -
                              ped-item.qt-atendida -
                              ped-item.qt-pendente -
                              de-quantidade
               de-tot-carteira2 = de-tot-carteira2 +
                                  ped-item.qt-pedida -
                                  ped-item.qt-atendida -
                                  ped-item.qt-pendente -
                                  de-quantidade
               de-tot-carteira-ger2 = de-tot-carteira-ger2 +
                                      ped-item.qt-pedida -
                                      ped-item.qt-atendida -
                                      ped-item.qt-pendente -
                                      de-quantidade
               de-tot-carteira-item2 = de-tot-carteira-item2 +
                                       ped-item.qt-pedida -
                                       ped-item.qt-atendida -
                                       ped-item.qt-pendente -
                                       de-quantidade.
    end.
    if  ped-item.dt-entrega >= tt-param.dt-entrega3-ini
    and ped-item.dt-entrega <= tt-param.dt-entrega3-fin then do:
        assign de-carteira3 = de-carteira3 +
                              ped-item.qt-pedida -
                              ped-item.qt-atendida -
                              ped-item.qt-pendente -
                              de-quantidade
               de-tot-carteira3 = de-tot-carteira3 +
                                  ped-item.qt-pedida -
                                  ped-item.qt-atendida -
                                  ped-item.qt-pendente -
                                  de-quantidade
               de-tot-carteira-ger3 = de-tot-carteira-ger3 +
                                      ped-item.qt-pedida -
                                      ped-item.qt-atendida -
                                      ped-item.qt-pendente -
                                      de-quantidade
               de-tot-carteira-item3 = de-tot-carteira-item3 +
                                       ped-item.qt-pedida -
                                       ped-item.qt-atendida -
                                       ped-item.qt-pendente -
                                       de-quantidade.
    end.
    assign de-reservada         = de-reservada + de-quantidade
           de-tot-reservada     = de-tot-reservada + de-quantidade
           de-tot-reservada-ger = de-tot-reservada-ger +
                                  de-quantidade
           de-tot-reservada-item = de-tot-reservada-item +
                                   de-quantidade.
    if c-refer = "rolo" then
       assign l-rolo = yes.
    if c-refer = "peca" then
       assign l-peca = yes.
    
    for each w-aux:
        delete w-aux.
    end.

    for each ped-item-res
        where ped-item-res.it-codigo = ped-item.it-codigo
          and ped-item-res.cod-refer = c-refer
              no-lock:
        IF ped-item-res.faturado = NO THEN DO:
           find first w-aux
                where w-aux.it-codigo = ped-item-res.it-codigo
                 and w-aux.cod-refer = ped-item-res.cod-refer
                      no-error.
           if not avail w-aux then do:
              create w-aux.
              assign w-aux.it-codigo  = ped-item-res.it-codigo
                     w-aux.cod-refer  = ped-item-res.cod-refer
                     w-aux.quantidade = 0.
           end.
           assign w-aux.quantidade = w-aux.quantidade
                                  + ped-item-res.qt-pedida.
        END.
    end.

    assign l-passou-saldo = no.

    for each saldo-estoq where saldo-estoq.it-codigo = ped-item.it-codigo
                           and saldo-estoq.cod-refer = c-refer
                         no-lock:
        assign l-passou-saldo = yes.
        find first w-work where w-work.it-codigo  = saldo-estoq.it-codigo
                            and w-work.referencia = saldo-estoq.cod-refer
                          no-error.
        if avail w-work then next.
        create w-work.
        assign w-work.it-codigo  = saldo-estoq.it-codigo
               w-work.referencia = saldo-estoq.cod-refer.
        find first w-aux where w-aux.it-codigo = saldo-estoq.it-codigo
                           and w-aux.cod-refer = saldo-estoq.cod-refer
                         no-error.
        if not avail w-aux then
           assign de-quantidade = 0.
        else
           assign de-quantidade = w-aux.quantidade.
        assign de-estoque = de-estoque + saldo-estoq.qtidade-atu
                          - de-quantidade
               de-tot-estoque = de-tot-estoque + saldo-estoq.qtidade-atu
                              - de-quantidade
               de-tot-estoque-ger = de-tot-estoque-ger
                                  + saldo-estoq.qtidade-atu
                                  - de-quantidade
               de-tot-estoque-item = de-tot-estoque-item
                                  + saldo-estoq.qtidade-atu
                                  - de-quantidade.
    end.
    if l-passou-saldo = no then do:
       for each w-aux where w-aux.it-codigo = ped-item.it-codigo
                        and w-aux.cod-refer = c-refer:
           assign de-estoque = de-estoque - w-aux.quantidade.
       end.
    end.
    assign de-disponivel = de-estoque - de-carteira3.
    if ped-item-ext.acondicionamento begins "rolo" then
      assign c-acondic = "R" + substr(ped-item-ext.acondicionamento,6,3).
    else
    if ped-item-ext.acondicionamento begins "peca" then
      assign c-acondic = "P" + substr(ped-item-ext.acondicionamento,6,3).
    else
       assign c-acondic = "????".
   
    if last-of(ped-item-ext.acondicionamento) then do:
       assign c-desenho   = substr(ped-item.cod-refer,3,5)
              c-obsoleto  = if item-ext.cod-obsoleto <= "0" then
                               "Lc"
                            else
                            if item-ext.cod-obsoleto = "1" then
                               "Fp"
                            else
                            if item-ext.cod-obsoleto = "2" then
                               "Pr"
                            else
                            if item-ext.cod-obsoleto = "3" then
                               "Rt"
                            else
                            if item-ext.cod-obsoleto = "4" then
                               "Di"
                            else
                               "Ex".
      if not l-imp-item then do:
         put "Item: "
             item.it-codigo
             " "
             item.desc-item format "x(30)"
             skip.
         assign l-imp-item = yes.
      end.
 
      if tt-param.tipo-rel = 2 then do: /* Detalhado */
         assign de-carteira2 = de-carteira2 + de-carteira1
                de-carteira3 = de-carteira3 + de-carteira2.
         assign de-diferenc1 = de-estoque - de-carteira1
                de-diferenc2 = de-estoque - de-carteira2
                de-diferenc3 = de-estoque - de-carteira3.
 
         display c-desenho     when not l-item
                 c-obsoleto    when not l-item
                 c-acondic
                 de-estoque
                 de-carteira1
                 de-diferenc1
                 de-carteira2
                 de-diferenc2
                 de-carteira3
                 de-diferenc3
                 de-reservada
                 de-disponivel
                 with frame f-detalhe.
         down with frame f-detalhe.
 
         assign l-prim-vez = no
                l-item     = yes.
      end.
 
      assign de-estoque    = 0
             de-carteira1  = 0
             de-carteira2  = 0
             de-carteira3  = 0
             de-disponivel = 0
             de-reservada  = 0.
    end.
    if last-of(ped-item.cod-refer) then do:
      for each saldo-estoq
          where saldo-estoq.it-codigo  =  ped-item.it-codigo
            and saldo-estoq.cod-refer  <> "s/corte"
            and (saldo-estoq.cod-refer <> "peca"
              or l-peca = no)
            and (saldo-estoq.cod-refer <> "rolo"
                 or l-rolo = no)
          no-lock
          break by saldo-estoq.cod-refer:
          assign de-estoque         = de-estoque
                                    + saldo-estoq.qtidade-atu
                 de-tot-estoque     = de-tot-estoque
                                    + saldo-estoq.qtidade-atu
                 de-tot-estoque-ger = de-tot-estoque-ger
                                    + saldo-estoq.qtidade-atu
                 de-tot-estoque-item = de-tot-estoque-item
                                    + saldo-estoq.qtidade-atu.

          if  last-of(saldo-estoq.cod-refer)
          and de-estoque <> 0 then do:
              if tt-param.tipo-rel = 2 then do: /* Detalhado */
                 display substr(saldo-estoq.cod-refer,1,4) @ c-acondic
                         de-estoque
                         with frame f-detalhe.
                 down with frame f-detalhe.
              end.
              assign de-estoque = 0.
          end.
      end.
      assign l-peca = no
             l-rolo = no.
      for each saldo-estoq
          where saldo-estoq.it-codigo = ped-item.it-codigo
            and saldo-estoq.cod-refer =  "s/corte"
                no-lock:
          assign de-estoque         = de-estoque
                                    + saldo-estoq.qtidade-atu
                 de-tot-estoque     = de-tot-estoque
                                    + saldo-estoq.qtidade-atu
                 de-tot-estoque-ger = de-tot-estoque-ger
                                    + saldo-estoq.qtidade-atu
                 de-tot-estoque-item = de-tot-estoque-item
                                    + saldo-estoq.qtidade-atu.
      end.
      if  tt-param.tipo-rel = 2 /* Detalhado */
      and (de-estoque <> 0   or
           de-carteira1 <> 0 or
           de-carteira2 <> 0 or
           de-carteira3 <> 0 or
           de-disponivel <> 0) then do:
           assign de-carteira2 = de-carteira2 + de-carteira1
                  de-carteira3 = de-carteira3 + de-carteira2.
           assign de-diferenc1 = de-estoque - de-carteira1
                  de-diferenc2 = de-estoque - de-carteira2
                  de-diferenc3 = de-estoque - de-carteira3.
           display "S/Co"        @ c-acondic
                   de-estoque
                   de-carteira1
                   de-diferenc1
                   de-carteira2
                   de-diferenc2
                   de-carteira3
                   de-diferenc2
                   de-disponivel
                   with frame f-detalhe.
           down with frame f-detalhe.
      end.
      assign i-programado = 0
             i-processo   = 0
             i-pronto     = 0.
      find item-prog where item-prog.it-codigo = ped-item.it-codigo
                           no-error.
      if avail item-prog then
         assign i-programado         = item-prog.programado
                i-processo           = item-prog.processo
                i-pronto             = item-prog.pronto
                i-tot-programado-item = i-tot-programado-item
                                     + i-programado
                i-tot-programado-ger = i-tot-programado-ger
                                     + i-programado
                i-tot-processo-item   = i-tot-processo-item
                                     + i-processo
                i-tot-processo-ger   = i-tot-processo-ger
                                     + i-processo
                i-tot-pronto-item     = i-tot-pronto-item + i-pronto
                i-tot-pronto-ger     = i-tot-pronto-ger + i-pronto.

      assign de-tot-carteira2 = de-tot-carteira2 + de-tot-carteira1
             de-tot-carteira3 = de-tot-carteira3 + de-tot-carteira2.
      assign de-diferenc1 = de-tot-estoque - de-tot-carteira1
             de-diferenc2 = de-tot-estoque - de-tot-carteira2
             de-diferenc3 = de-tot-estoque - de-tot-carteira3.
      display "Total"             @ c-desenho
              de-tot-estoque      @ de-estoque
              de-tot-carteira1    @ de-carteira1
              de-diferenc1
              de-tot-carteira2    @ de-carteira2
              de-diferenc2
              de-tot-carteira3    @ de-carteira3
              de-diferenc3
              de-tot-reservada    @ de-reservada
              i-programado
              i-processo
              i-pronto
              (de-tot-estoque - de-tot-carteira3) +
              (i-programado + i-processo + i-pronto)
                                  @ de-disponivel
           with frame f-detalhe.
      down with frame f-detalhe.
      put skip(1).
      assign l-prim-vez       = yes
             l-item           = no
             de-tot-estoque   = 0
             de-tot-reservada = 0
             de-tot-carteira1 = 0
             de-tot-carteira2 = 0
             de-tot-carteira3 = 0.

      if last-of(item.it-codigo) then do:
         assign de-tot-carteira-item2 = de-tot-carteira-item2 +
                                       de-tot-carteira-item1
                de-tot-carteira-item3 = de-tot-carteira-item3 +
                                       de-tot-carteira-item2.
         assign de-diferenc1 = de-tot-estoque-item - de-tot-carteira-item1
                de-diferenc2 = de-tot-estoque-item - de-tot-carteira-item2
                de-diferenc3 = de-tot-estoque-item -
                               de-tot-carteira-item3.
         display "Item"                 @ c-desenho
                 de-tot-estoque-item    @ de-estoque
                 de-tot-carteira-item1  @ de-carteira1
                 de-diferenc1
                 de-tot-carteira-item2  @ de-carteira2
                 de-diferenc2
                 de-tot-carteira-item3  @ de-carteira3
                 de-diferenc3
                 de-tot-reservada-item  @ de-reservada
                 i-tot-programado-item  @ i-programado
                 i-tot-processo-item    @ i-processo
                 i-tot-pronto-item      @ i-pronto
                  (de-tot-estoque-item - de-tot-carteira-item3) +
                  (i-tot-programado-item + i-tot-processo-item +
                   i-tot-pronto-item)   @ de-disponivel
              with frame f-detalhe.
        down with frame f-detalhe.
        put skip(1).
        assign de-tot-estoque-item   = 0
               de-tot-carteira-item1 = 0
               de-tot-carteira-item2 = 0
               de-tot-carteira-item3 = 0
               de-tot-reservada-item = 0
               i-tot-programado-item = 0
               i-tot-processo-item   = 0
               i-tot-pronto-item     = 0
               l-imp-item            = no.
      end.
      for each w-work:
          delete w-work.
      end.
    end.
end.
assign de-tot-carteira-ger2 = de-tot-carteira-ger2 + de-tot-carteira-ger1
       de-tot-carteira-ger3 = de-tot-carteira-ger3 +
                              de-tot-carteira-ger2.
assign de-diferenc1 = de-tot-estoque-ger - de-tot-carteira-ger1
       de-diferenc2 = de-tot-estoque-ger - de-tot-carteira-ger2
       de-diferenc3 = de-tot-estoque-ger - de-tot-carteira-ger3.
display "Geral"               @ c-desenho
        de-tot-estoque-ger    @ de-estoque
        de-tot-carteira-ger1  @ de-carteira1
        de-diferenc1
        de-tot-carteira-ger2  @ de-carteira2
        de-diferenc2
        de-tot-carteira-ger3  @ de-carteira3
        de-diferenc3
        de-tot-reservada-ger  @ de-reservada
        i-tot-programado-ger  @ i-programado
        i-tot-processo-ger    @ i-processo
        i-tot-pronto-ger      @ i-pronto
         (de-tot-estoque-ger - de-tot-carteira-ger3) +
         (i-tot-programado-ger + i-tot-processo-ger +
          i-tot-pronto-ger)   @ de-disponivel
    with frame f-detalhe.
down with frame f-detalhe.
      
IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.nr-pedcli-ini
           tt-param.nr-pedcli-fin
           tt-param.it-codigo-ini
           tt-param.it-codigo-fin
           tt-param.dt-entrega1-ini
           tt-param.dt-entrega1-fin
           tt-param.dt-entrega2-ini
           tt-param.dt-entrega2-fin
           tt-param.dt-entrega3-ini
           tt-param.dt-entrega3-fin
           tt-param.desc-credito
           tt-param.desc-tipo-rel
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

