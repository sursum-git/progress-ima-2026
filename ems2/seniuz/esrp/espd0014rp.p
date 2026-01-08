/* Programa: ESPD052.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Gerar o relatorio de Pedidos de Vendas por Representante/Cliente
** Autor...: Gilvando de Souza Araujo - Junho/96
**
**           Alterado em Fevereiro/2001 por FABIO COELHO LANZA
**           Incluido Quantidade Reservada / Data Pronto
**           Incluido Cor
**           Incluido Tipo Relatorio (Resumido/Detalhado/Compacto)
**           Incluido Data Implantacao
**           Excluido a coluna de Valor (de-valor) 
**           Excluido Representante da Linha de Impressao
**
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL / SANTA ELISABETH
**
** Conversao para EMS 2.04:
**   Programa: ESPD052.P  =>  ESPD0014RP.P
**   Autor...: Prodb - Toninho
**   Data....: 06/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESPD0014RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       FIELD c-repres-ini     LIKE ped-venda.no-ab-reppri 
       FIELD c-repres-fim     LIKE ped-venda.no-ab-reppri
       FIELD c-cliente-ini    LIKE ped-venda.nome-abrev
       FIELD c-cliente-fim    LIKE ped-venda.nome-abrev
       FIELD i-grupo-ini      LIKE ITEM.ge-codigo
       FIELD i-grupo-fim      LIKE ITEM.ge-codigo
       FIELD c-item-ini       LIKE ITEM.it-codigo
       FIELD c-item-fim       LIKE ITEM.it-codigo
       FIELD c-ref-ini        LIKE ref-item.cod-refer
       FIELD c-ref-fim        LIKE ref-item.cod-refer
       FIELD da-entr-ini      LIKE ped-venda.dt-entrega   
       FIELD da-entr-fim      LIKE ped-venda.dt-entrega   
       FIELD da-impl-ini      LIKE ped-venda.dt-implant
       FIELD da-impl-fim      LIKE ped-venda.dt-implant   
       FIELD da-can-ini       LIKE ped-venda.dt-cancela
       FIELD da-can-fim       LIKE ped-venda.dt-cancela
       FIELD de-percom-ini    LIKE ped-repre.perc-comis
       FIELD de-percom-fim    LIKE ped-repre.perc-comis
       FIELD c-cond-pag       AS   CHAR FORMAT "x"
       FIELD c-saldo          AS   CHAR FORMAT "x"
       FIELD c-tipo-rel       AS   CHAR FORMAT "x"
       FIELD c-tipo-merc      AS   CHAR FORMAT "x"
       FIELD c-tipo-artigo    AS   CHAR FORMAT "x"
       FIELD c-tp-pedido      AS   CHAR FORMAT "x"
       FIELD l-ped-abe        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-atp        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-att        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-pen        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-sus        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-can        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-out        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-crd-nava       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-crd-aval       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-crd-aprv       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-crd-repr       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-pula-pag       AS   LOG  FORMAT "Sim/NÆo"
       FIELD impr-param       AS   LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

def TEMP-TABLE tt-work  /* Itens sem fator de conversao */
    field it-codigo like item.it-codigo
    INDEX ch-work it-codigo.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def buffer b-ped-item for ped-item.
def var c-data-pronto as char format "x(10)".
def var c-tudo-pronto as log init yes.
def var c-cor as char format "XX.XXXX-X".
def var c-prazos as char format "x(3)".
def var da-ult-separ like ped-item-res.dt-trans.

def var c-nom-cred as char format "x(3)".
def var c-tab-cred as char extent 5 init ["NAv","Ava","Apr","NAp"].
def var c-nom-sit as char format "x(3)".
def var c-tab-sit as char extent 7 init ["Abe","Atp","Att","Pen","Sus","Can","Out"].

def var c-descr-cpag as char.
def var de-quantidade as dec.
def var de-qtd-conv as dec format "->>>>,>>9.9".
def var de-valor as dec format "->>>>>,>>9.99".
def var de-qtd-ped as dec format "->>>>,>>9.9".
def var de-qtd-rep as dec format "->>>>,>>9.99".
def var de-qtd-ger as dec format "->>>>,>>9.9".
def var de-vlr-ped as dec format "->,>>>,>>9.99".
def var de-vlr-rep as dec format "->,>>>,>>9.99".
def var de-vlr-ger as dec format "->,>>>,>>9.99".
def var de-res-item as dec format "->>>>,>>9.99".
def var de-res-rep  as dec format "->,>>>,>>9.99".
def var de-res-ger  as dec format "->,>>>,>>9.99".
def var de-tot-res-item as dec format "->,>>>,>>9.99".
def var c-seq     like ped-item.nr-sequencia.
def var c-rep-ant like ped-venda.no-ab-rep.
def var c-cli-ant like ped-venda.nome-abrev.
def var c-ped-ant like ped-venda.nr-pedcli.
def var l-imprimiu as log.
DEF VAR l-falta-fator    AS LOG.

form
    tt-param.c-repres-ini   LABEL "Representante de"       AT 8
    "a"                                                    AT 46
    tt-param.c-repres-fim   NO-LABELS
    tt-param.c-cliente-ini  LABEL "Cliente de"             AT 14
    "a"                                                    AT 46
    tt-param.c-cliente-fim  NO-LABELS  
    tt-param.i-grupo-ini    LABEL "Grupo Estoque de"       AT 8
    "a"                                                    AT 46
    tt-param.i-grupo-fim    NO-LABELS 
    tt-param.c-item-ini     LABEL "Item de"                AT 17
    "a"                                                    AT 46
    tt-param.c-item-fim     NO-LABELS
    tt-param.da-entr-ini    LABEL "Dt. Entrega de"         AT 10              
    "a"                                                    AT 46
    tt-param.da-entr-fim    NO-LABELS  
    tt-param.da-impl-ini    LABEL "Dt. Implant de"         AT 10          
    "a"                                                    AT 46
    tt-param.da-impl-fim    NO-LABELS             
    tt-param.da-can-ini     LABEL "Dt. Cancela de"         AT 10         
    "a"                                                    AT 46
    tt-param.da-can-fim     NO-LABELS            
    tt-param.de-percom-ini  LABEL "% Comiss de"            AT 13      
    "a"                                                    AT 46
    tt-param.de-percom-fim  NO-LABELS 
    tt-param.c-cond-pag     LABEL "Cond Pagto"  FORMAT "x(20)"  AT 14
    tt-param.c-saldo        LABEL "Saldo"       FORMAT "x(20)"  AT 19
    tt-param.c-tipo-rel     LABEL "Tipo Rel"    FORMAT "x(20)"  AT 16         
    tt-param.c-tipo-merc    LABEL "Mercado"     FORMAT "x(20)"  AT 17
    tt-param.c-tipo-artigo  LABEL "Artigo"      FORMAT "x(20)"  AT 17
    tt-param.c-tp-pedido    LABEL "Tipo Pedido" FORMAT "x(20)"  AT 13
    tt-param.l-ped-abe      LABEL "Aberto"                 AT 18          
    tt-param.l-ped-atp      LABEL "Atend Parcial"          AT 11
    tt-param.l-ped-att      LABEL "Atend Total"            AT 13         
    tt-param.l-ped-pen      LABEL "Pendente"               AT 16
    tt-param.l-ped-sus      LABEL "Suspenso"               AT 16         
    tt-param.l-ped-can      LABEL "Cancelado"              AT 15
    tt-param.l-ped-out      LABEL "Outros"                 AT 18         
    tt-param.l-crd-nava     LABEL "Cred NÆo Avaliado"      AT 7
    tt-param.l-crd-aval     LABEL "Cred Avaliado"          AT 11         
    tt-param.l-crd-aprv     LABEL "Cred Aprovado"          AT 11
    tt-param.l-crd-repr     LABEL "Cred Reprovado"         AT 10         
    tt-param.l-pula-pag     LABEL "Salta Pag por Repres."  AT 3 
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

form
    ped-venda.nome-abrev   label "Cliente"
    ped-venda.nr-pedcli    label "Pedido"          format "x(6)"
    ped-venda.nr-pedrep    label "Ped.Repres."
    ped-venda.dt-implant   label "Dt.Implan."
    ped-venda.dt-entrega   label "Dt.Entrega"
    c-prazos               label "Prz"             
    c-nom-sit              label "Sit"
    c-nom-cred             label "Cre"
    ped-venda.tp-pedido    LABEL "TP" 
    c-seq                  label "Seq"             format "9999"
    item.descricao-1       label "Artigo."         format "x(11)"
    c-cor                  label "Cor"
    de-qtd-conv            label "Quantidade"
    c-data-pronto          label "Dt.Pronto"
    de-res-item            label "Qtd Reserv"
    with no-box 55 down width 132 STREAM-IO frame f-detalhe no-label.

form
    "Representante: "
    repres.cod-rep format "99999"
    repres.nome
    repres.cidade
    repres.estado
    skip(1)
    with no-box no-labels 55 down width 132 STREAM-IO frame f-repres.

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
{utp/ut-liter.i Pedidos_de_Venda_por_Repres/Cliente/Reserva * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each ped-venda use-index ch-rep-cli WHERE
         ped-venda.no-ab-reppri >= tt-param.c-repres-ini AND
         ped-venda.no-ab-reppri <= tt-param.c-repres-fim AND
         ped-venda.nome-abrev   >= tt-param.c-cliente-ini AND
         ped-venda.nome-abrev   <= tt-param.c-cliente-fim AND
         ped-venda.dt-entrega   >= tt-param.da-entr-ini AND
         ped-venda.dt-entrega   <= tt-param.da-entr-fim AND
         ped-venda.dt-implant   >= tt-param.da-impl-ini AND
         ped-venda.dt-implant   <= tt-param.da-impl-fim no-lock:

    run pi-acompanhar in h-acomp (input ped-venda.nr-pedcli).
                        
    IF ped-venda.dt-cancela <> ? and
       (ped-venda.dt-cancela < tt-param.da-can-ini or                  
        ped-venda.dt-cancela > tt-param.da-can-fim) then next.
                       
    IF (tt-param.c-cond-pag = "V" and
        (ped-venda.cod-cond-pag = 1 or ped-venda.cod-cond-pag > 4)) OR
       (tt-param.c-cond-pag = "P" and
        (ped-venda.cod-cond-pag > 1 and ped-vend.cod-cond-pag < 5)) THEN NEXT.
       
    IF (ped-venda.cod-sit-ped = 1 and not tt-param.l-ped-abe) OR
       (ped-venda.cod-sit-ped = 2 and not tt-param.l-ped-atp) OR
       (ped-venda.cod-sit-ped = 3 and not tt-param.l-ped-att) OR
       (ped-venda.cod-sit-ped = 4 and not tt-param.l-ped-pen) OR
       (ped-venda.cod-sit-ped = 5 and not tt-param.l-ped-sus) OR
       (ped-venda.cod-sit-ped = 6 and not tt-param.l-ped-can) THEN NEXT.

    IF (ped-venda.cod-sit-aval = 1 and not tt-param.l-crd-nava) OR
       (ped-venda.cod-sit-aval = 2 and not tt-param.l-crd-aval) OR
       (ped-venda.cod-sit-aval = 3 and not tt-param.l-crd-aprv) OR
       (ped-venda.cod-sit-aval = 4 and not tt-param.l-crd-repr) THEN NEXT.
   
    IF ped-venda.cod-sit-ped > 5 and ped-venda.cod-sit-ped <> 6 AND
       not tt-param.l-ped-out then next.

    if tt-param.c-tp-pedido <> "" AND
       ped-venda.tp-pedido <> tt-param.c-tp-pedido then next.

    if (ped-venda.cod-cond-pag > 1 and ped-venda.cod-cond-pag < 5) then
       assign c-prazos = " V".
    else
       assign c-prazos = " P".

    find emitente where 
         emitente.cod-emitente = ped-venda.cod-emitente no-lock no-error.

    if avail emitente then
       if (tt-param.c-tipo-merc = "I" and emitente.natureza =  3)
       or (tt-param.c-tipo-merc = "E" and emitente.natureza <> 3) then next.

    find ped-repre WHERE
         ped-repre.nr-pedido = ped-venda.nr-pedido AND
         ped-repre.nome-ab-rep = ped-venda.no-ab-reppri no-lock no-error.

    if avail ped-repre then
       if ped-repre.perc-comis < tt-param.de-percom-ini OR
          ped-repre.perc-comis > tt-param.de-percom-fim then next.
   
    if ped-venda.cod-cond-pag <> 0 then do:
       find cond-pagto where 
            cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag no-lock no-error.

       if avail cond-pagto then
          assign c-descr-cpag = substr(cond-pagto.descricao,1,22).
       else
          assign c-descr-cpag = "Especial".
    end. 
    else
       assign c-descr-cpag = "Especial".

    assign c-nom-sit = c-tab-sit[ped-venda.cod-sit-ped].
    assign c-nom-cred = c-tab-cred[ped-venda.cod-sit-aval].

    if ped-venda.no-ab-rep <> c-rep-ant then do:
       if de-qtd-rep <> 0 OR de-vlr-rep <> 0 then
          put "Total do Representante:" at   1
              de-qtd-rep                at  96
              de-res-rep                at 120
              skip(1).                      

       assign de-qtd-rep      = 0       de-vlr-rep = 0
              de-res-rep      = 0       c-rep-ant  = ped-venda.no-ab-rep
              de-tot-res-item = 0.

       if tt-param.l-pula-pag then
          page.
       else
          put skip(1).

       find repres where
            repres.nome-abrev = ped-venda.no-ab-rep no-lock no-error.

       if avail repres then
          display repres.cod-rep
                  repres.nome
                  repres.cidade
                  repres.estado
                  with frame f-repres.

       down with frame f-repres.
    end.
   
    assign l-imprimiu     = no
           c-data-pronto  = "*"
           c-tudo-pronto  = yes
           da-ult-separ   = 01/01/0001.
          
    for each ped-item of ped-venda where
             ped-item.it-codigo >= tt-param.c-item-ini and
             ped-item.it-codigo <= tt-param.c-item-fim and
             ped-item.cod-refer >= tt-param.c-ref-ini and
             ped-item.cod-refer <= tt-param.c-ref-fim no-lock
             BY ped-item.nr-sequencia:
   
        find item where
             item.it-codigo = ped-item.it-codigo no-error.

        if avail item then
           if item.ge-codigo < tt-param.i-grupo-ini OR
              item.ge-codigo > tt-param.i-grupo-fim then next.

        FIND item-ext WHERE item-ext.it-codigo = ped-item.it-codigo
                      NO-LOCK NO-ERROR.

        IF AVAIL item-ext THEN
           IF item-ext.indigo = YES AND tt-param.c-tipo-artigo = "O" OR
              item-ext.indigo = NO  AND tt-param.c-tipo-artigo = "I" THEN NEXT.

        if ped-item.cod-sit-item = 6 and not tt-param.l-ped-can then next.

        assign c-cor = ped-item.cod-refer
               c-seq = ped-item.nr-sequencia.

        /* ------- Data Pronto / Quantidade Reservada ------- */
       
        de-res-item = 0.
        find ped-item-res WHERE
             ped-item-res.nome-abrev    = ped-item.nome-abrev AND
             ped-item-res.nr-pedcli     = ped-item.nr-pedcli AND
             ped-item-res.it-codigo     = ped-item.it-codigo AND
             ped-item-res.nr-sequencia  = ped-item.nr-sequencia no-lock no-error.
                    
        if avail ped-item-res then do:
           if da-ult-separ < ped-item-res.dt-trans then
              assign da-ult-separ = ped-item-res.dt-trans.
             
           assign c-data-pronto   = string(da-ult-separ, "99/99/9999").
          
           if ped-item.cod-sit-item <> 2 then
              de-res-item     = ped-item-res.qt-pedida.
        end.          
        else
        if ped-item.cod-sit-item <> 9 and
           ped-item.cod-sit-item <> 2 then
           assign c-data-pronto = ""
                  c-tudo-pronto = no.
        else
           if ped-item.cod-sit-item = 9 then do:
              if da-ult-separ < ped-item.dt-canseq then
                 assign da-ult-separ = ped-item.dt-canseq.
              assign c-data-pronto = string(da-ult-separ,"99/99/9999").       
           end.
        
        if c-data-pronto = "*" then
           assign c-data-pronto = "".

        assign de-tot-res-item = de-tot-res-item + de-res-item
               de-res-rep      = de-res-rep + de-res-item
               de-res-ger      = de-res-ger + de-res-item.

        IF tt-param.c-saldo = "A" THEN
           ASSIGN de-quantidade = ped-item.qt-pedida - ped-item.qt-atendida +
                                  ped-item.qt-devolvida.
        ELSE
           ASSIGN de-quantidade = ped-item.qt-pedida.

        IF de-quantidade = 0 then next.

        assign de-valor = de-quantidade * ped-item.vl-preori.

        /*------ Conversao de M para Kg ------- */
        IF item.un <> "m" then do:
           FIND item-ext WHERE ITEM-ext.it-codigo = ITEM.it-codigo
                         NO-LOCK NO-ERROR.
           IF AVAIL item-ext THEN
              assign de-qtd-conv = de-quantidade * item-ext.fator-conv.
           else do:
              assign l-falta-fator = yes
                     de-qtd-conv   = de-quantidade.
              find first tt-work where
                         tt-work.it-codigo = item.it-codigo
                         no-lock no-error.
              IF NOT AVAIL tt-work then do:
                 CREATE tt-work.
                 ASSIGN tt-work.it-codigo = item.it-codigo.
              END.
           END.   
        END.
        ELSE
           ASSIGN de-qtd-conv = de-quantidade.

        if tt-param.c-tipo-rel = "D" then do:   /* Detalhado */
           display ped-venda.nome-abrev when ped-venda.nome-abrev <> c-cli-ant
                   ped-venda.nr-pedcli   when ped-venda.nr-pedcli  <> c-ped-ant
                   ped-venda.nr-pedrep   when ped-venda.nr-pedcli  <> c-ped-ant
                   ped-venda.dt-entrega  when ped-venda.nr-pedcli  <> c-ped-ant
                   ped-venda.dt-implant  when ped-venda.nr-pedcli  <> c-ped-ant
                   c-prazos              when ped-venda.nr-pedcli  <> c-ped-ant
                   c-nom-sit             when ped-venda.nr-pedcli  <> c-ped-ant
                   c-nom-cred            when ped-venda.nr-pedcli  <> c-ped-ant
                   ped-venda.tp-pedido   when ped-venda.nr-pedcli  <> c-ped-ant
                   c-seq
                   item.descricao-1
                   c-cor
                   de-qtd-conv
                   c-data-pronto
                   de-res-item
                   with frame f-detalhe.
           down with frame f-detalhe.

           assign l-imprimiu  = yes
                  c-rep-ant   = ped-venda.no-ab-rep
                  c-cli-ant   = ped-venda.nome-abrev
                  c-ped-ant   = ped-venda.nr-pedcli
                  de-res-item = 0.
        end.
        assign de-qtd-ped = de-qtd-ped + de-qtd-conv
               de-vlr-ped = de-vlr-ped + de-valor
               de-qtd-rep = de-qtd-rep + de-qtd-conv
               de-vlr-rep = de-vlr-rep + de-valor
               de-qtd-ger = de-qtd-ger + de-qtd-conv
               de-vlr-ger = de-vlr-ger + de-valor.
    end. /* ped-item */

    if c-tudo-pronto = no and tt-param.c-tipo-rel = "R" then do:
       for each b-ped-item of ped-venda where
                b-ped-item.it-codigo >= tt-param.c-item-ini and
                b-ped-item.it-codigo <= tt-param.c-item-fim and
                b-ped-item.cod-refer >= tt-param.c-ref-ini and
                b-ped-item.cod-refer <= tt-param.c-ref-fim no-lock
                BY b-ped-item.nr-sequencia:

           find item where
                item.it-codigo = b-ped-item.it-codigo no-error.
           if avail item then
              if item.ge-codigo < tt-param.i-grupo-ini OR
                item.ge-codigo > tt-param.i-grupo-fim then next.

           if b-ped-item.cod-sit-item = 6 and not tt-param.l-ped-can then next.

           assign c-cor = b-ped-item.cod-refer
                  c-seq = b-ped-item.nr-sequencia.

           /* ------- Data Pronto / Quantidade Reservada ------- */
       
           de-res-item = 0.
           FIND ped-item-res WHERE
                ped-item-res.nome-abrev   = b-ped-item.nome-abrev AND
                ped-item-res.nr-pedcli    = b-ped-item.nr-pedcli AND
                ped-item-res.it-codigo    = b-ped-item.it-codigo AND 
                ped-item-res.nr-sequencia = b-ped-item.nr-sequencia
                no-lock no-error.
                    
           if avail ped-item-res then do:
              if da-ult-separ < ped-item-res.dt-trans then
                 assign da-ult-separ = ped-item-res.dt-trans.
             
              assign c-data-pronto   = string(da-ult-separ,"99/99/9999").
          
              if b-ped-item.cod-sit-item <> 2 then
                 de-res-item     = ped-item-res.qt-pedida.
           end.          
           else
              if b-ped-item.cod-sit-item <> 6 and
                 b-ped-item.cod-sit-item <> 3 then
                 assign c-data-pronto = ""
                        c-tudo-pronto = no.
              else
                 if b-ped-item.cod-sit-item = 6 then do:
                    if da-ult-separ < b-ped-item.dt-canseq then
                       assign da-ult-separ = b-ped-item.dt-canseq.

                    assign c-data-pronto = string(da-ult-separ,
                                                  "99/99/9999").       
                 end.
        
           if c-data-pronto = "*" then
              assign c-data-pronto = "".
                           
           find item where
                item.it-codigo = b-ped-item.it-codigo no-error.
           if avail item then
              if item.ge-codigo < tt-param.i-grupo-ini OR
                 item.ge-codigo > tt-param.i-grupo-fim then next.

           if b-ped-item.it-codigo < tt-param.c-item-ini OR
              b-ped-item.it-codigo > tt-param.c-item-fim then next.

           if tt-param.c-saldo = "A" then
              assign de-quantidade = b-ped-item.qt-pedida - b-ped-item.qt-atendida +
                                     b-ped-item.qt-devolvida.
           else
              assign de-quantidade = b-ped-item.qt-pedida.

           if de-quantidade = 0 then next.

           assign de-valor = de-quantidade * b-ped-item.vl-preori.

           /*------ Conversao de M para Kg ------- */
           if item.un <> "m" then do:
              FIND item-ext WHERE ITEM-ext.it-codigo = ITEM.it-codigo
                            NO-LOCK NO-ERROR.
              if AVAIL item-ext then
                 assign de-qtd-conv = de-quantidade * 
                                      item-ext.fator-conv.
              else do:
                 assign l-falta-fator = yes
                        de-qtd-conv   = de-quantidade.
                 find first tt-work where
                            tt-work.it-codigo = item.it-codigo
                            no-lock no-error.
                 if not avail tt-work then do:
                    create tt-work.
                    assign tt-work.it-codigo = item.it-codigo.
                 end.
              end.   
           end.
           ELSE 
              ASSIGN de-qtd-conv = de-quantidade.

           display ped-venda.nome-abrev when ped-venda.nome-abrev  <> c-cli-ant
                   ped-venda.nr-pedcli   when ped-venda.nr-pedcli  <> c-ped-ant
                   ped-venda.nr-pedrep   when ped-venda.nr-pedcli  <> c-ped-ant
                   ped-venda.dt-entrega  when ped-venda.nr-pedcli  <> c-ped-ant
                   ped-venda.dt-implant  when ped-venda.nr-pedcli  <> c-ped-ant
                   c-prazos              when ped-venda.nr-pedcli  <> c-ped-ant
                   c-nom-sit             when ped-venda.nr-pedcli  <> c-ped-ant
                   c-nom-cred            when ped-venda.nr-pedcli  <> c-ped-ant
                   ped-venda.tp-pedido   when ped-venda.nr-pedcli  <> c-ped-ant
                   c-seq
                   item.descricao-1
                   c-cor
                   de-qtd-conv
                   c-data-pronto
                   de-res-item
                   with frame f-detalhe.

           down with frame f-detalhe.
           assign c-rep-ant = ped-venda.no-ab-rep
                  c-cli-ant = ped-venda.nome-abrev
                  c-ped-ant = ped-venda.nr-pedcli.
       end. /* ped-item */
      
       display de-qtd-ped      @ de-qtd-conv
               de-tot-res-item @ de-res-item
               with frame f-detalhe.
       down with frame f-detalhe.        
      
       assign c-data-pronto   = ""         de-qtd-ped  = 0
              de-vlr-ped      = 0          de-res-item = 0
              de-tot-res-item = 0          c-rep-ant   = ped-venda.no-ab-reppri
              c-cli-ant       = ped-venda.nome-abrev.
    end.
    else
       if c-tudo-pronto = no then
          assign c-data-pronto = "".
         
    if de-qtd-ped + de-tot-res-item  <> 0 then do:
       display ped-venda.nome-abrev when ped-venda.nome-abrev <> c-cli-ant
               ped-venda.nr-pedcli   when tt-param.c-tipo-rel <> "D"
               ped-venda.nr-pedrep   when tt-param.c-tipo-rel <> "D"
               ped-venda.dt-entrega  when tt-param.c-tipo-rel <> "D"
               ped-venda.dt-implant  when tt-param.c-tipo-rel <> "D"
               c-prazos              when tt-param.c-tipo-rel <> "D"
               c-nom-sit             when tt-param.c-tipo-rel <> "D"
               c-nom-cred            when tt-param.c-tipo-rel <> "D"
               de-qtd-ped            @ de-qtd-conv
               c-data-pronto
               de-tot-res-item       @ de-res-item
               with frame f-detalhe.
               assign de-tot-res-item = 0.
       down with frame f-detalhe.
    
       assign de-qtd-ped  = 0       de-vlr-ped  = 0
              de-res-item = 0.

       assign c-rep-ant = ped-venda.no-ab-reppri
              c-cli-ant = ped-venda.nome-abrev.
    end.      
    else
       if l-imprimiu and c-nom-cred = "N/Aprov" then
          put "Motivo bloqueio: " at 28 ped-venda.desc-bloq-cr
              skip(1).
end. /* ped-venda */

if (de-qtd-rep <> 0 or de-vlr-rep <> 0) then
   put "Total do Representante:" at   1
       de-qtd-rep                at  96
       de-res-rep                at 120
       skip(1).                     

assign de-qtd-rep  = 0      de-vlr-rep  = 0
       de-res-rep  = 0      de-res-item = 0.

if tt-param.l-pula-pag THEN
   page.
ELSE
   put skip(1).
  
if de-qtd-ger <> 0 or de-vlr-ger <> 0 then
   put "Total Geral:"  at   1
       de-qtd-ger      at  96
       de-res-ger      at 120.

assign de-qtd-ger  = 0       de-vlr-ger  = 0
       de-res-ger  = 0       de-res-item = 0.

if l-falta-fator then do:
   page.
   put "Atencao ! - Ha itens sem fator de conversao:"
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

IF tt-param.impr-param THEN DO.
   PAGE.
   PUT "*****----- PAR¶METROS ------*****"
       SKIP(1).

   ASSIGN tt-param.c-cond-pag = IF tt-param.c-cond-pag = "V" 
                                THEN "· Vista" 
                                ELSE IF tt-param.c-cond-pag = "P"
                                     THEN "· Prazo" 
                                     ELSE "Todos"
             tt-param.c-saldo = IF tt-param.c-saldo = "A" 
                                THEN "Aberto" 
                                ELSE "Total" 
          tt-param.c-tipo-rel = IF tt-param.c-tipo-rel = "D"
                                THEN "Detalhado"
                                ELSE IF tt-param.c-tipo-rel = "R"
                                     THEN "Resumido"
                                     ELSE "Compacto"
         tt-param.c-tipo-merc = IF tt-param.c-tipo-merc = "I"
                                THEN "Interno"
                                ELSE IF tt-param.c-tipo-merc = "E"
                                     THEN "Externo"
                                     ELSE "Todos"

         tt-param.c-tipo-artigo = IF tt-param.c-tipo-artigo = "I"
                                  THEN "Indigo"
                                  ELSE IF tt-param.c-tipo-artigo = "O"
                                       THEN "Outros"
                                       ELSE "Todos"
         tt-param.c-tp-pedido = IF tt-param.c-tp-pedido = ""
                                THEN "Todos" 
                                ELSE tt-param.c-tp-pedido.

   DISPLAY tt-param.c-repres-ini             tt-param.c-repres-fim    
           tt-param.c-cliente-ini            tt-param.c-cliente-fim   
           tt-param.i-grupo-ini              tt-param.i-grupo-fim     
           tt-param.c-item-ini               tt-param.c-item-fim      
           tt-param.da-entr-ini              tt-param.da-entr-fim     
           tt-param.da-impl-ini              tt-param.da-impl-fim     
           tt-param.da-can-ini               tt-param.da-can-fim      
           tt-param.de-percom-ini            tt-param.de-percom-fim   
           tt-param.c-cond-pag               tt-param.c-saldo         
           tt-param.c-tipo-rel               tt-param.c-tipo-merc
           tt-param.c-tipo-artigo            tt-param.c-tp-pedido     
           tt-param.l-ped-abe                tt-param.l-ped-atp       
           tt-param.l-ped-att                tt-param.l-ped-pen       
           tt-param.l-ped-sus                tt-param.l-ped-can       
           tt-param.l-ped-out                tt-param.l-crd-nava      
           tt-param.l-crd-aval               tt-param.l-crd-aprv      
           tt-param.l-crd-repr               tt-param.l-pula-pag      
           WITH FRAME f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.


