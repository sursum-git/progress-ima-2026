/* Programa: ESPD036.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Gerar o relatorio de Pedidos de Vendas por Representante/Cliente
** Autor...: Gilvando de Souza Araujo - Junho/96
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL / SANTA ELISABETH
**
** Conversao para EMS 2.04:
**   Programa: ESPD036.P  =>  ESFT0017RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 02/02/2005
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0017RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino          AS INTEGER
       FIELD arquivo          AS CHAR FORMAT "x(35)"
       FIELD usuario          AS CHAR FORMAT "x(12)"
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       FIELD classifica       AS INTEGER
       FIELD c-repres-ini     LIKE ped-venda.no-ab-reppri 
       FIELD c-repres-fim     LIKE ped-venda.no-ab-reppri
       FIELD c-cliente-ini    LIKE ped-venda.nome-abrev
       FIELD c-cliente-fim    LIKE ped-venda.nome-abrev
       FIELD i-grupo-ini      LIKE ITEM.ge-codigo
       FIELD i-grupo-fim      LIKE ITEM.ge-codigo
       FIELD c-item-ini       LIKE ITEM.it-codigo
       FIELD c-item-fim       LIKE ITEM.it-codigo
       FIELD c-ref-ini        LIKE ped-item.cod-refer
       FIELD c-ref-fim        LIKE ped-item.cod-refer
       FIELD da-entr-ini      LIKE ped-venda.dt-entrega   
       FIELD da-entr-fim      LIKE ped-venda.dt-entrega   
       FIELD da-impl-ini      LIKE ped-venda.dt-implant
       FIELD da-impl-fim      LIKE ped-venda.dt-implant   
       FIELD da-can-ini       LIKE ped-venda.dt-cancela
       FIELD da-can-fim       LIKE ped-venda.dt-cancela
       FIELD de-percom-ini    LIKE ped-repre.perc-comis
       FIELD de-percom-fim    LIKE ped-repre.perc-comis
       FIELD c-transp-ini     LIKE ped-venda.nome-transp
       FIELD c-transp-fim     LIKE ped-venda.nome-transp
       FIELD c-cond-pag       AS   CHAR FORMAT "x"
       FIELD c-saldo          AS   CHAR FORMAT "x"
       FIELD c-tipo-rel       AS   CHAR FORMAT "x"
       FIELD c-tipo-merc      AS   CHAR FORMAT "x"
       FIELD c-tp-pedido      AS   CHAR FORMAT "x"
       FIELD l-ped-abe        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-atp        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-att        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-pen        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-sus        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-can        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-ped-out        AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-item-abe       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-item-atp       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-item-att       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-item-pen       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-item-sus       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-item-can       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-item-out       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-crd-nava       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-crd-aval       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-crd-aprv       AS   LOG  FORMAT "Sim/NÆo"     
       FIELD l-crd-repr       AS   LOG  FORMAT "Sim/NÆo"
       FIELD c-cod-descr      AS   CHAR FORMAT "x"
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

def var c-nom-cred    as char format "x(4)".
def var c-tab-cred    as char extent 5 init ["N/Av","Aval","Aprv","N/Ap"].
def var c-nom-sit     as char format "x(3)".
def var c-nom-sit-it  as char format "x(3)".
def var c-tab-sit     as char extent 7 init ["Abe","Atp","Att","Pen","Sus","Can","Out"].
def var c-descr-cpag  as char.
def var c-artigo      as char.
def var de-quantidade as dec.
def var de-qtd-conv   as dec format "->>>>,>>9.9999".
def var de-valor      as dec format "->>>>>,>>9.99".
def var de-qtd-ped    as dec format "->>>>,>>9.9999".
def var de-qtd-rep    as dec format "->>>>,>>9.9999".
def var de-qtd-ger    as dec format "->>>>,>>9.9999".
def var de-vlr-ped    as dec format "->,>>>,>>9.99".
def var de-vlr-rep    as dec format "->,>>>,>>9.99".
def var de-vlr-ger    as dec format "->,>>>,>>9.99".
def var c-rep-ant     like ped-venda.no-ab-rep.
def var c-cli-ant     like ped-venda.nome-abrev.
def var c-ped-ant     like ped-venda.nr-pedcli.
def var c-opcao       as char.
def var l-imprimiu    as log.
def var l-prim-vez    as log.
DEF VAR l-falta-fator AS LOG.

form
    tt-param.c-repres-ini   LABEL "Representante" 
    "a"  AT 33  
    tt-param.c-repres-fim   NO-LABELS SKIP                          
    tt-param.c-cliente-ini  LABEL "Cliente......"       
    "a"  AT 33                        
    tt-param.c-cliente-fim  NO-LABELS SKIP                 
    tt-param.i-grupo-ini    LABEL "Grupo Estoque"       
    "a"  AT 33                        
    tt-param.i-grupo-fim    NO-LABELS SKIP                 
    tt-param.c-item-ini     LABEL "Item........."       
    "a"  AT 33                        
    tt-param.c-item-fim     NO-LABELS SKIP                 
    tt-param.c-ref-ini      LABEL "Referencia..."       
    "a"  AT 33                             
    tt-param.c-ref-fim      NO-LABELS SKIP                 
    tt-param.da-entr-ini    LABEL "Dt.Entrega de"        
    "a"  AT 33                         
    tt-param.da-entr-fim    NO-LABELS SKIP                   
    tt-param.da-impl-ini    LABEL "Dt.Implant de"        
    "a"  AT 33                          
    tt-param.da-impl-fim    NO-LABELS SKIP                   
    tt-param.da-can-ini     LABEL "Dt.Cancela de"        
    "a"  AT 33                               
    tt-param.da-can-fim     NO-LABELS SKIP                   
    tt-param.de-percom-ini  LABEL "% Comiss de.."        
    "a"  AT 33                                         
    tt-param.de-percom-fim  NO-LABELS SKIP
    tt-param.c-transp-ini   LABEL "Transportador"        
    "a"  AT 33                                         
    tt-param.c-transp-fim   NO-LABELS SKIP
    tt-param.c-cond-pag     LABEL "Cond Pagto..." FORMAT "x(20)" SKIP 
    tt-param.c-saldo        LABEL "Saldo........" FORMAT "x(20)" SKIP     
    tt-param.c-tipo-rel     LABEL "Tipo Rel....." FORMAT "x(20)" SKIP             
    tt-param.c-tipo-merc    LABEL "Mercado......" FORMAT "x(20)" SKIP
    tt-param.c-tp-pedido    LABEL "Tipo Pedido.." FORMAT "x(20)" SKIP
    tt-param.l-ped-abe      LABEL "Pedido-Abe..." SKIP                      
    tt-param.l-ped-atp      LABEL "      -Atp..." SKIP                      
    tt-param.l-ped-att      LABEL "      -Att..." SKIP                      
    tt-param.l-ped-pen      LABEL "      -Pen..." SKIP                      
    tt-param.l-ped-sus      LABEL "      -Sus..." SKIP                     
    tt-param.l-ped-can      LABEL "      -Can..." SKIP                      
    tt-param.l-ped-out      LABEL "      -Out..." SKIP                      
    tt-param.l-item-abe     LABEL "Item-Abe....." SKIP                      
    tt-param.l-item-atp     LABEL "    -Atp....." SKIP                      
    tt-param.l-item-att     LABEL "    -Att....." SKIP                      
    tt-param.l-item-pen     LABEL "    -Pen....." SKIP                      
    tt-param.l-item-sus     LABEL "    -Sus....." SKIP                     
    tt-param.l-item-can     LABEL "    -Can....." SKIP                      
    tt-param.l-item-out     LABEL "    -Out....." SKIP                      
    tt-param.l-crd-nava     LABEL "Cred NÆo Aval" SKIP                      
    tt-param.l-crd-aval     LABEL "Cred Avaliado" SKIP
    tt-param.l-crd-aprv     LABEL "Cred Aprovado" SKIP 
    tt-param.l-crd-repr     LABEL "Cred Reprov.." SKIP 
    tt-param.l-pula-pag     LABEL "Salta Pagina."  
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

form
    ped-venda.no-ab-rep    label "Representante"   FORMAT "x(12)"
    ped-venda.cod-emitente label "Client"          FORMAT "999999"
    ped-venda.nome-abrev   label "Nome Abrev"
    ped-venda.nr-pedcli    label "Pedido"          format "x(6)"
    ped-venda.dt-entrega   label "Dt.Entrega"
    c-descr-cpag           label "Prazos"          format "x(17)"
    c-nom-sit              label "Sit"
    c-nom-cred             label "Cred"
    ped-item.nr-sequencia  label "Seq"             format "9999"
    c-nom-sit-it           label "Sit"
    c-artigo               label "Artigo/Ped.Rep." format "x(15)"
    de-qtd-conv            label "Quantidade"
    de-valor               label "Valor"
    with NO-LABEL no-box 55 down width 132 STREAM-IO frame f-detalhe.
/*
FORM
    "Total do Representante:" at   1
    de-qtd-rep                at 105
    de-vlr-rep                at 120
    skip(1)      
    with no-labels no-box no-attr-space width 132 STREAM-IO 1 down frame f-tot-rep.
*/
FORM
    "Total Geral:" at   1
    de-qtd-ger     at 105
    de-vlr-ger     at 120
    skip(1)                        
    with no-labels no-box no-attr-space width 132 STREAM-IO 1 down frame f-tot-ger.

FORM
    "Motivo bloqueio: "    at 28 
    ped-venda.desc-bloq-cr AT 44
    skip(1)                        
    with no-labels no-box no-attr-space width 132 STREAM-IO 1 down frame f-mot-bloq.

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
{utp/ut-liter.i Listagem_Pedidos_Venda_por_Representante/Cliente * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each ped-venda use-index ch-rep-cli 
   where ped-venda.no-ab-reppri >= tt-param.c-repres-ini 
     AND ped-venda.no-ab-reppri <= tt-param.c-repres-fim
     AND ped-venda.nome-abrev   >= tt-param.c-cliente-ini
     AND ped-venda.nome-abrev   <= tt-param.c-cliente-fim 
     AND ped-venda.dt-entrega   >= tt-param.da-entr-ini 
     AND ped-venda.dt-entrega   <= tt-param.da-entr-fim 
     AND ped-venda.dt-implant   >= tt-param.da-impl-ini 
     AND ped-venda.dt-implant   <= tt-param.da-impl-fim 
     AND ped-venda.nome-transp  >= tt-param.c-transp-ini
     AND ped-venda.nome-transp  <= tt-param.c-transp-fim
     AND ((SUBSTR(ped-venda.nat-operacao,4,3) = "zfm" AND tt-param.c-tipo-merc = "z") OR
                                                          tt-param.c-tipo-merc <> "z")
   no-lock:
    
   if (tt-param.c-cond-pag = "V" and
      (ped-venda.cod-cond-pag = 0 or ped-venda.cod-cond-pag > 3))
   or (tt-param.c-cond-pag = "P" and
      (ped-venda.cod-cond-pag > 0 and ped-vend.cod-cond-pag < 4)) then
      next.

   if (ped-venda.cod-sit-ped = 1 and not tt-param.l-ped-abe)
   or (ped-venda.cod-sit-ped = 2 and not tt-param.l-ped-atp)
   or (ped-venda.cod-sit-ped = 3 and not tt-param.l-ped-att)
   or (ped-venda.cod-sit-ped = 4 and not tt-param.l-ped-pen)
   or (ped-venda.cod-sit-ped = 5 and not tt-param.l-ped-sus)
   or (ped-venda.cod-sit-ped = 6 and not tt-param.l-ped-can)
   OR (ped-venda.cod-sit-ped > 6 AND NOT tt-param.l-ped-out) then next.

   if (ped-venda.cod-sit-aval = 1 and not tt-param.l-crd-nava)
   or (ped-venda.cod-sit-aval = 2 and not tt-param.l-crd-aval)
   or (ped-venda.cod-sit-aval = 3 and not tt-param.l-crd-aprv)
   or (ped-venda.cod-sit-aval = 4 and not tt-param.l-crd-repr) then next.
   
   /* ---notas de simples remessa--- */
   if ped-venda.nat-operacao begins "599"
   or ped-venda.nat-operacao begins "699" then next.

   if  tt-param.c-tp-pedido <> ""
   and ped-venda.tp-pedido <> tt-param.c-tp-pedido then next.

   find emitente where
        emitente.cod-emitente = ped-venda.cod-emitente 
        no-lock no-error.

   if avail emitente then
      if (tt-param.c-tipo-merc = "I" and emitente.natureza =  3)
      or (tt-param.c-tipo-merc = "E" and emitente.natureza <> 3) then next.
   
   find ped-repre where
        ped-repre.nr-pedido   = ped-venda.nr-pedido and
        ped-repre.nome-ab-rep = ped-venda.no-ab-reppri
        no-lock no-error.

   if avail ped-repre then
      if ped-repre.perc-comis < tt-param.de-percom-ini
      or ped-repre.perc-comis > tt-param.de-percom-fim then next.

   if ped-venda.cod-cond-pag <> 0 then do:
      find cond-pagto where 
           cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag
                                     no-lock no-error.
      if avail cond-pagto then
         assign c-descr-cpag = substr(cond-pagto.descricao,1,22).
      else
         assign c-descr-cpag = "Especial".
   end.
   else
      assign c-descr-cpag = "Especial".

   if ped-venda.cod-sit-ped <= 7 then
      assign c-nom-sit = c-tab-sit[ped-venda.cod-sit-ped].
   else
      assign c-nom-sit = "Out".

   if ped-venda.cod-sit-aval <= 5 then
      assign c-nom-cred = c-tab-cred[ped-venda.cod-sit-aval].
   else
      assign c-nom-cred = "Out".

   if ped-venda.no-ab-rep <> c-rep-ant then do:
       if (de-qtd-rep <> 0
       or de-vlr-rep <> 0) THEN DO:
          DISPLAY "Total Repres:" @ ped-venda.no-ab-rep
                  de-qtd-rep      @ de-qtd-conv
                  de-vlr-rep      @ de-valor
                  WITH FRAME f-detalhe.
          DOWN 2 WITH FRAME f-detalhe.
          assign de-qtd-rep = 0
                 de-vlr-rep = 0.
          if tt-param.l-pula-pag then
             page.
       END.
    end.

   assign l-imprimiu = no.
   for each ped-item of ped-venda 
      where ped-item.it-codigo >= tt-param.c-item-ini 
        and ped-item.it-codigo <= tt-param.c-item-fim 
        and ped-item.cod-refer >= tt-param.c-ref-ini 
        AND ped-item.cod-refer <= tt-param.c-ref-fim
      no-lock:
            
      if (ped-item.cod-sit-item = 1 and not tt-param.l-item-abe)
      or (ped-item.cod-sit-item = 2 and not tt-param.l-item-atp)
      or (ped-item.cod-sit-item = 3 and not tt-param.l-item-att)
      or (ped-item.cod-sit-item = 4 and not tt-param.l-item-pen)
      or (ped-item.cod-sit-item = 5 and not tt-param.l-item-sus)
      or (ped-item.cod-sit-item = 6 and not tt-param.l-item-can)
      OR (ped-item.cod-sit-item > 6 AND NOT tt-param.l-item-out) then next.

      IF tt-param.l-ped-can AND ped-item.cod-sit-item = 6 THEN DO:
         IF ped-item.dt-canseq < tt-param.da-can-ini OR 
            ped-item.dt-canseq > tt-param.da-can-fim THEN NEXT.
      END.

      find item WHERE item.it-codigo = ped-item.it-codigo no-error.
             
      IF AVAIL item THEN
         IF item.ge-codigo < tt-param.i-grupo-ini
         OR item.ge-codigo > tt-param.i-grupo-fim THEN NEXT.

      IF c-saldo = "A" THEN DO:
         IF ped-item.cod-sit-item <> 6 THEN
            ASSIGN de-quantidade = ped-item.qt-pedida -
                                   ped-item.qt-atendida +
                                   ped-item.qt-devolvida.
         ELSE
            ASSIGN de-quantidade = 0.
      END.
      ELSE
         ASSIGN de-quantidade = ped-item.qt-pedida.
      
      IF de-quantidade = 0 then next.

      assign de-valor = de-quantidade * ped-item.vl-preori.

      /*------ Conversao de Kg para M ------*/
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
        
      if ped-item.cod-sit-item < 6 then
         assign c-nom-sit-it = c-tab-sit[ped-item.cod-sit-item].
      else
         assign c-nom-sit-it = c-tab-sit[6].
      
      if tt-param.c-tipo-rel = "D" then do:   /* Detalhado */
         if tt-param.c-cod-descr = "C" then
            assign c-artigo = substr(item.it-codigo,1,6) + '.' +
                              ped-item.cod-refer.
         else
            assign c-artigo = substr(item.descricao-1,1,15).

         display ped-venda.no-ab-rep
                     when ped-venda.no-ab-rep <> c-rep-ant
                 ped-venda.cod-emitente
                     when ped-venda.nome-abrev <> c-cli-ant
                 ped-venda.nome-abrev
                     when ped-venda.nome-abrev <> c-cli-ant
                 ped-venda.nr-pedcli
                     when ped-venda.nr-pedcli <> c-ped-ant
                 ped-venda.dt-entrega
                     when ped-venda.nr-pedcli <> c-ped-ant
                 c-descr-cpag
                     when ped-venda.nr-pedcli <> c-ped-ant
                 c-nom-sit
                     when ped-venda.nr-pedcli <> c-ped-ant
                 c-nom-cred
                     when ped-venda.nr-pedcli <> c-ped-ant
                 ped-item.nr-sequencia
                 c-nom-sit-it
                 c-artigo
                 de-qtd-conv
                 de-valor
                 with frame f-detalhe.
         down with frame f-detalhe.

         assign l-imprimiu = yes
                c-rep-ant  = ped-venda.no-ab-rep
                c-cli-ant  = ped-venda.nome-abrev
                c-ped-ant  = ped-venda.nr-pedcli.
      end.
      assign de-qtd-ped = de-qtd-ped + de-qtd-conv
             de-vlr-ped = de-vlr-ped + de-valor
             de-qtd-rep = de-qtd-rep + de-qtd-conv
             de-vlr-rep = de-vlr-rep + de-valor
             de-qtd-ger = de-qtd-ger + de-qtd-conv
             de-vlr-ger = de-vlr-ger + de-valor.

   end. /* ped-item */

   if de-qtd-ped <> 0 
   or de-vlr-ped <> 0 then do:
      assign c-artigo = ped-venda.nr-pedrep.
      display ped-venda.no-ab-rep
                  when ped-venda.no-ab-rep <> c-rep-ant
              ped-venda.cod-emitente
                  when ped-venda.nome-abrev <> c-cli-ant
              ped-venda.nome-abrev
                  when ped-venda.nome-abrev <> c-cli-ant
              ped-venda.nr-pedcli
                  when tt-param.c-tipo-rel = "R"
              c-artigo
                  when tt-param.c-tipo-rel = "R"
              ped-venda.dt-entrega
                  when tt-param.c-tipo-rel = "R"
              c-descr-cpag
                  when tt-param.c-tipo-rel = "R"
              c-nom-sit
                  when tt-param.c-tipo-rel = "R"
              c-nom-cred
                  when tt-param.c-tipo-rel = "R"
              de-qtd-ped   @ de-qtd-conv
              de-vlr-ped   @ de-valor
              with frame f-detalhe.
      down with frame f-detalhe.
      assign de-qtd-ped = 0
             de-vlr-ped = 0.

      assign c-rep-ant = ped-venda.no-ab-rep
             c-cli-ant = ped-venda.nome-abrev.
   end.
   else
      if l-imprimiu and c-nom-cred = "N/Aprov" THEN DO:
         DISPLAY ped-venda.desc-bloq-cr
                 WITH FRAME f-mot-bloq.
         DOWN WITH FRAME f-mot-bloq.
      END.

end. /* ped-venda */

if (de-qtd-rep <> 0
or de-vlr-rep <> 0) THEN DO:
   DISPLAY "Total Repres:" @ ped-venda.no-ab-rep
           de-qtd-rep      @ de-qtd-conv
           de-vlr-rep      @ de-valor
           WITH FRAME f-detalhe.
   DOWN 2 WITH FRAME f-detalhe.
   assign de-qtd-rep = 0
          de-vlr-rep = 0.
END.

if tt-param.l-pula-pag then
   page.
else
   DOWN 1 WITH FRAME f-detalhe.
    
if de-qtd-ger <> 0
or de-vlr-ger <> 0 THEN DO:
   DISPLAY de-qtd-ger
           de-vlr-ger
           WITH FRAME f-tot-ger.
   DOWN WITH FRAME f-tot-ger.
END.

assign de-qtd-ger = 0
       de-vlr-ger = 0.

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

IF tt-param.impr-param THEN DO:
   PAGE.
   PUT "****-------------- PAR¶METROS ----------------****"
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
                                   ELSE "Resumido"
          tt-param.c-tipo-merc = IF tt-param.c-tipo-merc = "I"
                                    THEN "Interno"
                                    ELSE IF tt-param.c-tipo-merc = "E"
                                         THEN "Externo"
                                         ELSE "Todos"
          tt-param.c-tp-pedido = IF tt-param.c-tp-pedido = ""
                                 THEN "Todos" 
                                 ELSE tt-param.c-tp-pedido.
            
   DISPLAY tt-param.c-repres-ini      tt-param.c-repres-fim    
           tt-param.c-cliente-ini     tt-param.c-cliente-fim   
           tt-param.i-grupo-ini       tt-param.i-grupo-fim     
           tt-param.c-item-ini        tt-param.c-item-fim      
           tt-param.c-ref-ini         tt-param.c-ref-fim
           tt-param.da-entr-ini       tt-param.da-entr-fim     
           tt-param.da-impl-ini       tt-param.da-impl-fim     
           tt-param.da-can-ini        tt-param.da-can-fim      
           tt-param.de-percom-ini     tt-param.de-percom-fim
           tt-param.c-transp-ini      tt-param.c-transp-fim 
           tt-param.c-cond-pag        tt-param.c-saldo         
           tt-param.c-tipo-rel        tt-param.c-tipo-merc
           tt-param.c-tp-pedido       tt-param.l-ped-abe
           tt-param.l-ped-atp         tt-param.l-ped-att
           tt-param.l-ped-pen         tt-param.l-ped-sus
           tt-param.l-ped-can         tt-param.l-ped-out
           tt-param.l-item-abe        tt-param.l-item-atp
           tt-param.l-item-att        tt-param.l-item-pen
           tt-param.l-item-sus        tt-param.l-item-can
           tt-param.l-item-out        tt-param.l-crd-nava
           tt-param.l-crd-aval        tt-param.l-crd-aprv
           tt-param.l-crd-repr        tt-param.l-pula-pag   
           WITH FRAME f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.


