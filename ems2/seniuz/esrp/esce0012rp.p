/* Programa..: ESCE012.P
** Sistema...: Magnus da DATASUL S/A.
** Modulo....: Controle de Estoque
** Objetivo..: Listar o saldo disponivel estoques por itens de materiais
** Autor.....: Gilvando de Souza Araujo - Dezembro/95
**             Alterado em Jul/2001 por Fabio Coelho Lanza
**             Incluido as Colunas (COLECAO/SALDO VENDAS)
** Altera‡äes: Toninho - Fev/2006
**             Alterado para imprimir a Cor ao inv‚s da Cole‡Æo
** Obs.......: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Autor: Prodb - Toninho
**   Data: 07/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCE0012RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       FIELD c-est-ini        LIKE estabelec.cod-estabel
       FIELD c-est-fim        LIKE estabelec.cod-estabel
       FIELD c-item-ini       LIKE ITEM.it-codigo
       FIELD c-item-fim       LIKE ITEM.it-codigo
       FIELD c-ref-ini        LIKE ref-item.cod-refer
       FIELD c-ref-fim        LIKE ref-item.cod-refer
       FIELD desenho-ini      AS CHAR FORMAT "x(4)"
       FIELD desenho-fin      AS CHAR FORMAT "x(4)"
       FIELD variante-ini     AS CHAR FORMAT "x(1)"
       FIELD variante-fin     AS CHAR FORMAT "x(1)"
       FIELD c-cod-depos      LIKE deposito.cod-depos
       FIELD sit-lancamen     AS LOGICAL FORMAT "Sim/Nao"
       FIELD sit-foraprod     AS LOGICAL FORMAT "Sim/Nao"
       FIELD sit-emprod       AS LOGICAL FORMAT "Sim/Nao"
       FIELD sit-retalho      AS LOGICAL FORMAT "Sim/Nao"
       FIELD sit-exclusiv     AS LOGICAL FORMAT "Sim/Nao"
       FIELD sit-exportacao   AS LOGICAL FORMAT "Sim/Nao"
       FIELD l-teccru         AS LOGICAL
       FIELD l-sldneg         AS LOGICAL    
       FIELD l-sldzer         AS LOGICAL
       FIELD l-sldpos         AS LOGICAL
       FIELD c-opc-acond      AS CHAR
       FIELD c-opc-qualid     AS CHAR
       FIELD gerar-resumo     AS LOGICAL FORMAT "Sim/NÆo"
       FIELD c-opc-acab       AS CHAR
       FIELD c-opc-artigo     AS CHAR
       FIELD c-estoq-bloq     AS CHAR
       FIELD c-opc-rel        AS CHAR
       FIELD detalhado        AS LOGICAL FORMAT "Sim/NÆo"
       FIELD resumido         AS LOGICAL FORMAT "Sim/NÆo"
       FIELD de-sld-min       AS DEC FORMAT "->>,>>>,>>9.99" 
       FIELD l-pula-pag       AS LOGICAL
       FIELD gerar-excel      AS LOGICAL FORMAT "Sim/NÆo"
       FIELD arq-excel        AS CHAR FORMAT "x(45)"
       FIELD gerar-etiqueta   AS LOGICAL FORMAT "Sim/NÆo"
       FIELD arq-etiqueta     AS CHAR FORMAT "x(45)"
       FIELD impr-param       AS LOGICAL.

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

def temp-table tt-work
    field it-codigo  like saldo-estoq.it-codigo
    field cod-refer  like saldo-estoq.cod-refer
    field lote       like saldo-estoq.lote
    field quantidade like saldo-estoq.qtidade-atu
    field carteira   like saldo-estoq.qtidade-atu
    field reserva    like saldo-estoq.qtidade-atu
    field disponivel as dec format "->>,>>>,>>9.99"
    field venda      as dec format "->>,>>>,>>9.99"
    index ch-work it-codigo
                  cod-refer
                  lote.

DEF TEMP-TABLE tt-work1
    FIELD des-cor    AS CHAR FORMAT "x(5)" 
    FIELD desc-cor   LIKE ref-item-ext.cor
    FIELD disponivel AS DEC FORMAT "->>,>>>,>>9.99"
    FIELD venda      AS DEC FORMAT "->>,>>>,>>9.99"
    INDEX ch-work1 des-cor.

def var c-descricao    as char format "x(30)".
def var de-res-item    as dec.
def var de-car-item    as dec.
def var de-sld-p-item  as dec.
def var de-sld-p-geral as dec.
def var de-ven-p-item  as dec.
def var de-ven-p-geral as dec.
def var de-sld-r-item  as dec.
def var de-sld-r-geral as dec.
def var de-ven-r-item  as dec.
def var de-ven-r-geral as dec.
def var de-sld-o-item  as dec.
def var de-sld-o-geral as dec.
def var de-ven-o-geral as dec.
def var de-ven-o-item  as dec.
DEF VAR de-tot-disp    AS DEC FORMAT "->>,>>>,>>9.99".
DEF VAR de-tot-venda   AS DEC FORMAT "->>,>>>,>>9.99".


IF tt-param.gerar-etiqueta = YES THEN DO:
   DEF VAR x-linha        AS CHAR FORMAT "x(58)" EXTENT 6.
   DEF VAR i-cont         AS INT  INIT 1.
   DEF VAR i-lin          AS INT  INIT 1.
   DEF VAR c-refer-ant    AS CHAR FORMAT "x(6)".
   DEF VAR de-tot-qtd-ref AS DEC FORMAT "->>>,>>9.99".
   DEF STREAM arq-etiqueta.
END.

IF tt-param.gerar-excel THEN
   DEF STREAM arq-excel.

form
    tt-param.c-est-ini      LABEL "Estabelecimento.."
    "a" AT 37               
    tt-param.c-est-fim      NO-LABELS SKIP
    tt-param.c-item-ini     LABEL "Item............."
    "a" AT 37               
    tt-param.c-item-fim     NO-LABELS SKIP
    tt-param.c-ref-ini      LABEL "Referˆncia......."
    "a" AT 37               
    tt-param.c-ref-fim      NO-LABELS SKIP
    tt-param.desenho-ini    LABEL "Desenho.........."
    "a" AT 37               
    tt-param.desenho-fin    NO-LABELS SKIP
    tt-param.variante-ini   LABEL "Item............."
    "a" AT 37               
    tt-param.variante-fin   NO-LABELS SKIP
    tt-param.c-cod-depos    LABEL "Deposito........." SKIP
    tt-param.sit-lancamen   LABEL "Sit-Normal......." SKIP
    tt-param.sit-foraprod   LABEL "    Fora Producao" SKIP
    tt-param.sit-emprod     LABEL "    Em Producao.." SKIP
    tt-param.sit-retalho    LABEL "    Retalho......" SKIP
    tt-param.sit-exclusiv   LABEL "    Exclusividade" SKIP
    tt-param.sit-exportacao LABEL "    Exporta‡Æo..." SKIP
    tt-param.detalhado      LABEL "Detalhado........" SKIP
    tt-param.resumido       LABEL "Resumido........." SKIP
    tt-param.de-sld-min     LABEL "Sld Minimo......." SKIP
    tt-param.gerar-excel    LABEL "Gerar p/Excel...." SKIP
    tt-param.arq-excel      LABEL "Arquivo Excel...." SKIP
    tt-param.gerar-etiqueta LABEL "Gerar Etiqueta..." SKIP
    tt-param.arq-etiqueta   LABEL "Arquivo Etiqueta." SKIP(1)
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

form
    tt-work.it-codigo     column-label "Item" format "x(6)" 
    c-descricao           column-label "Descricao"
    item.un               column-label "Un"
    tt-work.cod-refer     column-label "Refer."      
    tt-work.lote          column-label "Lote"
    ref-item-ext.cor      column-label "Cor/Des" format "x(35)"
    tt-work.disponivel    column-label "Saldo Dispon" 
    tt-work.venda         column-label "Saldo Venda"
    with no-box 55 down width 132 STREAM-IO frame f-detalhe.

FORM 
    tt-work1.des-cor       COLUMN-LABEL "Des/Cor" FORMAT "XXXX-X"
    tt-work1.desc-cor      COLUMN-LABEL "Descricao"
    tt-work1.disponivel    column-label "Saldo Dispon" 
    tt-work1.venda         column-label "Saldo Venda"
    with no-box 55 down width 132 STREAM-IO frame f-detalhe1.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESTOQUE * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Listagem_Estoque_Disponivel_por_Item * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

IF tt-param.gerar-excel THEN DO:
   OUTPUT STREAM arq-excel TO VALUE(tt-param.arq-excel).
   PUT STREAM arq-excel "Item;" "Descricao;" "Un;" "Refer.;" "Lote;" 
                        "Cor/Des;" "Saldo Dispon;" "Saldo Venda"
                        SKIP.
END.
IF tt-param.gerar-etiqueta THEN DO:
   OUTPUT STREAM arq-etiqueta TO VALUE(tt-param.arq-etiqueta).
   PUT STREAM arq-etiqueta "Linha1;" "Linha2;" "Linha3;" 
                           "Linha4;" "Linha5;" "Linha6"
                           SKIP.
END.

FOR EACH ITEM WHERE item.it-codigo >= tt-param.c-item-ini 
                AND item.it-codigo <= tt-param.c-item-fim
                AND (if tt-param.l-teccru = yes
                    THEN SUBSTR(item.it-codigo,6,1) >= "0"
                    ELSE SUBSTR(item.it-codigo,6,1) <> "0") NO-LOCK,
    each item-ext 
         where item-ext.it-codigo = item.it-codigo
           and ((item-ext.indigo = yes and tt-param.c-opc-artigo = "I") or
                (item-ext.indigo = no  and tt-param.c-opc-artigo = "O") or
                                           tt-param.c-opc-artigo = "A")
                  no-lock:
    
    run pi-acompanhar in h-acomp (INPUT item.it-codigo + "Lendo Movto-Estoq").
    
    for each saldo-estoq where
             saldo-estoq.cod-estabel >= tt-param.c-est-ini AND
             saldo-estoq.cod-estabel <= tt-param.c-est-fim AND
             saldo-estoq.it-codigo    = item.it-codigo AND
             saldo-estoq.cod-refer   >= tt-param.c-ref-ini AND
             saldo-estoq.cod-refer   <= tt-param.c-ref-fim AND 
             SUBSTR(saldo-estoq.cod-refer,3,4) >= tt-param.desenho-ini AND
             SUBSTR(saldo-estoq.cod-refer,3,4) <= tt-param.desenho-fin AND
             SUBSTR(saldo-estoq.cod-refer,7,1) >= tt-param.variante-ini AND
             SUBSTR(saldo-estoq.cod-refer,7,1) <= tt-param.variante-fin AND
             ((SUBSTR(saldo-estoq.cod-refer,7,1) =  "0" AND tt-param.c-opc-acab = "L") OR
              (SUBSTR(saldo-estoq.cod-refer,7,1) <> "0" AND tt-param.c-opc-acab = "E") OR
              (tt-param.c-opc-acab = "A")) AND
             saldo-estoq.cod-depos    = tt-param.c-cod-depos no-lock:

       IF saldo-estoq.qtidade-atu = 0 THEN NEXT.

       if (tt-param.c-opc-acond = "P" and
           substr(saldo-estoq.lote,1,1) <> "P") or
          (tt-param.c-opc-acond = "R" and
           substr(saldo-estoq.lote,1,1) <> "R") then next.

       if (tt-param.c-opc-qualid = "P" and
           substr(saldo-estoq.lote,2,1) <> "P") or
          (tt-param.c-opc-qualid = "D" and
           substr(saldo-estoq.lote,2,1) <> "D") then next.
       
       find tt-work where tt-work.it-codigo = item.it-codigo
                      and tt-work.cod-refer = saldo-estoq.cod-refer
                      and tt-work.lote      = saldo-estoq.lote
                    no-lock no-error.
       if not avail tt-work then do:
          create tt-work.
          assign tt-work.it-codigo = item.it-codigo
                 tt-work.cod-refer = saldo-estoq.cod-refer
                 tt-work.lote      = saldo-estoq.lote.
       end.
       assign tt-work.quantidade = tt-work.quantidade +
                                   saldo-estoq.qtidade-atu.
    end.
  
    run pi-acompanhar in h-acomp (INPUT item.it-codigo + " Lendo Reservas").

    for each ped-item-res
       where ped-item-res.it-codigo =  item.it-codigo
         and ped-item-res.cod-refer >= tt-param.c-ref-ini
         and ped-item-res.cod-refer <= tt-param.c-ref-fim
         AND SUBSTR(ped-item-res.cod-refer,3,4) >= tt-param.desenho-ini 
         AND SUBSTR(ped-item-res.cod-refer,3,4) <= tt-param.desenho-fin 
         AND SUBSTR(ped-item-res.cod-refer,7,1) >= tt-param.variante-ini
         AND SUBSTR(ped-item-res.cod-refer,7,1) <= tt-param.variante-fin
       no-lock:
       
       if (tt-param.c-opc-acond = "P" and
           substr(ped-item-res.lote,1,1) <> "P") or
          (tt-param.c-opc-acond = "R" and
           substr(ped-item-res.lote,1,1) <> "R") then next.

       if (tt-param.c-opc-qualid = "P" and
           substr(ped-item-res.lote,2,1) <> "P") or
          (tt-param.c-opc-qualid = "D" and
           substr(ped-item-res.lote,2,1) <> "D") then next.

       ASSIGN de-res-item = 0.
       IF ped-item-res.faturado = yes THEN DO:
          find nota-fiscal where
               nota-fiscal.cod-estabel = ped-item-res.cod-estabel and
               nota-fiscal.serie       = ped-item-res.serie and
               nota-fiscal.nr-nota-fis = string(ped-item-res.nr-nota-fis,"9999999") 
               no-lock no-error.

           if avail nota-fiscal then
              if  nota-fiscal.ind-sit-nota < 6
              AND nota-fiscal.dt-confirma = ?
              and nota-fiscal.dt-cancela = ? then
                  assign de-res-item = de-res-item + ped-item-res.qt-pedida.
       end.
       else
          assign de-res-item = de-res-item + ped-item-res.qt-pedida.

       IF de-res-item <> 0 THEN DO:
          find tt-work where tt-work.it-codigo = item.it-codigo
                         and tt-work.cod-refer = ped-item-res.cod-refer
                         and tt-work.lote      = ped-item-res.lote
                       no-lock no-error.
          if not avail tt-work then do:
             create tt-work.
             assign tt-work.it-codigo = item.it-codigo
                    tt-work.cod-refer = ped-item-res.cod-refer
                    tt-work.lote      = ped-item-res.lote.
          end.
          assign tt-work.reserva = tt-work.reserva + de-res-item.
       END.
    END.
           
    run pi-acompanhar in h-acomp (INPUT item.it-codigo + " Lendo Carteira").

    /* --- Pesquisa Carteira --- */
    FOR EACH ped-item
       WHERE ped-item.it-codigo =  item.it-codigo
         AND ped-item.cod-refer >= tt-param.c-ref-ini
         AND ped-item.cod-refer <= tt-param.c-ref-fim 
         AND SUBSTR(ped-item.cod-refer,3,4) >= tt-param.desenho-ini 
         AND SUBSTR(ped-item.cod-refer,3,4) <= tt-param.desenho-fin 
         AND SUBSTR(ped-item.cod-refer,7,1) >= tt-param.variante-ini
         AND SUBSTR(ped-item.cod-refer,7,1) <= tt-param.variante-fin
        no-lock,
        EACH ped-item-ext 
        WHERE ped-item-ext.nome-abrev   = ped-item.nome-abrev
          AND ped-item-ext.nr-pedcli    = ped-item.nr-pedcli
          AND ped-item-ext.nr-sequencia = ped-item.nr-sequencia
          AND ped-item-ext.it-codigo    = ped-item.it-codigo
          AND ped-item-ext.cod-refer    = ped-item.cod-refer
        NO-LOCK:

        FIND ped-item-res WHERE
             ped-item-res.nome-abrev   = ped-item.nome-abrev AND
             ped-item-res.nr-pedcli    = ped-item.nr-pedcli  AND
             ped-item-res.it-codigo    = ped-item.it-codigo  AND
             ped-item-res.cod-refer    = ped-item.cod-refer  AND
             ped-item-res.nr-sequencia = ped-item.nr-sequencia 
             NO-LOCK NO-ERROR.

        IF AVAIL ped-item-res THEN NEXT.

        if (c-opc-acond = "P" and
            substr(ped-item-ext.lote,1,1) <> "P") or
           (c-opc-acond = "R" and
            substr(ped-item-ext.lote,1,1) <> "R") then next.               

        if (c-opc-qualid = "P" and
            substr(ped-item-ext.lote,2,1) <> "P") or
           (c-opc-qualid = "D" and
            substr(ped-item-ext.lote,2,1) <> "D") then next.

        IF ped-item.cod-sit-item = 3 OR
           ped-item.cod-sit-item = 6 THEN NEXT.

        assign de-car-item = ped-item.qt-pedida -
                             ped-item.qt-atendida +
                             ped-item.qt-devolvida.

        find tt-work where tt-work.it-codigo = item.it-codigo
                       and tt-work.cod-refer = ped-item.cod-refer
                       AND tt-work.lote      = ped-item-ext.lote
                     no-lock no-error.
        if not avail tt-work then do:
           create tt-work.
           assign tt-work.it-codigo = item.it-codigo
                  tt-work.cod-refer = ped-item.cod-refer
                  tt-work.lote      = ped-item-ext.lote.
        end.
        assign tt-work.carteira = tt-work.carteira + de-car-item.

    end. /* ped-item */
end.

FOR EACH tt-work:
    ASSIGN tt-work.disponivel = tt-work.quantidade - tt-work.reserva
           tt-work.venda      = tt-work.disponivel - tt-work.carteira. 

    if tt-work.venda <= 0 and tt-param.c-opc-rel = "V" then do:
       delete tt-work.
       next.
    end.
    overlay(tt-work.lote,1,2) = upper(substr(tt-work.lote,1,2)).
    if substr(tt-work.lote,2,1) = "D" then
       overlay(tt-work.lote,2,1) = lower(substr(tt-work.lote,2,1)).
END.

for each tt-work where
        (tt-param.l-sldneg = no and tt-work.disponivel >= 0 or
         tt-param.l-sldneg = yes) and
        (tt-param.l-sldzer = no and tt-work.disponivel <> 0 or
         tt-param.l-sldzer = yes) and
        (tt-param.l-sldpos = no and tt-work.disponivel <= 0 or
         tt-param.l-sldpos = yes) and
        (tt-work.venda >= tt-param.de-sld-min or tt-param.de-sld-min = 0),
    each ref-item-ext 
         WHERE ref-item-ext.cod-refer    =  tt-work.cod-refer
           AND ref-item-ext.it-codigo    =  tt-work.it-codigo
           AND ((ref-item-ext.cod-obsoleto = "0" AND tt-param.sit-lancamen = YES) OR
                (ref-item-ext.cod-obsoleto = "1" AND tt-param.sit-foraprod = YES) OR
                (ref-item-ext.cod-obsoleto = "2" AND tt-param.sit-emprod   = YES) OR
                (ref-item-ext.cod-obsoleto = "3" AND tt-param.sit-retalho  = YES) OR
                (ref-item-ext.cod-obsoleto = "4" AND tt-param.sit-exclusiv = YES) OR
                (ref-item-ext.cod-obsoleto = "5" AND tt-param.sit-exportacao = YES))
           AND ((ref-item-ext.bloqueio-pp = YES AND tt-work.lote BEGINS "pp" AND tt-param.c-estoq-bloq = "B") OR
                (ref-item-ext.bloqueio-pp = NO  AND tt-work.lote BEGINS "pp" AND tt-param.c-estoq-bloq = "L") OR
                (ref-item-ext.bloqueio-pd = YES AND tt-work.lote BEGINS "pd" AND tt-param.c-estoq-bloq = "B") OR
                (ref-item-ext.bloqueio-pd = NO  AND tt-work.lote BEGINS "pd" AND tt-param.c-estoq-bloq = "L") OR
                (ref-item-ext.bloqueio-rp = YES AND tt-work.lote BEGINS "rp" AND tt-param.c-estoq-bloq = "B") OR
                (ref-item-ext.bloqueio-rp = NO  AND tt-work.lote BEGINS "rp" AND tt-param.c-estoq-bloq = "L") OR
                (ref-item-ext.bloqueio-rd = YES AND tt-work.lote BEGINS "rd" AND tt-param.c-estoq-bloq = "B") OR
                (ref-item-ext.bloqueio-rd = NO  AND tt-work.lote BEGINS "rd" AND tt-param.c-estoq-bloq = "L") OR
                                                                                (tt-param.c-estoq-bloq = "A"))
         NO-LOCK
    break by tt-work.it-codigo
          by tt-work.cod-refer
          by tt-work.lote:
   run pi-acompanhar in h-acomp (INPUT "Imprimindo" + tt-work.it-codigo).

   FIND tt-work1 WHERE tt-work1.des-cor = SUBSTR(tt-work.cod-refer,3,5)
              NO-LOCK NO-ERROR.
   if not avail tt-work1 then do:
      create tt-work1.
      assign tt-work1.des-cor = SUBSTR(tt-work.cod-refer,3,5).
   end.
   assign tt-work1.desc-cor   = IF AVAIL ref-item-ext THEN ref-item-ext.cor
                                                      ELSE ""
          tt-work1.disponivel = tt-work1.disponivel + tt-work.disponivel
          tt-work1.venda      = tt-work1.venda + tt-work.venda.

   if first-of(tt-work.it-codigo) then do:
      find item where item.it-codigo = tt-work.it-codigo no-lock.
      assign c-descricao = item.descricao-1 + item.descricao-2.
                   
      IF tt-param.detalhado OR 
         tt-param.resumido THEN
         display tt-work.it-codigo
                 c-descricao
                 item.un
                 with frame f-detalhe.

      IF tt-param.gerar-excel THEN
         PUT STREAM arq-excel
                    tt-work.it-codigo ";"  
                    c-descricao ";"      
                    item.un ";".           
   end.
   ELSE DO:
      IF tt-param.gerar-excel THEN
         PUT STREAM arq-excel
                    " ;"  
                    " ;"      
                    " ;".           
   END.
   
   IF tt-param.gerar-etiqueta THEN DO:
      IF i-lin > 6 THEN DO:
         DO i-cont = 1 TO 6:
            PUT STREAM arq-etiqueta x-linha[i-cont] ";".
            ASSIGN x-linha[i-cont] = "".
         END.
         PUT STREAM arq-etiqueta 
                    SKIP.
         ASSIGN i-lin = 1.
      END.
      IF SUBSTR(tt-work.cod-refer,1,6) <> c-refer-ant AND c-refer-ant <> "" THEN DO:
         ASSIGN x-linha[i-lin] = "Total: " + STRING(FILL(" ",40),"x(40)") +
                                 STRING(de-tot-qtd-ref,"->>>,>>9.99").
         DO i-cont = 1 TO 6:
            PUT STREAM arq-etiqueta x-linha[i-cont] ";".
            ASSIGN x-linha[i-cont] = "".
         END.
         PUT STREAM arq-etiqueta 
                    SKIP.
         ASSIGN i-lin          = 1
                de-tot-qtd-ref = 0.
      END.
      
      ASSIGN x-linha[i-lin] = SUBSTR(tt-work.it-codigo,1,6) + " " +
                              SUBSTR(c-descricao,1,27) + " " +
                              STRING(ITEM.un,"xx") + " "
             c-refer-ant    = SUBSTR(tt-work.cod-refer,1,6).
   END.
   
   if tt-work.lote begins "p" then
      assign de-sld-p-item  = de-sld-p-item  + tt-work.disponivel
             de-sld-p-geral = de-sld-p-geral + tt-work.disponivel
             de-ven-p-item  = de-ven-p-item  + tt-work.venda
             de-ven-p-geral = de-ven-p-geral + tt-work.venda.
   else
   if tt-work.lote begins "r" then
      assign de-sld-r-item  = de-sld-r-item  + tt-work.disponivel
             de-sld-r-geral = de-sld-r-geral + tt-work.disponivel
             de-ven-r-item  = de-ven-r-item  + tt-work.venda
             de-ven-r-geral = de-ven-r-geral + tt-work.venda.
   else
      assign de-sld-o-item  = de-sld-o-item  + tt-work.disponivel
             de-sld-o-geral = de-sld-o-geral + tt-work.disponivel
             de-ven-o-item  = de-ven-o-item  + tt-work.venda
             de-ven-o-geral = de-ven-o-geral + tt-work.venda.
   
   IF tt-param.detalhado THEN DO:
      display tt-work.cod-refer
              tt-work.lote
              ref-item-ext.cor
              tt-work.disponivel  when tt-param.c-opc-rel <> "V" 
              tt-work.venda       when tt-param.c-opc-rel <> "D"
              with frame f-detalhe.
      down with frame f-detalhe.
   END.

   IF tt-param.gerar-excel THEN DO:
      PUT STREAM arq-excel
                 tt-work.cod-refer ";"  
                 tt-work.lote ";"      
                 ref-item-ext.cor ";".
      IF tt-param.c-opc-rel <> "V" THEN
         PUT STREAM arq-excel
                    tt-work.disponivel
                    ";".
      ELSE
         PUT STREAM arq-excel " ;".

      IF tt-param.c-opc-rel <> "D" THEN
         PUT STREAM arq-excel
                    tt-work.venda.
      ELSE
         PUT STREAM arq-excel " ".
      PUT STREAM arq-excel SKIP.
   END.

   IF tt-param.gerar-etiqueta THEN DO:
      ASSIGN x-linha[i-lin] = x-linha[i-lin] + SUBSTR(tt-work.lote,1,9).
             x-linha[i-lin] = x-linha[i-lin] + STRING(tt-work.venda,"->>>,>>9.99").
      ASSIGN de-tot-qtd-ref = de-tot-qtd-ref + tt-work.venda
             i-lin = i-lin + 1.
   END.

   if last-of(tt-work.it-codigo) then do:
      IF tt-param.detalhado or
         tt-param.resumido THEN DO:
         display "Total Item"  WHEN tt-param.detalhado = YES
                               @ c-descricao 
                 "Peca"        @ tt-work.lote
                 de-sld-p-item when c-opc-rel <> "V" 
                               @ tt-work.disponivel
                 de-ven-p-item when c-opc-rel <> "D"
                               @ tt-work.venda
                 with frame f-detalhe.
         down with frame f-detalhe.
      END.

      IF tt-param.gerar-excel THEN DO:
         PUT STREAM arq-excel
                    " ;"
                    "Total Item;"
                    " ;"
                    " ;"
                    "Peca;"
                    " ;".
         IF tt-param.c-opc-rel <> "V" THEN
            PUT STREAM arq-excel
                       de-sld-p-item
                       ";".
         ELSE
            PUT STREAM arq-excel " ;".
         
         IF tt-param.c-opc-rel <> "D" THEN
            PUT STREAM arq-excel
                       de-ven-p-item.
         ELSE
            PUT STREAM arq-excel " ".
         PUT STREAM arq-excel SKIP.
      END.

      IF tt-param.detalhado OR 
         tt-param.resumido THEN DO:
         display "Rolo"         @ tt-work.lote
                 de-sld-r-item  when c-opc-rel <> "V"
                                @ tt-work.disponivel
                 de-ven-r-item  when c-opc-rel <> "D"
                                @ tt-work.venda
                 with frame f-detalhe.
         down with frame f-detalhe.
      END.
      
      IF tt-param.gerar-excel THEN DO:
         PUT STREAM arq-excel
                    " ;"
                    " ;"
                    " ;"
                    " ;"
                    "Rolo;"
                    " ;".
         IF tt-param.c-opc-rel <> "V" THEN
            PUT STREAM arq-excel
                       de-sld-r-item
                       ";".
         ELSE
            PUT STREAM arq-excel " ;".

         IF tt-param.c-opc-rel <> "D" THEN
            PUT STREAM arq-excel
                       de-ven-r-item.
         ELSE
            PUT STREAM arq-excel " ".
         PUT STREAM arq-excel SKIP.
      END.
 
      IF tt-param.detalhado OR 
         tt-param.resumido THEN DO:
         display "Ret."         @ tt-work.lote
                 de-sld-o-item  when c-opc-rel <> "V"
                                @ tt-work.disponivel
                 de-ven-o-item  when c-opc-rel <> "D"
                                @ tt-work.venda
                 with frame f-detalhe.
         down 2 with frame f-detalhe.
      END.

      IF tt-param.gerar-excel THEN DO:
         PUT STREAM arq-excel
                    " ;"
                    " ;"
                    " ;"
                    " ;"
                    "Ret.;"
                    " ;".
         IF tt-param.c-opc-rel <> "V" THEN
            PUT STREAM arq-excel
                       de-sld-o-item
                       ";".
         ELSE
            PUT STREAM arq-excel " ;".

         IF tt-param.c-opc-rel <> "D" THEN
            PUT STREAM arq-excel
                       de-ven-o-item.
         ELSE
            PUT STREAM arq-excel " ".
         PUT STREAM arq-excel SKIP(1).
      END.
      
      assign de-sld-p-item = 0
             de-ven-p-item = 0
             de-sld-r-item = 0
             de-ven-r-item = 0
             de-sld-o-item = 0
             de-ven-o-item = 0.

      IF tt-param.l-pula-pag THEN PAGE.
   end.
end.

IF tt-param.detalhado OR 
   tt-param.resumido THEN DO:
   display "Total Geral"  @ c-descricao
           "Peca"         @ tt-work.lote
           de-sld-p-geral @ tt-work.disponivel
           de-ven-p-geral @ tt-work.venda
           with frame f-detalhe.
   down with frame f-detalhe.
END.
      
IF tt-param.gerar-excel THEN DO:
   PUT STREAM arq-excel
              " ;"
              "Total Geral;"
              " ;"
              " ;"
              "Peca;"
              " ;".
   IF tt-param.c-opc-rel <> "V" THEN
      PUT STREAM arq-excel
                 de-sld-p-geral
                 ";".
   ELSE
      PUT STREAM arq-excel " ;".

   IF tt-param.c-opc-rel <> "D" THEN
      PUT STREAM arq-excel
                 de-ven-p-geral
                 ";".
   ELSE
      PUT STREAM arq-excel " ;".
   PUT STREAM arq-excel SKIP.
END.

IF tt-param.detalhado OR
   tt-param.resumido THEN DO:
   display "Rolo"         @ tt-work.lote
           de-sld-r-geral @ tt-work.disponivel
           de-ven-r-geral @ tt-work.venda
           with frame f-detalhe.
   down with frame f-detalhe.
END.

IF tt-param.gerar-excel THEN DO:
   PUT STREAM arq-excel
              " ;"
              " ;"
              " ;"
              " ;"
              "Rolo;"
              " ;".
   IF tt-param.c-opc-rel <> "V" THEN
      PUT STREAM arq-excel
                 de-sld-r-geral
                 ";".
   ELSE
      PUT STREAM arq-excel " ;".
   
   IF tt-param.c-opc-rel <> "D" THEN
      PUT STREAM arq-excel
                 de-ven-r-geral.
   ELSE
      PUT STREAM arq-excel " ".
   PUT STREAM arq-excel SKIP.
END.

IF tt-param.detalhado OR 
   tt-param.resumido THEN
   display "Ret."         @ tt-work.lote
           de-sld-o-geral @ tt-work.disponivel
           de-ven-o-geral @ tt-work.venda
           with frame f-detalhe.

IF tt-param.gerar-excel THEN DO:
   PUT STREAM arq-excel
              " ;"
              " ;"
              " ;"
              " ;"
              "Ret.;"
              " ;".
   IF tt-param.c-opc-rel <> "V" THEN
      PUT STREAM arq-excel
                 de-sld-o-geral
                 ";".
   ELSE
      PUT STREAM arq-excel " ;".

   IF tt-param.c-opc-rel <> "D" THEN
      PUT STREAM arq-excel
                 de-ven-o-geral
                 ";".
   ELSE
      PUT STREAM arq-excel " ;".
   PUT STREAM arq-excel SKIP.
END.

IF tt-param.gerar-excel THEN
   OUTPUT STREAM arq-excel CLOSE.

IF tt-param.gerar-resumo THEN DO:
   PAGE.
   FOR EACH tt-work1:
       DISPLAY tt-work1.des-cor
               tt-work1.desc-cor
               tt-work1.disponivel
               tt-work1.venda
               WITH FRAME f-detalhe1.
       DOWN WITH FRAME f-detalhe1.
       ASSIGN de-tot-disp  = de-tot-disp + tt-work1.disponivel
              de-tot-venda = de-tot-venda + tt-work1.venda.
   END.
   DISPLAY "Total"      @ tt-work1.des-cor
           de-tot-disp  @ tt-work1.disponivel
           de-tot-venda @ tt-work1.venda
           WITH FRAME f-detalhe1.
END.

IF tt-param.impr-param THEN DO:
   PAGE.
   PUT "*****----- PAR¶METROS ------*****"
       SKIP(1).
    
   display tt-param.c-est-ini
           tt-param.c-est-fim
           tt-param.c-item-ini
           tt-param.c-item-fim
           tt-param.c-ref-ini
           tt-param.c-ref-fim
           tt-param.desenho-ini
           tt-param.desenho-fin
           tt-param.c-cod-depos
           tt-param.sit-lancamen  
           tt-param.sit-foraprod
           tt-param.sit-emprod  
           tt-param.sit-retalho 
           tt-param.sit-exclusiv
           tt-param.sit-exportacao
           tt-param.detalhado
           tt-param.resumido
           tt-param.de-sld-min 
           tt-param.gerar-excel
           tt-param.arq-excel
           tt-param.gerar-etiqueta
           tt-param.arq-etiqueta
           with frame f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

