/* Programa: ESCE036.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Controle de Estoque
** Objetivo: Relatorio Producao/Defeitos por Item
** Autor...: Gilvando de Souza Araujo - Outubro/1997
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESCE036.P  =>  ESSP0071RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 03/02/2004
**
** Alterado..: F bio Coelho Lanza
** Data......: 02/09/2005
**             Foi retirado 2 colunas "Sobra" e "%Prd" e inserido a 
**             coluna "Cod.Item"  
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0071RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param  NO-UNDO
       FIELD destino            AS INTEGER
       FIELD arquivo            AS CHAR FORMAT "x(35)"
       FIELD usuario            AS CHAR FORMAT "x(12)"
       FIELD data-exec          AS DATE
       FIELD hora-exec          AS INTEGER
       FIELD classifica         AS INTEGER
       FIELD desc-classifica    AS CHAR FORMAT "x(40)"
       FIELD fi-ini-cod-estabel LIKE mov-est-acbm.cod-estabel
       FIELD fi-fin-cod-estabel LIKE mov-est-acbm.cod-estabel
       FIELD fi-ini-it-codigo   like mov-est-acbm.it-codigo
       FIELD fi-fin-it-codigo   like mov-est-acbm.it-codigo
       FIELD fi-ini-cod-refer   LIKE mov-est-acbm.cod-refer
       FIELD fi-fin-cod-refer   LIKE mov-est-acbm.cod-refer
       FIELD fi-desenho         AS CHAR FORMAT "x(4)"
       FIELD l-inc-exc          AS LOG FORMAT "Inclusive/Exclusive"
       FIELD fi-ini-data-mov    LIKE mov-est-acbm.data-mov
       FIELD fi-fin-data-mov    LIKE mov-est-acbm.data-mov
       FIELD fi-ini-num-lote    LIKE mov-est-acbm.num-lote
       FIELD fi-fin-num-lote    LIKE mov-est-acbm.num-lote
       FIELD opc-artigo         AS CHAR FORMAT "x"
       FIELD l-excluir-ob       AS LOG FORMAT "Sim/NÆo"
       FIELD cam-nom-lista      AS CHAR FORMAT "x(45)"
       FIELD tp-tecelagem       AS CHAR
       FIELD l-som-reg-perf     AS LOG FORMAT "Sim/NÆo"
       FIELD l-resumo-qualid    AS LOG FORMAT "Sim/NÆo"
       FIELD imp-param          AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

DEFINE TEMP-TABLE tt-resumo-qualid
       FIELD cod-qualid LIKE ob-etiqueta.cod-qualid
       FIELD quantidade AS DEC
       INDEX ch-qualid cod-qualid.

DEF TEMP-TABLE tt-work
    FIELD it-codigo LIKE mov-est-acbm.it-codigo 
    FIELD qtd-total AS DEC
    INDEX ch-work it-codigo.

DEF TEMP-TABLE tt-excluir-ob
    FIELD num-lote LIKE mov-est-acbm.num-lote
    INDEX ch-excluir-ob num-lote.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var de-tot-prd-it  as dec format ">>>>>>,>>9".
def var de-tot-prf-it  as dec format ">>>>>,>>9".
def var de-tot-reg-it  as dec format ">>>>,>>9".
def var de-tot-ld-it   as dec format ">>>>,>>9".
def var de-tot-ret-it  as dec format ">>>>,>>9".
def var de-tot-sob-it  as dec format ">>>>,>>9".
def var de-tot-def-it  as dec format ">>>>,>>9".
def var de-tot-prd-ger as dec format ">>>>>>,>>9".
def var de-tot-prf-ger as dec format ">>>>>,>>9".
def var de-tot-reg-ger as dec format ">>>>,>>9".
def var de-tot-ld-ger  as dec format ">>>>,>>9".
def var de-tot-ret-ger as dec format ">>>>,>>9".
def var de-tot-sob-ger as dec format ">>>>,>>9".
def var de-tot-def-ger as dec format ">>>>,>>9".
def var de-aux-perc1 as dec format ">>9.9".
def var de-aux-perc2 as dec format ">>9.9".
def var de-aux-perc3 as dec format ">>9.9".
def var de-aux-perc4 as dec format ">>9.9".
def var de-aux-perc5 as dec format ">9.9".
def var de-aux-perc6 as dec format ">>9.9".

DEF STREAM aux.

form 
    "*-------------- Parƒmetros/Sele‡Æo ---------------*" SKIP
    tt-param.fi-ini-cod-estabel label "Estabelecimento.."
    "A"  AT 36
    tt-param.fi-fin-cod-estabel NO-LABELS SKIP
    tt-param.fi-ini-it-codigo   label "Item............."
    "A"  AT 36
    tt-param.fi-fin-it-codigo   NO-LABELS SKIP
    tt-param.fi-ini-cod-refer   label "Referˆncia......."
    "A"  AT 36
    tt-param.fi-fin-cod-refer   NO-LABELS SKIP
    tt-param.fi-desenho         LABEL "Desenho.........."
    " "  AT 24
    tt-param.l-inc-exc          NO-LABELS SKIP
    tt-param.fi-ini-data-mov    label "Data do Movimento"
    "A"  AT 36
    tt-param.fi-fin-data-mov    NO-LABELS SKIP
    tt-param.fi-ini-num-lote    label "N£mero do Lote..." 
    "A"  AT 36
    tt-param.fi-fin-num-lote    NO-LABELS SKIP
    tt-param.l-excluir-ob       LABEL "Excluir OB......." SKIP
    tt-param.l-som-reg-perf     LABEL "Somar Reg + Perf." SKIP
    tt-param.l-resumo-qualid    LABEL "Resumo Qualidade." SKIP
    tt-param.opc-artigo         LABEL "Tipos de Artigos." SKIP
    tt-param.tp-tecelagem       LABEL "Tecelagem........" FORMAT "x(50)"
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    ITEM.it-codigo     LABEL "Item"      FORMAT "x(6)"
    item.desc-item     label "Descricao" FORMAT "x(30)"
    de-tot-prd-it      label "Producao"
    de-tot-prf-it      label "Perfeito"
    de-aux-perc1       label "% Prd"
    de-tot-reg-it      label "Regular"
    de-aux-perc2       label "%Prd"
    de-tot-ld-it       label "Leve Def"
    de-aux-perc3       label "%Prd"
    de-tot-ret-it      label "Retalho"
    de-aux-perc4       label "%Prd"
/*  de-tot-sob-it      label "Sobra"
    de-aux-perc5       label "%Prd"  */
    de-tot-def-it      label "Total Def"
    de-aux-perc6       label "%Prd"
    ITEM.peso-bruto    LABEL "PesRet" FORMAT "9.9999"
    with no-box 55 down width 132 STREAM-IO frame f-detalhe no-label.

FORM
    tt-resumo-qualid.cod-qualid LABEL "Cod"
    qualid-tecido.descricao     LABEL "Descri‡Æo"
    de-aux-perc1                LABEL "% Prod."
    WITH NO-BOX 55 DOWN WIDTH 132 STREAM-IO FRAME f-res-qualid NO-LABEL.

IF tt-param.l-excluir-ob = YES THEN DO:
   INPUT STREAM aux FROM VALUE(tt-param.cam-nom-lista) CONVERT SOURCE "ibm850". 
   SET STREAM aux ^.
   REPEAT:
      CREATE tt-excluir-ob.
      IMPORT STREAM aux DELIMITER ";" tt-excluir-ob.
   END.
   INPUT STREAM aux CLOSE.
   FOR EACH tt-excluir-ob WHERE tt-excluir-ob.num-lote = 0:
      DELETE tt-excluir-ob.  
   END.
END.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESPECIFICO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Relatorio_de_Produ‡Æo/Defeitos_por_Item * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each mov-est-acbm
    WHERE mov-est-acbm.cod-estabel >= tt-param.fi-ini-cod-estabel  
      AND mov-est-acbm.cod-estabel <= tt-param.fi-fin-cod-estabel
      AND mov-est-acbm.data-mov    >= tt-param.fi-ini-data-mov  
      AND mov-est-acbm.data-mov    <= tt-param.fi-fin-data-mov  
      AND mov-est-acbm.it-codigo   >= tt-param.fi-ini-it-codigo 
      AND mov-est-acbm.it-codigo   <= tt-param.fi-fin-it-codigo 
      AND mov-est-acbm.cod-refer   >= tt-param.fi-ini-cod-refer
      AND mov-est-acbm.cod-refer   <= tt-param.fi-fin-cod-refer
      AND mov-est-acbm.num-lote    >= tt-param.fi-ini-num-lote
      AND mov-est-acbm.num-lote    <= tt-param.fi-fin-num-lote
      AND INDEX(tt-param.tp-tecelagem,mov-est-acbm.tipo-tear) <> 0 
      AND ((substr(mov-est-acbm.cod-refer,3,4) =  tt-param.fi-desenho AND tt-param.l-inc-exc = yes) or 
           (substr(mov-est-acbm.cod-refer,3,4) <> tt-param.fi-desenho and tt-param.l-inc-exc = no) or 
           (tt-param.fi-desenho = ""))
      AND NOT CAN-FIND(tt-excluir-ob WHERE tt-excluir-ob.num-lote = mov-est-acbm.num-lote)
    NO-LOCK,
    EACH item-ext WHERE item-ext.it-codigo = mov-est-acbm.it-codigo
                    AND (item-ext.indigo = YES AND tt-param.opc-artigo = "I" OR
                         item-ext.indigo = NO  AND tt-param.opc-artigo = "O" OR
                                                   tt-param.opc-artigo = "A")
                  NO-LOCK:
    
    run pi-acompanhar in h-acomp (input "Inicial - Item: " + mov-est-acbm.it-codigo + " Data: " +
                                        STRING(mov-est-acbm.data-mov)).

   /*----- Cria arquivo tempor rio para auxiliar na classifica‡Æo do relat¢rio -----*/
   FIND FIRST tt-work WHERE tt-work.it-codigo = mov-est-acbm.it-codigo  
                      NO-LOCK NO-ERROR.
   IF NOT AVAIL tt-work THEN DO:
      CREATE tt-work.
      ASSIGN tt-work.it-codigo = mov-est-acbm.it-codigo.
   END.
   ASSIGN tt-work.qtd-total = tt-work.qtd-total + (mov-est-acbm.qtd-tot-perf +
                                                   mov-est-acbm.qtd-tot-def +
                                                   mov-est-acbm.qtd-tot-sob).
END.

for each mov-est-acbm
    WHERE mov-est-acbm.cod-estabel >= tt-param.fi-ini-cod-estabel  
      AND mov-est-acbm.cod-estabel <= tt-param.fi-fin-cod-estabel
      AND mov-est-acbm.data-mov    >= tt-param.fi-ini-data-mov  
      AND mov-est-acbm.data-mov    <= tt-param.fi-fin-data-mov  
      AND mov-est-acbm.it-codigo   >= tt-param.fi-ini-it-codigo 
      AND mov-est-acbm.it-codigo   <= tt-param.fi-fin-it-codigo 
      AND mov-est-acbm.cod-refer   >= tt-param.fi-ini-cod-refer
      AND mov-est-acbm.cod-refer   <= tt-param.fi-fin-cod-refer
      AND mov-est-acbm.num-lote    >= tt-param.fi-ini-num-lote
      AND mov-est-acbm.num-lote    <= tt-param.fi-fin-num-lote
      AND INDEX(tt-param.tp-tecelagem,mov-est-acbm.tipo-tear) <> 0 
      AND ((substr(mov-est-acbm.cod-refer,3,4) =  tt-param.fi-desenho AND tt-param.l-inc-exc = yes) or 
           (substr(mov-est-acbm.cod-refer,3,4) <> tt-param.fi-desenho and tt-param.l-inc-exc = no) or 
           (tt-param.fi-desenho = ""))
      AND NOT CAN-FIND(tt-excluir-ob WHERE tt-excluir-ob.num-lote = mov-est-acbm.num-lote)
    NO-LOCK,
    EACH item-ext WHERE item-ext.it-codigo = mov-est-acbm.it-codigo
                    AND (item-ext.indigo = YES AND tt-param.opc-artigo = "I" OR
                         item-ext.indigo = NO  AND tt-param.opc-artigo = "O" OR
                                                   tt-param.opc-artigo = "A")
                  NO-LOCK,
    EACH tt-work WHERE tt-work.it-codigo = mov-est-acbm.it-codigo NO-LOCK

    BREAK BY tt-work.qtd-total DESCEND
          BY mov-est-acbm.it-codigo:
    
    run pi-acompanhar in h-acomp (input "Final - Item: " + mov-est-acbm.it-codigo + " Data: " +
                                        STRING(mov-est-acbm.data-mov)).
    
    assign de-tot-prd-it = de-tot-prd-it + 
                            mov-est-acbm.qtd-tot-perf +
                            mov-est-acbm.qtd-tot-def +
                            mov-est-acbm.qtd-tot-sob 
           de-tot-prd-ger = de-tot-prd-ger +
                            mov-est-acbm.qtd-tot-perf +
                            mov-est-acbm.qtd-tot-def +
                            mov-est-acbm.qtd-tot-sob 
           de-tot-prf-it  = de-tot-prf-it  + mov-est-acbm.qtd-tot-perf
           de-tot-prf-ger = de-tot-prf-ger + mov-est-acbm.qtd-tot-perf
           de-tot-sob-it  = de-tot-sob-it  + mov-est-acbm.qtd-tot-sob
           de-tot-sob-ger = de-tot-sob-ger + mov-est-acbm.qtd-tot-sob
           de-tot-def-it  = de-tot-def-it  + mov-est-acbm.qtd-tot-def +
                                             mov-est-acbm.qtd-tot-sob
           de-tot-def-ger = de-tot-def-ger + mov-est-acbm.qtd-tot-def +
                                             mov-est-acbm.qtd-tot-sob.
   
    for each mov-est-acbd WHERE mov-est-acbm.cod-estabel = mov-est-acbd.cod-estabel
                            AND mov-est-acbm.data-mov    = mov-est-acbd.data-mov
                            AND mov-est-acbm.num-lote    = mov-est-acbd.num-lote
                            AND mov-est-acbm.it-codigo   = mov-est-acbd.it-codigo
                            AND mov-est-acbm.cod-refer   = mov-est-acbd.cod-refer
                          NO-LOCK:
        if mov-est-acbd.classific = "Rg" then do:
           if tt-param.l-som-reg-perf then
              assign de-tot-prf-it  = de-tot-prf-it  + mov-est-acbd.qtd-defeit
                     de-tot-prf-ger = de-tot-prf-ger + mov-est-acbd.qtd-defeit
                     de-tot-def-it  = de-tot-def-it  - mov-est-acbd.qtd-defeit
                     de-tot-def-ger = de-tot-def-ger - mov-est-acbd.qtd-defeit.
           else
              assign de-tot-reg-it  = de-tot-reg-it  + mov-est-acbd.qtd-defeit
                     de-tot-reg-ger = de-tot-reg-ger + mov-est-acbd.qtd-defeit.
        end.
        ELSE DO:
           if mov-est-acbd.classific = "Rt" then
              assign de-tot-ret-it  = de-tot-ret-it  + mov-est-acbd.qtd-defeit
                     de-tot-ret-ger = de-tot-ret-ger + mov-est-acbd.qtd-defeit.
           else
             assign de-tot-ld-it  = de-tot-ld-it  + mov-est-acbd.qtd-defeit
                    de-tot-ld-ger = de-tot-ld-ger + mov-est-acbd.qtd-defeit.
        END.
    end.
    
    IF tt-param.l-resumo-qualid THEN DO:
       FOR EACH ob-etiqueta WHERE ob-etiqueta.nr-ob       = mov-est-acbm.num-lote
                              AND ob-etiqueta.tipo-ordem  = 1
                              AND ob-etiqueta.cod-estabel = mov-est-acbm.cod-estabel
                              AND ob-etiqueta.dt-emissao  = mov-est-acbm.data-mov
                              AND ob-etiqueta.it-codigo   = mov-est-acbm.it-codigo
                              AND ob-etiqueta.cod-refer   = mov-est-acbm.cod-refer  
                            NO-LOCK.
           FIND tt-resumo-qualid 
                WHERE tt-resumo-qualid.cod-qualid = ob-etiqueta.cod-qualid 
                NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-resumo-qualid THEN DO:
              CREATE tt-resumo-qualid.
              ASSIGN tt-resumo-qualid.cod-qualid = ob-etiqueta.cod-qualid.
           END.
           ASSIGN tt-resumo-qualid.quantidade = tt-resumo-qualid.quantidade + ob-etiqueta.quantidade.
       END.
    END.

    if first-of(mov-est-acbm.it-codigo) then do:
       find item where
            item.it-codigo = mov-est-acbm.it-codigo no-lock no-error.
       DISPLAY ITEM.it-codigo
               item.desc-item    
               item.peso-bruto
               with frame f-detalhe.
    end.
                                           
    if last-of(mov-est-acbm.it-codigo) then do:

       if de-tot-prd-it <> 0 then
          assign de-aux-perc1 = de-tot-prf-it / de-tot-prd-it * 100.
       else
           assign de-aux-perc1 = 0.
       if de-tot-prd-it <> 0 then
          assign de-aux-perc2 = de-tot-reg-it / de-tot-prd-it * 100.
       else
          assign de-aux-perc2 = 0.
       if de-tot-prd-it <> 0 then
          assign de-aux-perc3 = de-tot-ld-it / de-tot-prd-it * 100.
       else
          assign de-aux-perc3 = 0.
       if de-tot-prd-it <> 0 then
          assign de-aux-perc4 = de-tot-ret-it / de-tot-prd-it * 100.
       else
          assign de-aux-perc4 = 0.
       if de-tot-prd-it <> 0 then
          assign de-aux-perc5 = de-tot-sob-it / de-tot-prd-it * 100.
       else
          assign de-aux-perc5 = 0.
       if de-tot-prd-it <> 0 then
          assign de-aux-perc6 = de-tot-def-it / de-tot-prd-it * 100.
       else
          assign de-aux-perc6 = 0.
              
       display de-tot-prd-it
               de-tot-prf-it
               de-aux-perc1
               de-tot-reg-it when tt-param.l-som-reg-perf = no
               de-aux-perc2  when tt-param.l-som-reg-perf = no
               de-tot-ld-it
               de-aux-perc3
               de-tot-ret-it
               de-aux-perc4
/*               de-tot-sob-it
               de-aux-perc5   */
               de-tot-def-it
               de-aux-perc6
               with frame f-detalhe.
       down with frame f-detalhe.
       
       assign de-tot-prd-it = 0   
              de-tot-prf-it = 0   
              de-tot-reg-it = 0   
              de-tot-ld-it  = 0   
              de-tot-ret-it = 0   
              de-tot-sob-it = 0   
              de-tot-def-it = 0.  
    end.       
end.
if de-tot-prd-ger <> 0 then
   assign de-aux-perc1 = de-tot-prf-ger / de-tot-prd-ger * 100.
else
   assign de-aux-perc1 = 0.
if de-tot-prd-ger <> 0 then
   assign de-aux-perc2 = de-tot-reg-ger / de-tot-prd-ger * 100.
else
   assign de-aux-perc2 = 0.
if de-tot-prd-ger <> 0 then
   assign de-aux-perc3 = de-tot-ld-ger / de-tot-prd-ger * 100.
else
   assign de-aux-perc3 = 0.
if de-tot-prd-ger <> 0 then
   assign de-aux-perc4 = de-tot-ret-ger / de-tot-prd-ger * 100.
else
   assign de-aux-perc4 = 0.
if de-tot-prd-ger <> 0 then
   assign de-aux-perc5 = de-tot-sob-ger / de-tot-prd-ger * 100.
else
   assign de-aux-perc5 = 0.
if de-tot-prd-ger <> 0 then
   assign de-aux-perc6 = de-tot-def-ger / de-tot-prd-ger * 100.
else
   assign de-aux-perc6 = 0.
       
down 1 with frame f-detalhe.

display "Total Geral"  @ item.desc-item
        de-tot-prd-ger @ de-tot-prd-it
        de-tot-prf-ger @ de-tot-prf-it
        de-aux-perc1
        de-tot-reg-ger when tt-param.l-som-reg-perf = no  @ de-tot-reg-it 
        de-aux-perc2   when tt-param.l-som-reg-perf = no
        de-tot-ld-ger  @ de-tot-ld-it
        de-aux-perc3
        de-tot-ret-ger @ de-tot-ret-it
        de-aux-perc4
     /* de-tot-sob-ger @ de-tot-sob-it
        de-aux-perc5 */
        de-tot-def-ger @ de-tot-def-it
        de-aux-perc6
        with frame f-detalhe.

assign de-tot-prd-ger = 0
       de-tot-prf-ger = 0
       de-tot-reg-ger = 0
       de-tot-ld-ger  = 0
       de-tot-ret-ger = 0
       de-tot-sob-ger = 0
       de-tot-def-ger = 0.

IF tt-param.l-resumo-qualid THEN DO:
   PAGE.
   FOR EACH tt-resumo-qualid:
       ACCUMULATE tt-resumo-qualid.quantidade(TOTAL).
   END.
   FOR EACH tt-resumo-qualid:
       FIND qualid-tecido WHERE qualid-tecido.codigo = tt-resumo-qualid.cod-qualid
                          NO-LOCK NO-ERROR.
       IF (ACCUM TOTAL tt-resumo-qualid.quantidade) <> 0 THEN
          ASSIGN de-aux-perc1 = tt-resumo-qualid.quantidade / (ACCUM TOTAL tt-resumo-qualid.quantidade) * 100.
       ELSE
          ASSIGN de-aux-perc1 = 0.
       DISPLAY tt-resumo-qualid.cod-qualid
               qualid-tecido.descricao WHEN AVAIL qualid-tecido
               de-aux-perc1
               WITH FRAME f-res-qualid.
       DOWN WITH FRAME f-res-qualid.
   END.
END.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.fi-ini-cod-estabel
           tt-param.fi-fin-cod-estabel
           tt-param.fi-ini-it-codigo
           tt-param.fi-fin-it-codigo
           tt-param.fi-ini-cod-refer
           tt-param.fi-fin-cod-refer
           tt-param.fi-desenho
           tt-param.l-inc-exc
           tt-param.fi-ini-data-mov
           tt-param.fi-fin-data-mov
           tt-param.fi-ini-num-lote 
           tt-param.fi-fin-num-lote 
           tt-param.l-excluir-ob
           tt-param.l-som-reg-perf
           tt-param.l-resumo-qualid
           tt-param.opc-artigo
           tt-param.tp-tecelagem
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

