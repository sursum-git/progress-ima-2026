/* Programa: ESSP0109.W
** Modulo..: Controle de Estoque / Controle Acabado
** Objetivo: Listar Estatistica de (Cortes e Qualidade) por Rolos
** Autor...: Fábio Coelho Lanza - Mar‡o/2006
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/
/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0109RP 2.04.00.000}

DEFINE temp-table tt-param  no-undo
       field destino             as integer
       field arquivo             as char format "x(35)"
       field usuario             as char format "x(12)"
       field data-exec           as date
       field hora-exec           as integer
       field classifica          as integer
       field desc-classifica     as char format "x(40)"
       field fi-ini-it-codigo    like ob-etiqueta.it-codigo
       field fi-fin-it-codigo    like ob-etiqueta.it-codigo
       field fi-ini-cod-refer    like ob-etiqueta.cod-refer
       field fi-fin-cod-refer    like ob-etiqueta.cod-refer
       field fi-ini-dt-emissao   like ob-etiqueta.dt-emissao
       field fi-fin-dt-emissao   like ob-etiqueta.dt-emissao
       field tipo-relatorio      AS INTEGER
       field desc-tipo-relat     as char format "x(20)"
       field imp-param           as log.

define temp-table tt-raw-digita
       field raw-digita as raw.

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

def var de-cor-00    as INT FORMAT  ">>>9".
def var de-cor-01    as INT FORMAT  ">>>9".
def var de-cor-02    as INT FORMAT  ">>>9".
def var de-qua-00    as INT FORMAT   ">>>9".
def var de-qua-01    as INT FORMAT  ">>>9".
def var de-itcor-00  as INT FORMAT  ">>>9".
def var de-itcor-01  as INT FORMAT  ">>>9".
def var de-itcor-02  as INT FORMAT  ">>>9".
def var de-itqua-00  as INT FORMAT  ">>>9".
def var de-itqua-01  as INT FORMAT  ">>>9".
def var de-tot-cor   as INT FORMAT  ">>>9".
def var de-tot-qua   as INT FORMAT  ">>>9".
def var de-cor-perc0 as dec FORMAT  ">>9.99%".
def var de-cor-perc1 as dec FORMAT  ">>9.99%".
def var de-cor-perc2 as dec FORMAT  ">>9.99%".
def var de-qua-perc0 as dec FORMAT  ">>9.99%".
def var de-qua-perc1 as dec FORMAT  ">>9.99%".
def var c-descricao  AS CHAR FORMAT "x(36)".

form 
    "*--------------- Parƒmetros/Sele‡Æo ----------------*" SKIP
    tt-param.fi-ini-it-codigo     label "Item............."
    "A"  AT 36                    
    tt-param.fi-fin-it-codigo     NO-LABELS SKIP
    tt-param.fi-ini-cod-refer     label "Referˆncia......."
    "A"  AT 36                    
    tt-param.fi-fin-cod-refer     NO-LABELS SKIP
    tt-param.fi-ini-dt-emissao    label "Data do Movimento"
    "A"  AT 36                    
    tt-param.fi-fin-dt-emissao    NO-LABELS SKIP
    tt-param.desc-tipo-relat      LABEL "Tipo Relat¢rio..."
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    ob-etiqueta.it-codigo FORMAT "x(6)"     
    ob-etiqueta.cod-refer FORMAT "xx.xxxx.x"
    c-descricao
    de-cor-00
    de-cor-perc0
    de-cor-01
    de-cor-perc1
    de-cor-02
    de-cor-perc2
    de-tot-cor
    de-qua-00      at 103
    de-qua-perc0   at 108
    de-qua-01      at 116
    de-qua-perc1   at 121
    de-tot-qua     at 129
    with no-box NO-LABEL 55 down WIDTH 132 STREAM-IO frame f-detalhe.

form header
    "***********  C  O  R  T  E  S  ************"           at  55
    "********* QUALIDADE **********"                        at 103
    "ITEM   REFERENC. DESCRICAO"                            at   1
    "QTDE S/CORTE QTDE 1 CORTE QTDE 2 CORTE TOT."           at  55
    "QTDE  NORMAL QTDE EXPORT. TOT."                        at 103 
    "------ --------- ------------------------------------" at   1
    "------------ ------------ ------------ ----"           at  55
    "------------ ------------ ----"                        at 103
    with width 132 no-labels no-box page-top STREAM-IO frame f-form-cab.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESPECÖFICOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Estatistica_do_Rolo_Por_Corte_e_Qualidade * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-form-cab.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).
 
for each ob-etiqueta
   where ob-etiqueta.dt-emissao >= tt-param.fi-ini-dt-emissao
     and ob-etiqueta.dt-emissao <= tt-param.fi-fin-dt-emissao
     and ob-etiqueta.it-codigo  >= tt-param.fi-ini-it-codigo
     and ob-etiqueta.it-codigo  <= tt-param.fi-fin-it-codigo
     and ob-etiqueta.cod-refer  >= tt-param.fi-ini-cod-refer
     and ob-etiqueta.cod-refer  <= tt-param.fi-fin-cod-refer
     NO-LOCK
     break by ob-etiqueta.it-codigo
           by ob-etiqueta.cod-refer:
    
   run pi-acompanhar in h-acomp (input "Item: "  + ob-etiqueta.it-codigo +
                                       " Data: " + STRING (ob-etiqueta.dt-emissao)).

   CASE ob-etiqueta.nr-cortes.
        WHEN 0  then 
             assign de-cor-00 = de-cor-00 + 1.
        WHEN 1  then
             assign de-cor-01 = de-cor-01 + 1.
        WHEN 2  then
             assign de-cor-02 = de-cor-02 + 1.
   END CASE.

   if ob-etiqueta.qualidade = 0 then 
      assign de-qua-00 = de-qua-00 + 1.
   else 
      assign de-qua-01 = de-qua-01 + 1.
   
   if tt-param.tipo-relatorio = 2 then do: /* ANALITICO */
      if last-of(ob-etiqueta.cod-refer) then do:
         assign de-tot-cor   = de-cor-00 + de-cor-01 + de-cor-02
                de-tot-qua   = de-qua-00 + de-qua-01
                de-cor-perc0 = de-cor-00 / de-tot-cor * 100
                de-cor-perc1 = de-cor-01 / de-tot-cor * 100
                de-cor-perc2 = de-cor-02 / de-tot-cor * 100
                de-qua-perc0 = de-qua-00 / de-tot-qua * 100
                de-qua-perc1 = de-qua-01 / de-tot-qua * 100.

         find item where item.it-codigo = ob-etiqueta.it-codigo no-lock no-error.
         assign c-descricao = ITEM.descricao-1 + ITEM.descricao-2.

         display ob-etiqueta.it-codigo
                 ob-etiqueta.cod-refer
                 c-descricao
                 de-cor-00
                 de-cor-perc0 
                 de-cor-01
                 de-cor-perc1
                 de-cor-02
                 de-cor-perc2
                 de-tot-cor
                 de-qua-00 
                 de-qua-perc0
                 de-qua-01
                 de-qua-perc1
                 de-tot-qua
                 with frame f-detalhe.
         down with frame f-detalhe.
       
         assign de-itcor-00 = de-itcor-00 + de-cor-00
                de-itcor-01 = de-itcor-01 + de-cor-01
                de-itcor-02 = de-itcor-02 + de-cor-02
                de-itqua-00 = de-itqua-00 + de-qua-00
                de-itqua-01 = de-itqua-01 + de-qua-01
                de-cor-00   = 0
                de-cor-01   = 0
                de-cor-02   = 0
                de-qua-00   = 0
                de-qua-01   = 0.

         if last-of(ob-etiqueta.it-codigo) then do:
            assign de-tot-cor   = de-itcor-00 + de-itcor-01 + de-itcor-02
                   de-tot-qua   = de-itqua-00 + de-itqua-01
                   de-cor-perc0 = de-itcor-00 / de-tot-cor * 100
                   de-cor-perc1 = de-itcor-01 / de-tot-cor * 100
                   de-cor-perc2 = de-itcor-02 / de-tot-cor * 100
                   de-qua-perc0 = de-itqua-00 / de-tot-qua * 100
                   de-qua-perc1 = de-itqua-01 / de-tot-qua * 100.

            display "TOTAL DO ITEM ..." @ c-descricao
                    de-itcor-00         @ de-cor-00
                    de-cor-perc0 
                    de-itcor-01         @ de-cor-01
                    de-cor-perc1
                    de-itcor-02         @ de-cor-02
                    de-cor-perc2
                    de-tot-cor
                    de-itqua-00         @ de-qua-00
                    de-qua-perc0
                    de-itqua-01         @ de-qua-01
                    de-qua-perc1
                    de-tot-qua
                    with frame f-detalhe.
            down 2 with frame f-detalhe.

            assign de-itcor-00 = 0
                   de-itcor-01 = 0
                   de-itcor-02 = 0
                   de-itqua-00 = 0
                   de-itqua-01 = 0.
         end.
      end.
   end.
   else do: /* SINTETICO */
      if last-of(ob-etiqueta.it-codigo) then do:
         assign de-tot-cor   = de-cor-00 + de-cor-01 + de-cor-02
                de-tot-qua   = de-qua-00 + de-qua-01
                de-cor-perc0 = de-cor-00 / de-tot-cor * 100
                de-cor-perc1 = de-cor-01 / de-tot-cor * 100
                de-cor-perc2 = de-cor-02 / de-tot-cor * 100
                de-qua-perc0 = de-qua-00 / de-tot-qua * 100
                de-qua-perc1 = de-qua-01 / de-tot-qua * 100.

         find item where item.it-codigo = ob-etiqueta.it-codigo no-lock no-error.
         assign c-descricao = ITEM.descricao-1 + ITEM.descricao-2.

         display ob-etiqueta.it-codigo
                 c-descricao
                 de-cor-00
                 de-cor-perc0 
                 de-cor-01
                 de-cor-perc1
                 de-cor-02
                 de-cor-perc2
                 de-tot-cor
                 de-qua-00
                 de-qua-perc0
                 de-qua-01
                 de-qua-perc1
                 de-tot-qua
                 with frame f-detalhe.
         down with frame f-detalhe.

         assign de-cor-00 = 0
                de-cor-01 = 0
                de-cor-02 = 0
                de-qua-00 = 0
                de-qua-01 = 0.
      end.
   end.
end.

if tt-param.imp-param then do:
   PAGE.
   display tt-param.fi-ini-it-codigo
           tt-param.fi-fin-it-codigo
           tt-param.fi-ini-cod-refer
           tt-param.fi-fin-cod-refer
           tt-param.fi-ini-dt-emissao
           tt-param.fi-fin-dt-emissao
           tt-param.desc-tipo-relat
           with frame f-param.
end.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

