/* Programa: ESBENM11A.P
** Sistema.: Controle de Servicos de Atendimento ao Consumidor - SAC
** Setor...: Benefiamento
** Objetivo: Imprimir os Controles de SAC
** Autor...: Gilvando Souza Araujo - Marco/2000
**
** Conversao para EMS 2.04:
**   Programa: ESBENM11A.P  =>  ESSP0096RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 16/04/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0096RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino            as integer
       field arquivo            as char format "x(35)"
       field usuario            as char format "x(12)"
       field data-exec          as date
       field hora-exec          as integer
       field classifica         as integer
       field desc-classifica    as char format "x(40)"
       FIELD fm-codigo-ini      LIKE contr-sac.fm-codigo
       FIELD fm-codigo-fin      LIKE contr-sac.fm-codigo
       FIELD it-codigo-ini      LIKE contr-sac.it-codigo
       FIELD it-codigo-fin      LIKE contr-sac.it-codigo
       FIELD des-cor-ini        LIKE contr-sac.des-cor
       FIELD des-cor-fin        LIKE contr-sac.des-cor
       FIELD cod-ocorr-ini      LIKE contr-sac.cod-ocorr
       FIELD cod-ocorr-fin      LIKE contr-sac.cod-ocorr
       FIELD cod-tipo-def-ini   LIKE contr-sac.cod-tipo-def
       FIELD cod-tipo-def-fin   LIKE contr-sac.cod-tipo-def
       FIELD cod-defeito-ini    LIKE contr-sac.cod-defeito
       FIELD cod-defeito-fin    LIKE contr-sac.cod-defeito
       FIELD cod-emitente-ini   LIKE contr-sac.cod-emitente
       FIELD cod-emitente-fin   LIKE contr-sac.cod-emitente
       FIELD data-reg-ini       LIKE contr-sac.data-reg
       FIELD data-reg-fin       LIKE contr-sac.data-reg
       FIELD imp-param          AS LOG.

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

def var i-cont-reg  as int format ">>>>9".
def var i-tot-fat   like contr-sac.qtd-fat.
def var i-tot-def   like contr-sac.qtd-def.
def var c-aux-def   as char format "x(4)".
def var c-nome-def  as char format "x(28)".
def var c-lis-ocor  as char extent 12.
def var de-perc-def as dec format ">>9.9".

form 
    "*--------------- Parƒmetros/Sele‡Æo ----------------*" SKIP
    tt-param.fm-codigo-ini        label "Fam¡lia.........."
    "A"  AT 36                    
    tt-param.fm-codigo-fin        NO-LABELS SKIP
    tt-param.it-codigo-ini        label "Item............."
    "A"  AT 36                    
    tt-param.it-codigo-fin        NO-LABELS SKIP
    tt-param.des-cor-ini          label "Desenho/Cor......"
    "A"  AT 36                    
    tt-param.des-cor-fin          NO-LABELS SKIP
    tt-param.cod-ocorr-ini        label "Ocorrˆncia......."
    "A"  AT 36                    
    tt-param.cod-ocorr-fin        NO-LABELS SKIP
    tt-param.cod-tipo-def-ini     label "Tipo do Defeito.."
    "A"  AT 36
    tt-param.cod-tipo-def-fin     NO-LABELS SKIP
    tt-param.cod-defeito-ini      label "Defeito.........."
    "A"  AT 36
    tt-param.cod-defeito-fin      NO-LABELS SKIP
    tt-param.cod-emitente-ini     label "Cliente.........."
    "A"  AT 36
    tt-param.cod-emitente-fin     NO-LABELS SKIP
    tt-param.data-reg-ini         label "Data do Registro."
    "A"  AT 36                    
    tt-param.data-reg-fin         NO-LABELS SKIP
    tt-param.desc-classifica      LABEL "Classifica‡Æo...."
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    contr-sac.fm-codigo  label "Familia"
    familia.descricao    label "Descricao"
    contr-sac.des-cor    label "DesCor"
    emitente.nome-abrev  label "Cliente"
    contr-sac.cod-ocorr  label "Oc"
    c-aux-def            label "Def."
    c-nome-def           label "Descricao/Ocorrencia"
    contr-sac.qtd-fat    label "Qtd.Fatur."
    contr-sac.qtd-def    label "Qtd.Defeit"
    de-perc-def          label "%Def"
    with no-box 55 down width 132 STREAM-IO frame f-detalhe no-label.

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
{utp/ut-liter.i Controles_de_SAC * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

assign c-lis-ocor[1]  = "01-Qualidade do produto"
       c-lis-ocor[2]  = "02-Falta de Artigos"
       c-lis-ocor[3]  = "03-Excesso de Artigos"
       c-lis-ocor[4]  = "04-Troca artigo,cor,variante"
       c-lis-ocor[5]  = "05-Troca Rolo x Peca"
       c-lis-ocor[6]  = "06-Atraso entrega-Tear"
       c-lis-ocor[7]  = "07-Atraso entrega-Transp"
       c-lis-ocor[8]  = "08-Faturamento antecipado"
       c-lis-ocor[9]  = "09-Metragem em desacordo"
       c-lis-ocor[10] = "10-Preench.incorreto pedido"
       c-lis-ocor[11] = "11-Implantacao incorreta"
       c-lis-ocor[12] = "12-Outros".

for each contr-sac where contr-sac.fm-codigo    >= tt-param.fm-codigo-ini
                     and contr-sac.fm-codigo    <= tt-param.fm-codigo-fin
                     and contr-sac.des-cor      >= tt-param.des-cor-ini
                     and contr-sac.des-cor      <= tt-param.des-cor-fin
                     and contr-sac.data-reg     >= tt-param.data-reg-ini
                     and contr-sac.data-reg     <= tt-param.data-reg-fin
                     and contr-sac.cod-emitente >= tt-param.cod-emitente-ini
                     and contr-sac.cod-emitente <= tt-param.cod-emitente-fin
                     and contr-sac.cod-tipo-def >= tt-param.cod-tipo-def-ini
                     and contr-sac.cod-tipo-def <= tt-param.cod-tipo-def-fin
                     AND contr-sac.cod-defeito  >= tt-param.cod-defeito-ini
                     AND contr-sac.cod-defeito  <= tt-param.cod-defeito-fin
                  no-lock
                  by if tt-param.classifica = 1 then contr-sac.fm-codigo
                                                else string(contr-sac.cod-emitente):

    run pi-acompanhar in h-acomp (input "Fam¡lia: " + contr-sac.fm-codigo + 
                                        " Item: " + contr-sac.it-codigo + 
                                        " Data: " + STRING(contr-sac.data-reg)).

    find familia where familia.fm-codigo = contr-sac.fm-codigo
                 no-lock no-error.
    find emitente where emitente.cod-emitente = contr-sac.cod-emitente
                  no-lock no-error.
    find repres where repres.cod-rep = contr-sac.cod-rep
                no-lock no-error.
    if contr-sac.cod-ocorr = "01" then do:
       find tipo-def where tipo-def.cod-tipo-def = contr-sac.cod-tipo-def
                     no-lock no-error.
       find defeito where defeito.cod-tipo-def = contr-sac.cod-tipo-def
                      and defeito.cod-defeito  = contr-sac.cod-defeito
                    no-lock no-error.
       assign c-aux-def  = contr-sac.cod-tipo-def + "." +
                           contr-sac.cod-defeito
              c-nome-def = defeito.descricao.
    end.
    else
       assign c-nome-def = substr(c-lis-ocor[int(contr-sac.cod-ocorr)],4,24).
    assign de-perc-def = contr-sac.qtd-def / contr-sac.qtd-fat * 100.

    display contr-sac.fm-codigo 
            familia.descricao 
            contr-sac.des-cor
            emitente.nome-abrev
            contr-sac.cod-ocorr
            c-aux-def
            c-nome-def
            contr-sac.qtd-fat
            contr-sac.qtd-def
            de-perc-def
            with frame f-detalhe.
    down with frame f-detalhe.
    /*
    if contr-sac.observ[1] <> "" then
       put contr-sac.observ[1] skip.
    if contr-sac.observ[2] <> "" then
       put contr-sac.observ[2] skip.
    if contr-sac.observ[3] <> "" then
       put contr-sac.observ[3].
    down 1 with frame f-detalhe. 
    */
    assign i-cont-reg = i-cont-reg + 1
           i-tot-fat  = i-tot-fat + contr-sac.qtd-fat
           i-tot-def  = i-tot-def + contr-sac.qtd-def.
end.

if i-tot-fat <> 0 then
   assign de-perc-def = i-tot-def / i-tot-fat * 100.
else
   assign de-perc-def = 0.

display "Totais:"  @ contr-sac.fm-codigo
        i-cont-reg @ c-nome-def
        i-tot-fat  @ contr-sac.qtd-fat
        i-tot-def  @ contr-sac.qtd-def
        de-perc-def
        with frame f-detalhe.
assign i-cont-reg = 0
       i-tot-fat  = 0
       i-tot-def  = 0.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.fm-codigo-ini
           tt-param.fm-codigo-fin
           tt-param.it-codigo-ini
           tt-param.it-codigo-fin
           tt-param.des-cor-ini
           tt-param.des-cor-fin
           tt-param.cod-ocorr-ini
           tt-param.cod-ocorr-fin
           tt-param.cod-tipo-def-ini
           tt-param.cod-tipo-def-fin
           tt-param.cod-defeito-ini
           tt-param.cod-defeito-fin
           tt-param.cod-emitente-ini
           tt-param.cod-emitente-fin
           tt-param.data-reg-ini
           tt-param.data-reg-fin
           tt-param.desc-classifica     
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

