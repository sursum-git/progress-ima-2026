/* Programa: ESFT001.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Gerar o relatorio de Mercadorias para Embarque
** Autor...: Gilvando de Souza Araujo - Novembro/1997
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL / SANTA ELISABETH
**
** Conversao para EMS 2.04:
**   Programa: ESFT001.P  =>  ESFT0012RP.P
**   Autor...: Prodb - Toninho
**   Data....: 09/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0012RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino        AS INTEGER
       FIELD arquivo        AS CHAR FORMAT "x(35)"
       FIELD usuario        AS CHAR FORMAT "x(12)"
       FIELD data-exec      AS DATE
       FIELD hora-exec      AS INTEGER
       FIELD c-cod-estabel  LIKE nota-fiscal.cod-estabel 
       FIELD dt-emis-ini    LIKE nota-fiscal.dt-emis
       FIELD dt-emis-fim    LIKE nota-fiscal.dt-emis
       FIELD dt-saida-ini   LIKE nota-fiscal.dt-saida
       FIELD dt-saida-fim   LIKE nota-fiscal.dt-saida
       FIELD c-transp-ini   like nota-fiscal.nome-transp
       FIELD c-transp-fim   like nota-fiscal.nome-transp
       FIELD i-numero-ini   like nota-fiscal.nr-nota-fis 
       FIELD i-numero-fim   like nota-fiscal.nr-nota-fis 
       FIELD impr-param     AS LOGICAL.

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

def TEMP-TABLE  w-aux
    field volume-ini like ped-item-res.volume-ini
    field volume-fim like ped-item-res.volume-fim
    field sigla-emb  like ped-item-res.sigla-emb.

def var l-passou as log.
def var i-nr-volumes as int format "999".
def var c-desc-emb as char format "x(100)".
def var i-qtd-vol as int extent 6.
DEF VAR c-nr-nota    AS CHAR FORMAT "x(13)".
DEF VAR c-nome-emit  AS CHAR FORMAT "x(35)".
def var de-tot-bruto like nota-fiscal.peso-bru-tot.
def var de-tot-liq   like de-tot-bruto.
def var de-tot-valor like de-tot-bruto.
def var c-cidade     as char format "x(21)".
def var c-uf         like emitente.estado.

form
    tt-param.c-cod-estabel  LABEL "Estabelecimento" AT 3
    tt-param.dt-emis-ini    label "Emissao de"      at 8
    "a"                                             AT 36
    tt-param.dt-emis-fim    NO-LABELS
    tt-param.dt-saida-ini   label "Sa¡da de.."      at 8 
    "a"                                             AT 36
    tt-param.dt-saida-fim   NO-LABELS                    
    tt-param.c-transp-ini   label "Transp de"       at 9
    "a"                                             AT 36    
    tt-param.c-transp-fim   NO-LABELS
    tt-param.i-numero-ini   label "Nota Fiscal"     at 7
    "a"                                             AT 36    
    tt-param.i-numero-fim   NO-LABELS
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

form
    c-nr-nota                      FORMAT "x(13)"      COLUMN-LABEL "Nota Fis" 
    c-nome-emit                    FORMAT "x(35)"      COLUMN-LABEL "Emitente"  
    c-cidade                                           column-label "Cidade"
    c-uf
    nota-fiscal.peso-liq-tot       format ">>>,>>9.99" column-label "Peso Liq."
    nota-fiscal.peso-bru-tot       format ">>>,>>9.99" column-label "Peso Bruto"
    nota-fiscal.vl-tot-nota        format ">>>,>>9.99"
                                   column-label "Vl.Total Nota"
    w-aux.volume-ini               column-label "Vol.Ini"
    w-aux.volume-fim               column-label "Vol.Fim"
    w-aux.sigla-emb                column-label "Emb"
    with NO-BOX 55 down width 132 STREAM-IO frame f-detalhe.

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
{utp/ut-liter.i Mercadorias_para_Embarque * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each nota-fiscal where 
         nota-fiscal.cod-estabel  =  tt-param.c-cod-estabel AND
         nota-fiscal.dt-emis-nota >= tt-param.dt-emis-ini   AND
         nota-fiscal.dt-emis-nota <= tt-param.dt-emis-fim   AND
         nota-fiscal.dt-saida     >= tt-param.dt-saida-ini  AND
         nota-fiscal.dt-saida     <= tt-param.dt-saida-fim  AND
         nota-fiscal.dt-cancela    = ?                      AND
         nota-fiscal.nr-nota-fis  >= tt-param.i-numero-ini  AND
         nota-fiscal.nr-nota-fis  <= tt-param.i-numero-fim  AND 
         nota-fiscal.nome-trans   >= tt-param.c-transp-ini  AND
         nota-fiscal.nome-trans   <= tt-param.c-transp-fim no-lock
         break BY nota-fiscal.nome-trans
               BY nota-fiscal.nr-nota-fis:

    run pi-acompanhar in h-acomp (INPUT nota-fiscal.nr-nota-fis).

    find first emitente where
               emitente.cod-emit = nota-fiscal.cod-emit  AND
               emitente.identific <> 2 no-lock.

    find transporte where 
         transporte.nome-abrev = nota-fiscal.nome-trans no-lock.

    for each w-aux.
        delete w-aux.
    end.


    FOR each it-nota-fisc of nota-fiscal NO-LOCK,
        each item where item.it-codigo = it-nota-fisc.it-codigo NO-LOCK.

        /* PEDIDO DE VENDA */
        FIND ped-venda where
             ped-venda.nome-abrev = nota-fiscal.nome-ab-cli AND
             ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli NO-LOCK NO-ERROR.
    
        IF NOT AVAIL ped-venda AND
           nota-fiscal.nome-abrev-tri <> "" AND
           nota-fiscal.nome-abrev-tri = nota-fiscal.nome-ab-cli THEN DO.
           FIND FIRST ped-venda WHERE
                      ped-venda.nr-pedcli = nota-fiscal.nr-pedcli NO-LOCK NO-ERROR.
        END.
    
        /* -----  PESQUISA PARA PREENCHER OS CAMPOS DE VOLUME ----- */
        find last ped-item-res
             where ped-item-res.nome-abrev   = ped-venda.nome-abrev
               and ped-item-res.nr-pedcli    = ped-venda.nr-pedcli
               and ped-item-res.it-codigo    = it-nota-fisc.it-codigo
               AND ped-item-res.cod-refer    = it-nota-fisc.cod-refer
               and ped-item-res.nr-sequencia = it-nota-fisc.nr-seq-ped
               no-lock no-error.

        IF AVAIL ped-item-res THEN DO.
           FIND FIRST w-aux WHERE
                      w-aux.volume-ini = ped-item-res.volume-ini AND
                      w-aux.volume-fim = ped-item-res.volume-fim NO-ERROR.
    
           if not avail w-aux then do.
              create w-aux.
              assign w-aux.volume-ini = ped-item-res.volume-ini
                     w-aux.volume-fim = ped-item-res.volume-fim
                     w-aux.sigla-emb  = ped-item-res.sigla-emb.
           end.
        END.
    END.

    find loc-entr where 
         loc-entr.nome-abrev  = nota-fiscal.nome-ab-cli AND
         loc-entr.cod-entrega = nota-fiscal.cod-entrega no-lock no-error.

    if avail loc-entr then
       assign c-uf = loc-entr.estado
              c-cidade = loc-entr.cidade.
    ELSE DO.
        if nota-fiscal.endereco <> "" then
           assign c-uf     = nota-fiscal.estado
                  c-cidade = nota-fiscal.cidade.
        else do:
             if  avail ped-venda then do:
                 if ped-venda.local-entreg <> "" then
                    assign c-uf     = ped-venda.estado
                           c-cidade = ped-venda.cidade.
                 else
                    assign c-uf     = emitente.estado
                           c-cidade = emitente.cidade.
             end.
             else
                assign c-uf     = emitente.estado
                       c-cidade = emitente.cidade.
        end.
    END.

    if first-of(nota-fiscal.nome-trans) then do:
       find transporte where
            transporte.nome-abrev = nota-fiscal.nome-trans no-lock.
       put "Transportador:"
           transporte.cod-transp "-"
           transporte.nome
           "Contato: "
           transporte.contato
           "Tel.: " transporte.telefone
           skip(2).
    end.

    display nota-fiscal.nr-nota-fis @ c-nr-nota
            emitente.nome-emit      @ c-nome-emit
            c-cidade
            c-uf
            nota-fiscal.peso-liq-tot
            nota-fiscal.peso-bru-tot
            nota-fiscal.vl-tot-nota
            with frame f-detalhe.

    assign l-passou = no.
    for each w-aux.
        display w-aux.volume-ini
                w-aux.volume-fim
                w-aux.sigla-emb
                with frame f-detalhe.
        down with frame f-detalhe.
        assign i-nr-volumes = i-nr-volumes +
                              ((w-aux.volume-fim -
                               w-aux.volume-ini) + 1)
               l-passou = yes.

        if w-aux.sigla-emb = "Cx" then
           assign i-qtd-vol[1] = i-qtd-vol[1] + 1.
        else
        if w-aux.sigla-emb = "Fd" then
           assign i-qtd-vol[2] = i-qtd-vol[2] + 1.
        else
        if w-aux.sigla-emb = "Rl" then
           assign i-qtd-vol[3] = i-qtd-vol[3] + 1.
        else
        if w-aux.sigla-emb = "Pt" then
           assign i-qtd-vol[4] = i-qtd-vol[4] + 1.
        else
        if w-aux.sigla-emb = "Pc" then
           assign i-qtd-vol[5] = i-qtd-vol[5] + 1.
        else
           assign i-qtd-vol[6] = i-qtd-vol[6] + 1.
    end.

    if l-passou = no then
       down with frame f-detalhe.

    assign de-tot-bruto = de-tot-bruto + nota-fiscal.peso-bru-tot
           de-tot-liq = de-tot-liq + nota-fiscal.peso-liq-tot
           de-tot-valor = de-tot-valor + nota-fiscal.vl-tot-nota.

    if last-of(nota-fiscal.nome-trans) then do.
       display "Totais"      @ c-cidade
                de-tot-liq   @ nota-fiscal.peso-liq-tot
                de-tot-bruto @ nota-fiscal.peso-bru-tot
                de-tot-valor @ nota-fiscal.vl-tot-nota
                i-nr-volumes @ w-aux.volume-ini
                with frame f-detalhe.

         assign de-tot-liq   = 0
                de-tot-bruto = 0
                de-tot-valor = 0
                i-nr-volumes = 0.
          
         put skip(2).

         c-desc-emb = "Totais dos Volumes: ".
         if i-qtd-vol[1] > 0 then
            assign c-desc-emb = c-desc-emb + "Cx: " + 
                                 string(i-qtd-vol[1],">>9").
         if i-qtd-vol[2] > 0 then
            assign c-desc-emb = c-desc-emb + "  Fd: " + 
                                 string(i-qtd-vol[2],">>9").
         if i-qtd-vol[3] > 0 then
            assign c-desc-emb = c-desc-emb + "  Rl: " + 
                                 string(i-qtd-vol[3],">>9").
         if i-qtd-vol[4] > 0 then
            assign c-desc-emb = c-desc-emb + "  Pt: " + 
                                 string(i-qtd-vol[4],">>9").
         if i-qtd-vol[5] > 0 then
            assign c-desc-emb = c-desc-emb + "  Pc: " + 
                                 string(i-qtd-vol[5],">>9").
         if i-qtd-vol[6] > 0 then
            assign c-desc-emb = c-desc-emb + "  Out: " + 
                                 string(i-qtd-vol[6],">>9").
         put c-desc-emb
             skip(2).

         PUT "Embarque: ______/______/______ _______:_______  "
             "Embarcador Responsavel:_______________________________ "
             "Assin.:_______________________" skip(2)
             "          Placa:______________ Motorista:____________________________ "
             "DI/CPF: _________________ Assinatura:__________________________" skip(2)
             "Chamadas"
             "    Data         Hora   Atendente          "
             "    Data         Hora   Atendente          "
             "    Data         Hora   Atendente          " skip(2)
             "     "
             "1 ____/____/____  ___:___  _______________ "
             "2 ____/____/____  ___:___  _______________ "
             "3 ____/____/____  ___:___  _______________ " skip(2)
             "     "
             "4 ____/____/____  ___:___  _______________ "
             "5 ____/____/____  ___:___  _______________ "
             "6 ____/____/____  ___:___  _______________ ".
         page.

         assign i-qtd-vol[1] = 0
                i-qtd-vol[2] = 0
                i-qtd-vol[3] = 0
                i-qtd-vol[4] = 0
                i-qtd-vol[5] = 0
                i-qtd-vol[6] = 0.
    END.
END. /* Nota Fiscal */

IF tt-param.impr-param THEN DO.
   PAGE.
   PUT "*****----- PAR¶METROS ------*****"
       SKIP(1).

   display tt-param.c-cod-estabel
           tt-param.dt-emis-ini       
           tt-param.dt-emis-fim       
           tt-param.dt-saida-ini
           tt-param.dt-saida-fim
           tt-param.c-transp-ini 
           tt-param.c-transp-fim 
           tt-param.i-numero-ini 
           tt-param.i-numero-fim 
           with frame f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.


