/* Programa: ESFT0038RP.P
** Sistema.: EMS da Datasul
** M¢dulo..: Faturamento
** Objetivo: EmissÆo do relat¢rio de NF por Transportadora, para controle
**           de Substitui‡Æo Tribut ria sobre Fretes.
** Autor...: Gilvando Souza Araujo
** Data....: 24/02/2006
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0038RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param  NO-UNDO
       FIELD destino           AS INTEGER
       FIELD arquivo           AS CHAR FORMAT "x(35)"
       FIELD usuario           AS CHAR FORMAT "x(12)"
       FIELD data-exec         AS DATE
       FIELD hora-exec         AS INTEGER
       FIELD classifica        AS INTEGER
       FIELD desc-classifica   AS CHAR FORMAT "x(40)"
       FIELD cod-estabel       like nota-fiscal.cod-estabel 
       FIELD ini-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD fin-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD ini-dt-embarque   LIKE nota-fiscal.dt-embarque 
       FIELD fin-dt-embarque   LIKE nota-fiscal.dt-embarque 
       FIELD ini-dt-saida      LIKE nota-fiscal.dt-saida
       FIELD fin-dt-saida      LIKE nota-fiscal.dt-saida
       FIELD ini-transp        LIKE nota-fiscal.nome-transp
       FIELD fin-transp        LIKE nota-fiscal.nome-transp
       FIELD ini-transp-red    LIKE nota-fiscal.nome-tr-red
       FIELD fin-transp-red    LIKE nota-fiscal.nome-tr-red
       FIELD tipo-frete        AS INTEGER
       FIELD desc-tipo-frete   AS CHAR FORMAT "x(31)"
       FIELD imp-sedex         AS LOG FORMAT "Sim/NÆo"
       FIELD imp-param         AS LOG.

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
def var h-acomp as handle NO-UNDO.

DEF VAR c-tp-frete     AS CHAR.
DEF VAR i-tot-vol-nf   AS INT.
DEF VAR i-tot-vol-tra  AS INT.
DEF VAR i-tot-vol-ger  AS INT.
DEF VAR de-tot-qtd-not AS DEC FORMAT ">>>,>>9.99".
DEF VAR de-tot-qtd-tra AS DEC FORMAT ">>>,>>9.99".
DEF VAR de-tot-qtd-ger AS DEC FORMAT ">>>,>>9.99".
DEF VAR de-tot-vlr-tra AS DEC FORMAT ">>>,>>9.99".
DEF VAR de-tot-vlr-ger AS DEC FORMAT ">>>,>>9.99".
DEF VAR de-tot-pes-tra AS DEC FORMAT ">>>,>>9.99".
DEF VAR de-tot-pes-ger AS DEC FORMAT ">>>,>>9.99".

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelec....."  SKIP
    tt-param.ini-dt-emissao   label "Data Emissao.."
    "A"  AT 33
    tt-param.fin-dt-emissao   NO-LABELS               SKIP
    tt-param.ini-dt-embarque  label "Data Embarque."
    "A"  AT 33
    tt-param.fin-dt-embarque  NO-LABELS               SKIP
    tt-param.ini-dt-saida     label "Data Sa¡da...."
    "A"  AT 33
    tt-param.fin-dt-saida     NO-LABELS               SKIP
    tt-param.ini-transp       label "Transportadora"
    "A"  AT 33
    tt-param.fin-transp       NO-LABELS               SKIP
    tt-param.ini-transp-red   label "Transp.Redesp."
    "A"  AT 33
    tt-param.fin-transp-red   NO-LABELS               SKIP
    tt-param.desc-tipo-frete  LABEL "Tipos de Frete"  SKIP
    tt-param.imp-sedex        LABEL "Imprime Sedex."  SKIP
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

FORM
    "Transportador: " 
    transporte.cod-transp " - " 
    transporte.nome-abrev " - "
    transporte.nome
    WITH NO-LABELS 2 DOWN NO-BOX NO-ATTR-SPACE WIDTH 132 STREAM-IO FRAME f-transp.

FORM
    nota-fiscal.dt-emis-nota LABEL "Dt.Emissao" FORMAT "99/99/9999" 
    nota-fiscal.nr-nota-fis  LABEL "Nr Nota"    FORMAT "x(7)" 
    nota-fiscal.nome-tr-red  LABEL "Redespacho" 
    emitente.nome-abrev      LABEL "Cliente"               
    nota-fiscal.cidade       LABEL "Cidade"             
    nota-fiscal.estado       LABEL "UF"         FORMAT "x(2)"
    nota-fiscal.vl-tot-nota  LABEL "Valor Nota" FORMAT ">>,>>>,>>9.99"
    de-tot-qtd-not           LABEL "Quantidade" FORMAT ">>>,>>9.99"
    nota-fiscal.peso-bru-tot LABEL "Peso Bruto" FORMAT ">>>,>>9.99"
    c-tp-frete               LABEL "Frt"        FORMAT "x(3)"
    i-tot-vol-nf             LABEL "Vol."       FORMAT ">>>9"
    WITH NO-BOX NO-LABEL 55 down width 136 STREAM-IO frame f-detalhe.

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
{utp/ut-liter.i Notas_Fiscais_para_Subst_Trib_S/Fretes * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each nota-fiscal WHERE nota-fiscal.cod-estabel  =  tt-param.cod-estabel
                       AND nota-fiscal.dt-emis-nota >= tt-param.ini-dt-emissao
                       AND nota-fiscal.dt-emis-nota <= tt-param.fin-dt-emissao
                       AND nota-fiscal.dt-embarque  >= tt-param.ini-dt-embarque
                       AND nota-fiscal.dt-embarque  <= tt-param.fin-dt-embarque
                       AND nota-fiscal.nome-transp  >= tt-param.ini-transp
                       AND nota-fiscal.nome-transp  <= tt-param.fin-transp
                       AND nota-fiscal.nome-tr-red  >= tt-param.ini-transp-red
                       AND nota-fiscal.nome-tr-red  <= tt-param.fin-transp-red
                       AND nota-fiscal.dt-saida     >= tt-param.ini-dt-saida
                       AND nota-fiscal.dt-saida     <= tt-param.fin-dt-saida
                       AND nota-fiscal.emite-duplic = YES
                       AND ((nota-fiscal.cidade-cif =  "" AND tt-param.tipo-frete = 2) OR
                            (nota-fiscal.cidade-cif <> "" AND tt-param.tipo-frete = 1) OR
                                                             (tt-param.tipo-frete = 3))
                       AND ((nota-fiscal.nome-transp MATCHES "sedex" AND tt-param.imp-sedex = YES) OR
                             NOT (nota-fiscal.nome-transp MATCHES "sedex"))
                       AND nota-fiscal.dt-cancela     =  ? 
                     NO-LOCK,
    EACH ped-venda WHERE ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                     AND ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
                   NO-LOCK 
    BREAK BY nota-fiscal.nome-transp
          BY nota-fiscal.nr-nota-fis:

    run pi-acompanhar in h-acomp (input string(nota-fiscal.cod-rep) + " " + nota-fiscal.nr-nota-fis).

    IF FIRST-OF(nota-fiscal.nome-transp) THEN DO:
       FIND transporte WHERE transporte.nome-abrev = nota-fiscal.nome-transp NO-LOCK NO-ERROR.
       IF AVAIL transporte THEN DO:
          DISPLAY transporte.cod-transp
                  transporte.nome-abrev
                  transporte.nome
                  WITH FRAME f-transp.
          DOWN 2 WITH FRAME f-transp.
       END.
    END.
    
    FIND emitente OF nota-fiscal NO-LOCK.
    
    ASSIGN c-tp-frete = IF nota-fiscal.cidade-cif = "" THEN "FOB"
                                                       ELSE "CIF".
    ASSIGN de-tot-qtd-not = 0.

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
        ASSIGN de-tot-qtd-not = de-tot-qtd-not + it-nota-fisc.qt-faturada[1].
        FOR EACH ped-item-rom WHERE ped-item-rom.nome-abrev   = it-nota-fisc.nome-ab-cli
                                AND ped-item-rom.nr-pedcli    = it-nota-fisc.nr-pedcli
                                AND ped-item-rom.nr-sequencia = it-nota-fisc.nr-seq-ped
                              NO-LOCK:
            ASSIGN i-tot-vol-nf  = i-tot-vol-nf  + 1
                   i-tot-vol-tra = i-tot-vol-tra + 1
                   i-tot-vol-ger = i-tot-vol-ger + 1.
        END.
    END.

    DISPLAY nota-fiscal.dt-emis-nota   
            nota-fiscal.nr-nota-fis 
            nota-fiscal.nome-tr-red
            emitente.nome-abrev                      
            nota-fiscal.cidade                  
            nota-fiscal.estado FORMAT "x(2)"                
            nota-fiscal.vl-tot-nota
            de-tot-qtd-not
            nota-fiscal.peso-bru-tot
            c-tp-frete
            i-tot-vol-nf
            WITH FRAME f-detalhe.
    DOWN WITH FRAME f-detalhe.

    ASSIGN de-tot-qtd-tra = de-tot-qtd-tra + de-tot-qtd-not 
           de-tot-qtd-ger = de-tot-qtd-ger + de-tot-qtd-not
           de-tot-vlr-tra = de-tot-vlr-tra + nota-fiscal.vl-tot-nota
           de-tot-vlr-ger = de-tot-vlr-ger + nota-fiscal.vl-tot-nota
           de-tot-pes-tra = de-tot-pes-tra + nota-fiscal.peso-bru-tot
           de-tot-pes-ger = de-tot-pes-ger + nota-fiscal.peso-bru-tot
           i-tot-vol-nf   = 0.

    IF LAST-OF(nota-fiscal.nome-transp) THEN DO:
       DISPLAY "Tot.Transp"   @ nota-fiscal.dt-emis-nota
               de-tot-qtd-tra @ de-tot-qtd-not
               de-tot-vlr-tra @ nota-fiscal.vl-tot-nota
               de-tot-pes-tra @ nota-fiscal.peso-bru-tot
               i-tot-vol-tra  @ i-tot-vol-nf
               WITH FRAME f-detalhe.
       DOWN 2 WITH FRAME f-detalhe.
       ASSIGN de-tot-qtd-tra = 0
              de-tot-vlr-tra = 0
              de-tot-pes-tra = 0
              i-tot-vol-tra  = 0.
    END.
END.
       
DISPLAY "Tot.Geral"    @ nota-fiscal.dt-emis-nota
        de-tot-qtd-ger @ de-tot-qtd-not
        de-tot-vlr-ger @ nota-fiscal.vl-tot-nota
        de-tot-pes-ger @ nota-fiscal.peso-bru-tot
        i-tot-vol-ger  @ i-tot-vol-nf
        WITH FRAME f-detalhe.
ASSIGN de-tot-vlr-ger = 0
       de-tot-qtd-ger = 0
       de-tot-pes-ger = 0
       i-tot-vol-ger  = 0.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.cod-estabel
           tt-param.ini-dt-emissao
           tt-param.fin-dt-emissao
           tt-param.ini-dt-embarque
           tt-param.fin-dt-embarque
           tt-param.ini-dt-saida
           tt-param.fin-dt-saida
           tt-param.ini-transp  
           tt-param.fin-transp 
           tt-param.ini-transp-red
           tt-param.fin-transp-red
           tt-param.desc-tipo-frete
           tt-param.imp-sedex
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

