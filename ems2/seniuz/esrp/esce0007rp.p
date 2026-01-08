/* Programa: ESCE043.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Controle de Estoque
** Objetivo: Listar as Necessidades de Compra por Item
** Autor...: Gilvando de Souza Araujo - Setembro/2000
**           Alterado Por Fabio Coelho Lanza em: ABRIL/2001
**           Excluido as Colunas: UNIDADE/ESTABELECIMENTO/DEPOSITO.
**           Incluido as Colunas: PREV.ENTR   PREV.ENTR   PREV.ENTR   PREV.ENTR
**                                (00 A 07)   (08 A 15)   (09 A 22)   (23 A 30)
**           Alterado Por Fabio Coelho Lanza em: SETEMBRO/2002
**           Incluido Na Selecao: Obsoleto De:
**                                        Ate: 
** Obs.....: Especifico da TEAT TEXTIL IND.COM.TDA.
**
** Conversao para EMS 2.04:
**   Programa: ESCE043.P  =>  ESCE0007RP.P
**   Autor...: FµBIO COELHO LANZA
**   Data....: 16/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCE0007RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino             as integer
       field arquivo             as char format "x(35)"
       field usuario             as char format "x(12)"
       field data-exec           as date
       field hora-exec           as integer
       field classifica          as integer
       field desc-classifica     as char format "x(40)"
       FIELD cod-estabel         LIKE saldo-estoq.cod-estabel
       FIELD ini-it-codigo       LIKE item.it-codigo      
       FIELD fin-it-codigo       LIKE item.it-codigo      
       FIELD ini-cod-obsoleto    LIKE item.cod-obsoleto     
       FIELD fin-cod-obsoleto    LIKE item.cod-obsoleto       
       FIELD ini-cod-depos       LIKE saldo-estoq.cod-depos  
       FIELD fin-cod-depos       LIKE saldo-estoq.cod-depos
       FIELD tipo-estoque        AS INTEGER
       FIELD desc-tipo-estoq     AS CHAR FORMAT "x(20)"
       FIELD imp-param           AS LOG.

define temp-table tt-digita no-undo
       field it-codigo LIKE ITEM.it-codigo
       index id it-codigo.

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

def var c-it-codigo    like item.it-codigo.
def var c-descricao    as char format "x(33)".
def var de-prv-ent1    as dec format ">>>,>>>,>>9".
def var de-prv-ent2    as dec format ">>>,>>>,>>9".
def var de-prv-ent3    as dec format ">>>,>>>,>>9".
def var de-prv-ent4    as dec format ">>>,>>>,>>9".
def var de-nec-compra  as dec format "->>>>,>>>,>>9.99".
DEF VAR l-lista-item   AS LOG.
DEF VAR c-lista-item   AS CHAR.

FOR EACH tt-raw-digita.
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
    IF c-lista-item <> "" THEN
       ASSIGN c-lista-item = c-lista-item + ",".
    ASSIGN c-lista-item = c-lista-item + tt-digita.it-codigo.
END.

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelecimento." SKIP
    tt-param.ini-it-codigo    label "Item............"
    "A"  AT 35
    tt-param.fin-it-codigo    NO-LABELS SKIP
    tt-param.ini-cod-obsoleto label "Obsoleto........"
    "A"  AT 35
    tt-param.fin-cod-obsoleto no-labels SKIP
    tt-param.ini-cod-depos    label "Deposito........"
    "A"  AT 35
    tt-param.fin-cod-depos    no-labels SKIP
    tt-param.desc-tipo-estoq  LABEL "Tipo de Estoque."
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    c-it-codigo               column-label "CODIGO"
    c-descricao               column-label "DESCRICAO DO ITEM"
    saldo-estoq.qtidade-atu   column-label "QTDE ATUAL"
    de-nec-compra             column-label "QTDE A COMPRAR"
    de-prv-ent1               column-label "PREV.ENTR!01 a 07"
    de-prv-ent2               column-label "PREV.ENTR!08 a 15"
    de-prv-ent3               column-label "PREV.ENTR!09 a 22"
    de-prv-ent4               column-label "PREV.ENTR!23 a 30"
    with no-box 55 down width 132 STREAM-IO FRAME f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i MATERIAIS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Necessidade_de_Compra_Por_Item * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each item where item.it-codigo    >= tt-param.ini-it-codigo
                and item.it-codigo    <= tt-param.fin-it-codigo
                and item.cod-obsoleto >= tt-param.ini-cod-obsoleto
                and item.cod-obsoleto <= tt-param.fin-cod-obsoleto
                and (lookup(ITEM.it-codigo,c-lista-item) <> 0 or 
                                           c-lista-item = "")
              no-lock
              break by if tt-param.classific = 1 then item.it-codigo
                       else item.descricao-1:
    assign c-it-codigo = item.it-codigo
           c-descricao = item.descricao-1 + item.descricao-2.
    for each saldo-estoq where saldo-estoq.cod-estabel  = tt-param.cod-estabel
                           and saldo-estoq.cod-depos   >= tt-param.ini-cod-depos
                           and saldo-estoq.cod-depos   <= tt-param.fin-cod-depos
                           and saldo-estoq.it-codigo    =  item.it-codigo
                               no-lock:
 
        run pi-acompanhar in h-acomp (input item.it-codigo).

        assign l-lista-item  = yes
               de-nec-compra = item.quant-segur - saldo-estoq.qtidade-atu.
        if tt-param.tipo-estoque = 1 then
           if de-nec-compra <= 0 then
              assign l-lista-item = no.
        if tt-param.tipo-estoque = 2 then
           if de-nec-compra <> 0 then
              assign l-lista-item = no.
        if tt-param.tipo-estoque = 3 then
           if de-nec-compra >= 0 then
              assign l-lista-item = no.
        
        if l-lista-item then do:
           for each prazo-compra
               where prazo-compra.it-codigo           = item.it-codigo
                 and prazo-compra.quant-saldo         > 0
                 and month(prazo-compra.data-entrega) = month(today)
                 and year(prazo-compra.data-entrega)  = year(today)
                     no-lock:
               if day(prazo-compra.data-entrega) <= 7 then
                  assign de-prv-ent1 = de-prv-ent1 +  prazo-compra.quant-saldo.
               if  day(prazo-compra.data-entrega) >= 8
               and day(prazo-compra.data-entrega) <= 15 then
                   assign de-prv-ent2 = de-prv-ent2 +  prazo-compra.quant-saldo.
               if  day(prazo-compra.data-entrega) >= 16
               and day(prazo-compra.data-entrega) <= 22 then
                   assign de-prv-ent3 = de-prv-ent3 +  prazo-compra.quant-saldo.
               if day(prazo-compra.data-entrega) >= 23 then
                  assign de-prv-ent4 = de-prv-ent4 +  prazo-compra.quant-saldo.
           end.          
        
           if c-it-codigo <> "" then do:
              display c-it-codigo
                      c-descricao
                      saldo-estoq.qtidade-atu format "->>>>,>>>,>>9.99"
                      de-nec-compra
                      de-prv-ent1
                      de-prv-ent2
                      de-prv-ent3
                      de-prv-ent4
                      with frame f-detalhe.
              down with frame f-detalhe.
          end.
          
          ASSIGN c-it-codigo   = ""
                 c-descricao   = ""
                 de-prv-ent1   = 0
                 de-prv-ent2   = 0
                 de-prv-ent3   = 0
                 de-prv-ent4   = 0.
        end.
    end.
end.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.cod-estabel
           tt-param.ini-it-codigo
           tt-param.fin-it-codigo 
           tt-param.ini-cod-obsoleto     
           tt-param.fin-cod-obsoleto     
           tt-param.ini-cod-depos
           tt-param.fin-cod-depos
           tt-param.desc-tipo-estoq
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

