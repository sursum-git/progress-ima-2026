/* Programa: ESCE009.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Controle de Estoque
** Objetivo: Listar o saldo atual de estoques por itens/referencia 
** Autor...: Gilvando de Souza Araujo - Abril/1998
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Autor: Prodb - Toninho
**   Data: 06/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCE0005RP 2.04.00.000}


define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       FIELD c-cod-estabel    LIKE estabelec.cod-estabel
       FIELD c-item-ini       LIKE ITEM.it-codigo
       FIELD c-item-fim       LIKE ITEM.it-codigo
       FIELD c-ref-ini        LIKE ref-item.cod-refer
       FIELD c-ref-fim        LIKE ref-item.cod-refer
       FIELD c-acond-ini      AS CHAR FORMAT "x(5)" 
       FIELD c-acond-fim      AS CHAR FORMAT "x(5)" 
       FIELD c-lote-ini       AS CHAR FORMAT "x(2)" 
       FIELD c-lote-fim       AS CHAR FORMAT "x(2)" 
       FIELD c-obso-ini       LIKE item-ext.cod-obsoleto
       FIELD c-obso-fim       LIKE item-ext.cod-obsoleto
       FIELD c-estoq-bloq     AS CHARACTER
       FIELD c-opcao-list     AS CHARACTER FORMAT "x(9)"
       FIELD mascara-item     AS CHAR FORMAT "x(6)"
       FIELD mascara-refer    AS CHAR FORMAT "x(7)"
       FIELD l-lista-teccru   AS LOGICAL
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

def TEMP-TABLE tt-work  /* Itens s/fator de conversao */
    field it-codigo like item.it-codigo.

def var l-prim-vez      as log.
def var l-falta-fator   as log.
def var l-lista-item    as log.
def var c-acond         AS CHAR FORMAT "x(5)".
def var c-sit-prog      as char.
def var de-est-item-pec as dec format "->>,>>>,>>9.99".
def var de-est-item-rol like de-est-item-pec.
def var de-est-item-out like de-est-item-pec.
def var de-res-item-pec like de-est-item-pec.
def var de-res-item-rol like de-est-item-pec.
def var de-res-item-out like de-est-item-pec.
def var de-res-item-nfa as dec extent 3 format "->>,>>>,>>9.99".
def var de-car-item-pec like de-est-item-pec.
def var de-car-item-rol like de-est-item-pec.
def var de-car-item-out like de-est-item-pec.
def var de-est-item     like de-est-item-pec.
def var de-res-item     like de-est-item-pec.
def var de-car-item     like de-car-item-pec.
def var de-tot-est-it   like de-car-item-pec.
def var de-tot-res-it   like de-car-item-pec.
def var de-res-it-nfa   as dec extent 3 format "->>,>>>,>>9.99".
def var de-tot-car-it   like de-car-item-pec.
def var de-tot-est-ger  like de-car-item-pec.
def var de-tot-res-ger  like de-car-item-pec.
def var de-res-ger-nfa  as dec extent 3 format "->>,>>>,>>9.99".
def var de-tot-car-ger  like de-car-item-pec.
def var de-disponivel   like de-car-item-pec.
def var de-car-nres     like de-car-item-pec.

form
    tt-param.c-cod-estabel  label "Estabelecimento." SKIP
    tt-param.c-item-ini     label "Item............"
    "a" AT 36
    tt-param.c-item-fim     no-labels                SKIP
    tt-param.c-ref-ini      LABEL "Referencia......"
    "a" AT 36
    tt-param.c-ref-fim      NO-LABELS                SKIP
    tt-param.c-acond-ini    LABEL "Acondicionamento"
    "a" AT 36
    tt-param.c-acond-fim    NO-LABELS                SKIP
    tt-param.c-lote-ini     LABEL "Lote............"
    "a" AT 36
    tt-param.c-lote-fim     NO-LABELS                SKIP
    tt-param.c-obso-ini     label "C¢digo Obsoleto." 
    "a" AT 36
    tt-param.c-obso-fim     no-labels                SKIP
    tt-param.c-opcao-list   label "Op‡Æo de Saldo.." SKIP
    tt-param.mascara-item   LABEL "M scara Item...." SKIP
    tt-param.mascara-refer  LABEL "M scara Refer..." skip(1)   
    with no-box side-labels width 132 stream-io frame f-parlis.

form
    item.it-codigo            column-label "ITEM"        format "x(6)"
    ref-item.cod-refer        COLUMN-LABEL "REF"         FORMAT "x(8)"
    item.desc-item            column-label "DESCRICAO"   FORMAT "x(28)"  
    item.un                   column-label "UN"
    ref-item-ext.cod-obsoleto column-label "COb"         format "x(1)"
    de-est-item               column-label "QTD ATUAL"
    de-res-item               column-label "QTD RESERVADA"
    de-disponivel             column-label "SLD DISPONIVEL"
    de-car-nres               column-label "CART.N/RESERV."
    c-acond                   column-label "ACONDIC."
    c-sit-prog                column-label "PrPcPt"      format "x(5)"
    with no-box down width 132 STREAM-IO frame f-detalhe.

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
{utp/ut-liter.i Listagem_Estoque_Atual_/_Disponivel_por_Item * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each tt-work.
    delete tt-work.
end.

FOR EACH item WHERE item.it-codigo >= tt-param.c-item-ini 
                AND item.it-codigo <= tt-param.c-item-fim 
                AND ((SUBSTR(ITEM.it-codigo,6,1) =  "0" AND tt-param.l-lista-teccru = YES) OR
                     (SUBSTR(ITEM.it-codigo,6,1) <> "0"))
              NO-LOCK,
    EACH ref-item WHERE ref-item.it-codigo =  ITEM.it-codigo 
                    AND ref-item.cod-refer >= tt-param.c-ref-ini 
                    AND ref-item.cod-refer <= tt-param.c-ref-fim
                  NO-LOCK,
    EACH ref-item-ext WHERE
         ref-item-ext.it-codigo     = ref-item.it-codigo AND
         ref-item-ext.cod-refer     = ref-item.cod-refer AND
         ref-item-ext.cod-obsoleto >= tt-param.c-obso-ini AND
         ref-item-ext.cod-obsoleto <= tt-param.c-obso-fim NO-LOCK

    BREAK BY item.it-codigo:

    RUN pi-acompanhar in h-acomp (input item.it-codigo + " " + ref-item.cod-refer).

    assign de-est-item-pec    = 0        
           de-est-item-out    = 0        
           de-res-item-rol    = 0        
           de-res-item-nfa[1] = 0        
           de-res-item-nfa[3] = 0        
           de-car-item-rol    = 0        
           de-est-item-rol    = 0 
           de-res-item-pec    = 0 
           de-res-item-out    = 0 
           de-res-item-nfa[2] = 0 
           de-car-item-pec    = 0 
           de-car-item-out    = 0.
    
    /* Verifica Saldo */
    FOR EACH saldo-estoq 
        WHERE saldo-estoq.cod-estabel    = tt-param.c-cod-estabel 
          AND saldo-estoq.it-codigo      = item.it-codigo 
          AND saldo-estoq.cod-refer      = ref-item.cod-refer
          AND ((ref-item-ext.bloqueio-pp = YES AND saldo-estoq.lote BEGINS "pp" AND tt-param.c-estoq-bloq = "B") OR
               (ref-item-ext.bloqueio-pp = NO  AND saldo-estoq.lote BEGINS "pp" AND tt-param.c-estoq-bloq = "L") OR
               (ref-item-ext.bloqueio-pd = YES AND saldo-estoq.lote BEGINS "pd" AND tt-param.c-estoq-bloq = "B") OR
               (ref-item-ext.bloqueio-pd = NO  AND saldo-estoq.lote BEGINS "pd" AND tt-param.c-estoq-bloq = "L") OR
               (ref-item-ext.bloqueio-rp = YES AND saldo-estoq.lote BEGINS "rp" AND tt-param.c-estoq-bloq = "B") OR
               (ref-item-ext.bloqueio-rp = NO  AND saldo-estoq.lote BEGINS "rp" AND tt-param.c-estoq-bloq = "L") OR
               (ref-item-ext.bloqueio-rd = YES AND saldo-estoq.lote BEGINS "rd" AND tt-param.c-estoq-bloq = "B") OR
               (ref-item-ext.bloqueio-rd = NO  AND saldo-estoq.lote BEGINS "rd" AND tt-param.c-estoq-bloq = "L") OR
                                                                                   (tt-param.c-estoq-bloq = "A"))
        NO-LOCK:

        IF saldo-estoq.cod-depos   <> "exp" OR
           saldo-estoq.qtidade-atu =  0     OR
           SUBSTR(saldo-estoq.lote,1,2) < tt-param.c-lote-ini OR
           SUBSTR(saldo-estoq.lote,1,2) > tt-param.c-lote-fim then next.

        if substr(saldo-estoq.lote,1,2) = "PP" then
           assign de-est-item-pec = de-est-item-pec +
                                    saldo-estoq.qtidade-atu.
        else
        if substr(saldo-estoq.lote,1,2) = "RP" then
           assign de-est-item-rol = de-est-item-rol +
                                    saldo-estoq.qtidade-atu.
        else
           assign de-est-item-out = de-est-item-out +
                                    saldo-estoq.qtidade-atu.
    END. /* saldo-estoq */

    /* Verifica Reservas */
    FOR EACH ped-item-res WHERE 
             ped-item-res.it-codigo = item.it-codigo AND
             ped-item-res.cod-refer = ref-item.cod-refer AND
             ((ref-item-ext.bloqueio-pp = YES AND ped-item-res.lote BEGINS "pp" AND tt-param.c-estoq-bloq = "B") OR           
              (ref-item-ext.bloqueio-pp = NO  AND ped-item-res.lote BEGINS "pp" AND tt-param.c-estoq-bloq = "L") OR           
              (ref-item-ext.bloqueio-pd = YES AND ped-item-res.lote BEGINS "pd" AND tt-param.c-estoq-bloq = "B") OR           
              (ref-item-ext.bloqueio-pd = NO  AND ped-item-res.lote BEGINS "pd" AND tt-param.c-estoq-bloq = "L") OR           
              (ref-item-ext.bloqueio-rp = YES AND ped-item-res.lote BEGINS "rp" AND tt-param.c-estoq-bloq = "B") OR           
              (ref-item-ext.bloqueio-rp = NO  AND ped-item-res.lote BEGINS "rp" AND tt-param.c-estoq-bloq = "L") OR           
              (ref-item-ext.bloqueio-rd = YES AND ped-item-res.lote BEGINS "rd" AND tt-param.c-estoq-bloq = "B") OR           
              (ref-item-ext.bloqueio-rd = NO  AND ped-item-res.lote BEGINS "rd" AND tt-param.c-estoq-bloq = "L") OR           
                                                                                   (tt-param.c-estoq-bloq = "A"))             
        NO-LOCK:
        ASSIGN de-res-item = 0.

        IF ped-item-res.faturado = YES THEN DO:
           FIND nota-fiscal WHERE
                nota-fiscal.cod-estabel = ped-item-res.cod-estabel AND
                nota-fiscal.serie       = ped-item-res.serie AND
                nota-fiscal.nr-nota-fis = STRING(ped-item-res.nr-nota-fis,"9999999")
                NO-LOCK NO-ERROR.
           IF AVAIL nota-fiscal THEN DO.
              IF nota-fiscal.ind-sit-nota <= 2 AND
                 nota-fiscal.dt-confirma = ? AND
                 nota-fiscal.dt-cancela = ? THEN
                 ASSIGN de-res-item = ped-item-res.qt-pedida.
           END.
        END.
        ELSE
            ASSIGN de-res-item = ped-item-res.qt-pedida.
        
        if de-res-item = 0 then next.

        if substr(ped-item-res.lote,1,2) = "PP" then do:
           assign de-res-item-pec = de-res-item-pec + de-res-item.
           if ped-item-res.faturado = no then
              assign de-res-item-nfa[1] = de-res-item-nfa[1] + de-res-item.
        end.
        else
        if substr(ped-item-res.lote,1,2) = "RP" then do:
           assign de-res-item-rol = de-res-item-rol + de-res-item.
           if ped-item-res.faturado = no then
              assign de-res-item-nfa[2] = de-res-item-nfa[2] + de-res-item.
        end.
        else do:
           assign de-res-item-out = de-res-item-out + de-res-item.
           if ped-item-res.faturado = no then
              assign de-res-item-nfa[3] = de-res-item-nfa[3] + de-res-item.
        end.
    end. /* ped-item-res */              
    
    /* --- Pesquisa Carteira --- */
    FOR EACH ped-item
        WHERE ped-item.it-codigo = ITEM.it-codigo AND
              ped-item.cod-refer = ref-item.cod-refer NO-LOCK,
        EACH ped-item-ext OF ped-item
        WHERE ((ref-item-ext.bloqueio-pp = YES AND ped-item-ext.lote BEGINS "pp" AND tt-param.c-estoq-bloq = "B") OR           
               (ref-item-ext.bloqueio-pp = NO  AND ped-item-ext.lote BEGINS "pp" AND tt-param.c-estoq-bloq = "L") OR           
               (ref-item-ext.bloqueio-pd = YES AND ped-item-ext.lote BEGINS "pd" AND tt-param.c-estoq-bloq = "B") OR           
               (ref-item-ext.bloqueio-pd = NO  AND ped-item-ext.lote BEGINS "pd" AND tt-param.c-estoq-bloq = "L") OR           
               (ref-item-ext.bloqueio-rp = YES AND ped-item-ext.lote BEGINS "rp" AND tt-param.c-estoq-bloq = "B") OR           
               (ref-item-ext.bloqueio-rp = NO  AND ped-item-ext.lote BEGINS "rp" AND tt-param.c-estoq-bloq = "L") OR           
               (ref-item-ext.bloqueio-rd = YES AND ped-item-ext.lote BEGINS "rd" AND tt-param.c-estoq-bloq = "B") OR           
               (ref-item-ext.bloqueio-rd = NO  AND ped-item-ext.lote BEGINS "rd" AND tt-param.c-estoq-bloq = "L") OR           
                                                                                    (tt-param.c-estoq-bloq = "A"))             
               NO-LOCK:
        
        ASSIGN de-car-item = 0.
        
        IF ped-item.cod-sit-item < 3 
        OR ped-item.cod-sit-item = 5 THEN 
           ASSIGN de-car-item = ped-item.qt-pedida -
                                ped-item.qt-atendida +
                                ped-item.qt-devolvida.
    
        IF de-car-item = 0 THEN NEXT.

        if substr(ped-item-ext.lote,1,2) = "PP" then
           assign de-car-item-pec = de-car-item-pec +
                                    de-car-item.
        else
        if substr(ped-item-ext.lote,1,2) = "RP" then
           assign de-car-item-rol = de-car-item-rol +
                                    de-car-item.
        else
           assign de-car-item-out = de-car-item-out +
                                    de-car-item.
    end. /* ped-item */

    /* Verifica Programacao */

    assign c-sit-prog = "N N N".
    if ref-item-ext.qtd-prog <> 0 then
       assign c-sit-prog = "S ".
    else
       assign c-sit-prog = "N ".
    if ref-item-ext.qtd-proc <> 0 then
       assign c-sit-prog = c-sit-prog + "S ".
    else 
       assign c-sit-prog = c-sit-prog + "N ".
    if ref-item-ext.qtd-pron <> 0 then
       assign c-sit-prog = c-sit-prog + "S".
    else 
       assign c-sit-prog = c-sit-prog + "N".
         
    assign de-est-item = de-est-item-pec +
                         de-est-item-rol +
                         de-est-item-out
           de-res-item = de-res-item-pec +
                         de-res-item-rol +
                         de-res-item-out
           de-car-item = de-car-item-pec +
                         de-car-item-rol +
                         de-car-item-out.
            
    if  de-est-item-pec = 0 AND de-est-item-rol = 0 AND
        de-est-item-out = 0 and de-res-item     = 0 AND
        de-car-item     = 0 then next.
            
    assign de-disponivel = de-est-item - de-res-item 
           de-car-nres   = de-car-item - de-res-item-nfa[1]
                                       - de-res-item-nfa[2]
                                       - de-res-item-nfa[3].
    if de-car-nres < 0 then
       assign de-car-nres = 0.
      
    assign l-lista-item = yes.
    /* Em Teste - Gilvando 24.02.2005        
    if tt-param.c-opcao-list = "Positivos" then DO:
       if (de-est-item-pec <= 0 and
           de-est-item-rol <= 0 and
           de-est-item-out <= 0) then
          assign l-lista-item = no.
    END.
    ELSE
    if tt-param.c-opcao-list = "Negativos" THEN DO:
       if (de-est-item-pec >= 0 and
           de-est-item-rol >= 0 and
           de-est-item-out >= 0) then
          assign l-lista-item = no.
    END.
    */
    IF (tt-param.c-opcao-list = "Positivos" AND de-disponivel <= 0) OR
       (tt-param.c-opcao-list = "Negativos" AND de-disponivel >= 0) THEN
       ASSIGN l-lista-item = NO.

    if l-lista-item then do:
       display item.it-codigo
               ref-item.cod-refer
               item.desc-item
               item.un
               ref-item-ext.cod-obsoleto      
               with frame f-detalhe.
   
       if de-est-item-pec <> 0
       or de-res-item-pec <> 0
       or de-car-item-pec <> 0 then do:
          assign de-disponivel = de-est-item-pec - de-res-item-pec
                 de-car-nres   = de-car-item-pec - de-res-item-nfa[1]
                 c-acond       = "Peca".
          if de-car-nres < 0 then
             assign de-car-nres = 0.
             display de-est-item-pec @ de-est-item
                     de-res-item-pec @ de-res-item
                     de-disponivel
                     de-car-nres
                     c-acond
                     c-sit-prog
                     with frame f-detalhe.
             down with frame f-detalhe.
       end.
    
       if de-est-item-rol <> 0
       or de-res-item-rol <> 0
       or de-car-item-rol <> 0 then do:
          assign de-disponivel = de-est-item-rol - de-res-item-rol
                 de-car-nres   = de-car-item-rol - de-res-item-nfa[2]
                 c-acond       = "Rolo".
          if de-car-nres < 0 then
             assign de-car-nres = 0.

             display de-est-item-rol @ de-est-item
                     de-res-item-rol @ de-res-item
                     de-disponivel
                     de-car-nres
                     c-acond
                     c-sit-prog
                     with frame f-detalhe.
             down with frame f-detalhe.
       end.
    
       if de-est-item-out <> 0
       or de-res-item-out <> 0
       or de-car-item-out <> 0 then do:
          assign de-disponivel = de-est-item-out - de-res-item-out
                 de-car-nres   = de-car-item-out - de-res-item-nfa[3]
                 c-acond       = "Rd/Pd".
          if de-car-nres < 0 then
             assign de-car-nres = 0.

             display de-est-item-out @ de-est-item
                     de-res-item-out @ de-res-item
                     de-disponivel
                     de-car-nres
                     c-acond
                     c-sit-prog
                     with frame f-detalhe.
             down with frame f-detalhe.
       end.
    
       /* Conversao de Outras UN para M */
       if item.un <> "m" then do:
          if avail item-ext then
             assign de-est-item = de-est-item * item-ext.fator-conv
                    de-res-item = de-res-item * item-ext.fator-conv
                    de-car-item = de-car-item * item-ext.fator-conv.
          else do:
             assign l-falta-fator = yes.
             find first tt-work
                  where tt-work.it-codigo = item.it-codigo no-lock no-error.
             if not avail tt-work then do:
                create tt-work.
                assign tt-work.it-codigo = item.it-codigo.
             end.
          end.   
       end.
       assign de-tot-est-it    = de-tot-est-it + de-est-item
              de-tot-res-it    = de-tot-res-it + de-res-item
              de-res-it-nfa[1] = de-res-it-nfa[1] +
                                  de-res-item-nfa[1]
              de-res-it-nfa[2] = de-res-it-nfa[2] +
                                  de-res-item-nfa[2]
              de-res-it-nfa[3] = de-res-it-nfa[3] +
                                  de-res-item-nfa[3]
              de-tot-car-it    = de-tot-car-it + de-car-item.
         
       assign de-tot-est-ger    = de-tot-est-ger + de-est-item
              de-tot-res-ger    = de-tot-res-ger + de-res-item
              de-res-ger-nfa[1] = de-res-ger-nfa[1] +
                                  de-res-item-nfa[1]
              de-res-ger-nfa[2] = de-res-ger-nfa[2] +
                                  de-res-item-nfa[2]
              de-res-ger-nfa[3] = de-res-ger-nfa[3] +
                                  de-res-item-nfa[3] 
              de-tot-car-ger    = de-tot-car-ger + de-car-item.
    end.        

    if last-of(item.it-codigo) then do:
       if de-tot-est-it <> 0
       or de-tot-res-it <> 0
       or de-tot-car-it <> 0 then do:
          assign de-disponivel = de-tot-est-it - de-tot-res-it
                 de-car-nres   = de-tot-car-it - de-res-it-nfa[1]
                                                - de-res-it-nfa[2]
                                                - de-res-it-nfa[3].
          if de-car-nres < 0 then 
             assign de-car-nres = 0.

             display "Total do Item" @ item.desc-item
                     de-tot-est-it   @ de-est-item
                     de-tot-res-it   @ de-res-item
                     de-disponivel
                     de-car-nres
                     with frame f-detalhe.
             down 2 with frame f-detalhe.
          
          assign de-tot-est-it    = 0
                 de-tot-res-it    = 0
                 de-res-it-nfa[1] = 0
                 de-res-it-nfa[2] = 0
                 de-res-it-nfa[3] = 0
                 de-tot-car-it    = 0.
       end.
    end.
end. /* item */

if de-tot-est-it <> 0
or de-tot-res-it <> 0
or de-tot-car-it <> 0 then do:
   assign de-disponivel = de-tot-est-it - de-tot-res-it
          de-car-nres   = de-tot-car-it - de-res-it-nfa[1]
                                         - de-res-it-nfa[2]
                                         - de-res-it-nfa[3].
   if de-car-nres < 0 then
      assign de-car-nres = 0.

      display "Total do Item" @ item.desc-item
              de-tot-est-it   @ de-est-item
              de-tot-res-it   @ de-res-item
              de-disponivel
              de-car-nres
              with frame f-detalhe.
      down 2 with frame f-detalhe.
end.
                  
assign de-tot-est-it    = 0
       de-tot-res-it    = 0
       de-res-it-nfa[1] = 0
       de-res-it-nfa[2] = 0
       de-res-it-nfa[3] = 0
       de-tot-car-it    = 0.

if de-tot-est-ger <> 0
or de-tot-res-ger <> 0
or de-tot-car-ger <> 0 then do:
   assign de-disponivel = de-tot-est-ger - de-tot-res-ger
          de-car-nres   = de-tot-car-ger - de-res-ger-nfa[1]
                                         - de-res-ger-nfa[2]
                                         - de-res-ger-nfa[3].
   if de-car-nres < 0 then
      assign de-car-nres = 0.
      display "Total Geral"   @ item.desc-item
              de-tot-est-ger  @ de-est-item
              de-tot-res-ger  @ de-res-item
              de-disponivel
              de-car-nres
              with frame f-detalhe.
      if l-falta-fator then do:
         page.
         put "Atencao ! - Ha itens sem fator de conversao:"
             skip(1).
         for each tt-work:
             display tt-work.it-codigo @ item.it-codigo
                     with frame f-detalhe.
             down with frame f-detalhe.
         end.
         for each tt-work.
             delete tt-work.
         end.
      end.   
end.
                  
assign de-tot-est-ger    = 0
       de-tot-res-ger    = 0
       de-res-ger-nfa[1] = 0
       de-res-ger-nfa[2] = 0
       de-res-ger-nfa[3] = 0
       de-tot-car-ger    = 0.

IF tt-param.impr-param THEN DO.
   PAGE.
   PUT "*****--------------- PAR¶METROS ---------------*****"
       SKIP(1).
    
   display tt-param.c-cod-estabel
           tt-param.c-item-ini 
           tt-param.c-item-fim
           tt-param.c-ref-ini  
           tt-param.c-ref-fim  
           tt-param.c-acond-ini
           tt-param.c-acond-fim
           tt-param.c-lote-ini 
           tt-param.c-lote-fim 
           tt-param.c-obso-ini
           tt-param.c-obso-fim
           tt-param.c-opcao-list
           tt-param.mascara-item 
           tt-param.mascara-refer
           with frame f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

