/* Programa: ESFT0011RP.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Gerar o relatorio de Faturamento com Pre‡o Medio por tipo de Pedido
** Autor...: Gilvando de Souza Araujo - Setembro/2004
** Obs.....: Especifico da TEAR TÒXTIL INDéSTRIA E COMRCIO LTDA.
**
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0011RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       field classifica       as integer
       FIELD estab-ini        like nota-fiscal.cod-estabel              
       FIELD estab-fin        like nota-fiscal.cod-estabel   
       FIELD serie-ini        LIKE nota-fiscal.serie 
       FIELD serie-fin        LIKE nota-fiscal.serie
       FIELD espdoc-ini       like nota-fiscal.esp-docto
       FIELD espdoc-fin       like nota-fiscal.esp-docto
       FIELD dt-emis-ini      LIKE nota-fiscal.dt-emis-nota
       FIELD dt-emis-fin      LIKE nota-fiscal.dt-emis-nota
       FIELD arq-topazio      AS CHAR FORMAT "x(45)"
       FIELD arq-prata        AS CHAR FORMAT "x(45)"
       FIELD arq-normal       AS CHAR FORMAT "x(45)"
       FIELD arq-todos        AS CHAR FORMAT "x(45)"
       FIELD impr-param       AS   LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

def temp-table w-work
    field it-codigo    like it-nota-fisc.it-codigo
    FIELD cod-refer    LIKE it-nota-fisc.cod-refer
    field aliquota-icm like it-nota-fisc.aliquota-icm
    field quantidade   as dec format ">>>,>>>,>>9.99"
    field valor        as dec format ">>>,>>>,>>9.99"
    INDEX ch-work it-codigo
                  cod-refer
                  aliquota-icm.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

DEF STREAM saida.

form
    tt-param.estab-ini    LABEL "Estabelecimento.." AT 1
    "a"                                             AT 31
    tt-param.estab-fin    NO-LABELS
    tt-param.serie-ini    LABEL "Serie............" AT 1
    "a"                                             AT 31
    tt-param.serie-fin    NO-LABELS
    tt-param.espdoc-ini   LABEL "Esp‚cie.........." AT 1
    "a"                                             AT 31
    tt-param.espdoc-fin   NO-LABELS
    tt-param.dt-emis-ini  LABEL "Data EmissÆo....." AT 1
    "a"                                             AT 31
    tt-param.dt-emis-fin  NO-LABELS
    tt-param.arq-topazio  LABEL "Arq.saida Topazio" AT 1
    tt-param.arq-prata    LABEL "Arq.saida Prata.." AT 1
    tt-param.arq-normal   LABEL "Arq.saida Normal." AT 1
    tt-param.arq-todos    LABEL "Arq.saida Todos.." AT 1
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

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
{utp/ut-liter.i Faturamento_com_Pre‡o_M‚dio_por_Tipo_de_Pedido * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

output STREAM saida to value(tt-param.arq-topazio).
put STREAM saida 
           "Item;"       
           "Descricao;" 
           "Refer;"     
           "% ICM;"     
           "Quantidade;"
           "Valor;"
           "PreMedio"
           skip.

for each nota-fiscal where nota-fiscal.cod-estabel  >= tt-param.estab-ini
                       AND nota-fiscal.cod-estabel  <= tt-param.estab-fin
                       and nota-fiscal.serie        >= tt-param.serie-ini
                       AND nota-fiscal.serie        <= tt-param.serie-fin
                       and nota-fiscal.dt-emis-nota >= tt-param.dt-emis-ini
                       and nota-fiscal.dt-emis-nota <= tt-param.dt-emis-fin
                       and nota-fiscal.esp-docto    >= tt-param.espdoc-ini
                       AND nota-fiscal.esp-docto    <= tt-param.espdoc-fin
                       and nota-fiscal.dt-cancela   = ?
                       AND nota-fiscal.emite-dup    = YES
                     no-lock,
    each ped-venda where ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                     and ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
                     and (ped-venda.tp-pedido = "T" or  
                          ped-venda.tp-pedido = "W")
                   no-lock:

    run pi-acompanhar in h-acomp (input "Topazio - Nota Fiscal: " + nota-fiscal.nr-nota-fis).

    for each it-nota-fisc of nota-fiscal no-lock,
        EACH ITEM OF it-nota-fisc
        WHERE ITEM.it-codigo >= "5"
          AND ITEM.it-codigo <= "5ZZZZZZZZZZZZZZZ"
          AND ITEM.ge-codigo >= 50
          AND ITEM.ge-codigo <= 59
        NO-LOCK,
        each natur-oper 
             where natur-oper.nat-operacao = it-nota-fisc.nat-operacao
               and natur-oper.emite-duplic = yes
             no-lock:
        
        find first w-work where w-work.it-codigo    = it-nota-fisc.it-codigo
                            AND w-work.cod-refer    = it-nota-fisc.cod-refer
                            and w-work.aliquota-icm = it-nota-fisc.aliquota-icm
                          no-lock no-error.
                          
        if not avail w-work then do:
           create w-work.
           assign w-work.it-codigo    = it-nota-fisc.it-codigo
                  w-work.cod-refer    = it-nota-fisc.cod-refer
                  w-work.aliquota-icm = it-nota-fisc.aliquota-icm
                  w-work.quantidade   = 0
                  w-work.valor        = 0.
        end.
        assign w-work.quantidade = w-work.quantidade + it-nota-fisc.qt-faturada[1]
               w-work.valor      = w-work.valor + it-nota-fisc.vl-tot-item.
    end.
end.

for each w-work:
    find item where item.it-codigo = w-work.it-codigo no-lock.
    put STREAM saida
        w-work.it-codigo    ";"
        item.desc-item      ";"
        w-work.cod-refer    ";"
        w-work.aliquota-icm ";"
        w-work.quantidade   ";"
        w-work.valor        ";"
        w-work.valor /
        w-work.quantidade
        SKIP.
end.

output STREAM saida close.

for each w-work:
    delete w-work.
end.

output STREAM saida to value(tt-param.arq-prata).
put STREAM saida 
           "Item;"       
           "Descricao;" 
           "Refer;"     
           "% ICM;"     
           "Quantidade;"
           "Valor;"
           "PreMedio" 
           skip.

for each nota-fiscal where nota-fiscal.cod-estabel  >= tt-param.estab-ini
                       AND nota-fiscal.cod-estabel  <= tt-param.estab-fin
                       and nota-fiscal.serie        >= tt-param.serie-ini
                       AND nota-fiscal.serie        <= tt-param.serie-fin
                       and nota-fiscal.dt-emis-nota >= tt-param.dt-emis-ini
                       and nota-fiscal.dt-emis-nota <= tt-param.dt-emis-fin
                       and nota-fiscal.esp-docto    >= tt-param.espdoc-ini
                       AND nota-fiscal.esp-docto    <= tt-param.espdoc-fin
                       and nota-fiscal.dt-cancela   = ?
                       AND nota-fiscal.emite-dup    = YES
                     no-lock,
    each ped-venda where ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                     and ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
                     and ped-venda.tp-pedido  = "P"
                   no-lock:

    run pi-acompanhar in h-acomp (input "Prata - Nota Fiscal: " + nota-fiscal.nr-nota-fis).

    for each it-nota-fisc of nota-fiscal no-lock,
        EACH ITEM OF it-nota-fisc
        WHERE ITEM.it-codigo >= "5"
          AND ITEM.it-codigo <= "5ZZZZZZZZZZZZZZZ"
          AND ITEM.ge-codigo >= 50
          AND ITEM.ge-codigo <= 59
        NO-LOCK,
        each natur-oper 
             where natur-oper.nat-operacao = it-nota-fisc.nat-operacao
               and natur-oper.emite-duplic = yes
             no-lock:
        
        find first w-work where w-work.it-codigo    = it-nota-fisc.it-codigo
                            AND w-work.cod-refer    = it-nota-fisc.cod-refer
                            and w-work.aliquota-icm = it-nota-fisc.aliquota-icm
                          no-lock no-error.
                          
        if not avail w-work then do:
           create w-work.
           assign w-work.it-codigo    = it-nota-fisc.it-codigo
                  w-work.cod-refer    = it-nota-fisc.cod-refer
                  w-work.aliquota-icm = it-nota-fisc.aliquota-icm
                  w-work.quantidade   = 0
                  w-work.valor        = 0.
        end.
        assign w-work.quantidade = w-work.quantidade + it-nota-fisc.qt-faturada[1]
               w-work.valor      = w-work.valor + it-nota-fisc.vl-tot-item.
    end.
end.

for each w-work:
    find item where item.it-codigo = w-work.it-codigo no-lock.
    put STREAM saida
        w-work.it-codigo    ";"
        item.desc-item      ";"
        w-work.cod-refer    ";"
        w-work.aliquota-icm ";"
        w-work.quantidade   ";"
        w-work.valor        ";"
        w-work.valor /
        w-work.quantidade
        SKIP.
end.

output STREAM saida close.

for each w-work:
    delete w-work.
end.

output STREAM saida to value(tt-param.arq-normal).
put STREAM saida 
           "Item;"       
           "Descricao;" 
           "Refer;"     
           "% ICM;"     
           "Quantidade;"
           "Valor;"
           "PreMedio"
           skip.

for each nota-fiscal where nota-fiscal.cod-estabel  >= tt-param.estab-ini
                       AND nota-fiscal.cod-estabel  <= tt-param.estab-fin
                       and nota-fiscal.serie        >= tt-param.serie-ini
                       AND nota-fiscal.serie        <= tt-param.serie-fin
                       and nota-fiscal.dt-emis-nota >= tt-param.dt-emis-ini
                       and nota-fiscal.dt-emis-nota <= tt-param.dt-emis-fin
                       and nota-fiscal.esp-docto    >= tt-param.espdoc-ini
                       AND nota-fiscal.esp-docto    <= tt-param.espdoc-fin
                       and nota-fiscal.dt-cancela   = ?
                       AND nota-fiscal.emite-dup    = YES
                     no-lock,
    each ped-venda where ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                     and ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
                     and ped-venda.tp-pedido  <> "W"
                     and ped-venda.tp-pedido  <> "T"
                     and ped-venda.tp-pedido  <> "P"
                   no-lock:
    
    run pi-acompanhar in h-acomp (input "Normal - Nota Fiscal: " + nota-fiscal.nr-nota-fis).

    for each it-nota-fisc of nota-fiscal no-lock,
        EACH ITEM OF it-nota-fisc
        WHERE ITEM.it-codigo >= "5"
          AND ITEM.it-codigo <= "5ZZZZZZZZZZZZZZZ"
          AND ITEM.ge-codigo >= 50
          AND ITEM.ge-codigo <= 59
        NO-LOCK,
        each natur-oper 
             where natur-oper.nat-operacao = it-nota-fisc.nat-operacao
               and natur-oper.emite-duplic = yes
             no-lock:
        
        find first w-work where w-work.it-codigo    = it-nota-fisc.it-codigo
                            AND w-work.cod-refer    = it-nota-fisc.cod-refer
                            and w-work.aliquota-icm = it-nota-fisc.aliquota-icm
                          no-lock no-error.
                          
        if not avail w-work then do:
           create w-work.
           assign w-work.it-codigo    = it-nota-fisc.it-codigo
                  w-work.cod-refer    = it-nota-fisc.cod-refer
                  w-work.aliquota-icm = it-nota-fisc.aliquota-icm
                  w-work.quantidade   = 0
                  w-work.valor        = 0.
        end.
        assign w-work.quantidade = w-work.quantidade + it-nota-fisc.qt-faturada[1]
               w-work.valor      = w-work.valor + it-nota-fisc.vl-tot-item.
    end.
end.

for each w-work:
    find item where item.it-codigo = w-work.it-codigo no-lock.
    put STREAM saida
        w-work.it-codigo    ";"
        item.desc-item      ";"
        w-work.cod-refer    ";"
        w-work.aliquota-icm ";"
        w-work.quantidade   ";"
        w-work.valor        ";"
        w-work.valor /
        w-work.quantidade
        SKIP.
end.

output STREAM saida close.

for each w-work:
    delete w-work.
end.


output STREAM saida to value(tt-param.arq-todos).
put STREAM saida 
           "Item;"       
           "Descricao;" 
           "Refer;"     
           "% ICM;"     
           "Quantidade;"
           "Valor;"
           "PreMedio"
           skip.

for each nota-fiscal where nota-fiscal.cod-estabel  >= tt-param.estab-ini
                       AND nota-fiscal.cod-estabel  <= tt-param.estab-fin
                       and nota-fiscal.serie        >= tt-param.serie-ini
                       AND nota-fiscal.serie        <= tt-param.serie-fin
                       and nota-fiscal.dt-emis-nota >= tt-param.dt-emis-ini
                       and nota-fiscal.dt-emis-nota <= tt-param.dt-emis-fin
                       and nota-fiscal.esp-docto    >= tt-param.espdoc-ini
                       AND nota-fiscal.esp-docto    <= tt-param.espdoc-fin
                       and nota-fiscal.dt-cancela   = ?
                       AND nota-fiscal.emite-dup    = YES
                     NO-LOCK:
    /* Gilvando - 07.05.08, para incluir notas sem pedido
    each ped-venda where ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                     and ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
                   no-lock:
    */
    run pi-acompanhar in h-acomp (input "Todos - Nota Fiscal: " + nota-fiscal.nr-nota-fis).

    for each it-nota-fisc of nota-fiscal no-lock,
        EACH ITEM OF it-nota-fisc
        WHERE ITEM.it-codigo >= "5"
          AND ITEM.it-codigo <= "5ZZZZZZZZZZZZZZZ"
          AND ITEM.ge-codigo >= 50
          AND ITEM.ge-codigo <= 59
        NO-LOCK,
        each natur-oper 
             where natur-oper.nat-operacao = it-nota-fisc.nat-operacao
               and natur-oper.emite-duplic = yes
             no-lock:
        
        find first w-work where w-work.it-codigo    = it-nota-fisc.it-codigo
                            AND w-work.cod-refer    = it-nota-fisc.cod-refer
                            and w-work.aliquota-icm = it-nota-fisc.aliquota-icm
                          no-lock no-error.
                          
        if not avail w-work then do:
           create w-work.
           assign w-work.it-codigo    = it-nota-fisc.it-codigo
                  w-work.cod-refer    = it-nota-fisc.cod-refer
                  w-work.aliquota-icm = it-nota-fisc.aliquota-icm
                  w-work.quantidade   = 0
                  w-work.valor        = 0.
        end.
        assign w-work.quantidade = w-work.quantidade + it-nota-fisc.qt-faturada[1]
               w-work.valor      = w-work.valor + it-nota-fisc.vl-tot-item.
    end.
end.

for each w-work:
    find item where item.it-codigo = w-work.it-codigo no-lock.
    put STREAM saida
        w-work.it-codigo    ";"
        item.desc-item      ";"
        w-work.cod-refer    ";"
        w-work.aliquota-icm ";"
        w-work.quantidade   ";"
        w-work.valor        ";"
        w-work.valor /
        w-work.quantidade
        SKIP.
end.

output STREAM saida close.

for each w-work:
    delete w-work.
end.

IF tt-param.impr-param THEN DO:
   PAGE.
   PUT "*****---------- PAR¶METROS ----------*****"
       SKIP(1).

   DISPLAY tt-param.estab-ini     tt-param.estab-fin                                           
           tt-param.serie-ini     tt-param.serie-fin    
           tt-param.espdoc-ini    tt-param.espdoc-fin   
           tt-param.dt-emis-ini   tt-param.dt-emis-fin       
           tt-param.arq-topazio
           tt-param.arq-prata  
           tt-param.arq-normal      
           tt-param.arq-todos    
           WITH FRAME f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.



