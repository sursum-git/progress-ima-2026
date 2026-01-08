{include/i_dbvers.i}
/********************************************************************************************
**   Include...: ftapi400.i                                                                **
**                                                                                         **
**   Versao....: 2.00.00.000 - jun/2000 - Carlos Alberto Soares Pereira                    **
**                                                                                         **
**   Objetivo..: Temp-table itens do pre-faturamento                                       **
**                                                                                         **
********************************************************************************************/

Define temp-table tt-integracao-400 no-undo
       field cdd-embarq       as dec format ">>>>>>>>>>>>>>>9"
       field nr-resumo-ini    as integer
       field nr-resumo-fim    as integer
       field coletor-ini      as char
       field coletor-fim      as char
       field ler-it-dep-fat   as log 
       field opcao-inteiro-1  as int
       field opcao-inteiro-2  as int
       field opcao-caracter-1 as char
       field opcao-caracter-2 as char
       field cod-versao-integracao as integer.

define temp-table tt-it-dep-fat no-undo
&IF "{&mguni_version}" >= "2.071" &THEN
field cod-estabel  as                   char    format "x(05)"
&ELSE
field cod-estabel  as                   char    format "X(3)"
&ENDIF
field nr-sequencia as                   inte    format ">>,>>9"
field cod-depos    as                   char    format "x(3)"
field nome-abrev   as                   char    format "x(12)"
field nr-pedcli    as                   char    format "x(12)"
field qt-alocada   as                   deci    format ">>>>,>>9.9999"
field nr-serlote   as                   char    format "x(10)"
field it-codigo    as                   char    format "x(16)"
field cdd-embarq   as                   dec     format ">>>>>>>>>>>>>>>9"
field qt-faturada  as                   deci    format ">>,>>>,>>>,>>9.99"
field un-familia   as                   char    format "x(2)"
field nr-entrega   as                   inte    format ">>>>9"
field nr-resumo    as                   inte    format ">>>>,>>9"
field utiliza-op-aloca as               logi    format "Sim/NÆo"
field tp-aloca         as               inte    format "99"
field qt-log-utiliza   as               deci    format ">>>>,>>9.9999"
field sd-aloca-op      as               deci    format ">>>>,>>9.9999"
field qt-tot-aloca-op  as               deci    format ">>>>,>>9.9999"
field cod-localiz      as               char    format "x(10)"
field char-1           as               char    format "x(100)"
field char-2           as               char    format "x(100)"
field dec-1            as               deci    format "->>>>>>>>>>>9.99999"
field dec-2            as               deci    format "->>>>>>>>>>>9.99999"
field int-1            as               inte    format "->>>>>>>>>9"
field int-2            as               inte    format "->>>>>>>>>9"
field log-1            as               logi    format "Sim/NÆo"
field log-2            as               logi    format "Sim/NÆo"
field data-1           as               date    format "99/99/9999"
field data-2           as               date    format "99/99/9999"
index id is primary cdd-embarq
                    nr-resumo
                    nome-abrev
                    nr-pedcli
                    cod-estabel
                    nr-sequencia
                    it-codigo
                    nr-entrega
                    cod-depos
                    cod-localiz
                    nr-serlote.

Define Temp-Table tt-it-pre-fat No-undo
field aliquota-ipi as decimal format ">>9.99"
field aliquota-tax as decimal format ">>9.99"
field baixa-estoq as logical format "Sim/NÆo"
field cd-referencia as character format "x(4)"
field char-1 as character format "x(100)"
field char-2 as character format "x(100)"
field check-sum as character format "x(20)"
field class-fiscal as character format "9999.99.99"
field cod-tax as integer format ">>9"
field cod-vat as integer format ">>9"
field ct-cuscon as character format "x(17)"
field data-1 as date format "99/99/9999"
field data-2 as date format "99/99/9999"
field dec-1 as decimal format "->>>>>>>>>>>9.99999999"
field dec-2 as decimal format "->>>>>>>>>>>9.99999999"
field dt-entrega as date format "99/99/9999"
field dt-prev-fat as date format "99/99/9999"
field int-1 as integer format "->>>>>>>>>9"
field int-2 as integer format "->>>>>>>>>9"
field it-codigo as character format "x(16)"
field log-1 as logical format "Sim/NÆo"
field log-2 as logical format "Sim/NÆo"
field narrativa as character format "x(2000)"
field nat-operacao as character format "9.99-XXX"
field nome-abrev as character format "x(12)"
field cdd-embarq as decimal format ">>>>>>>>>>>>>>>9"
field nr-entrega as integer format ">>>>9"
field nr-pedcli as character format "x(12)"
field nr-resumo as integer format ">>>>,>>9"
field nr-sequencia as integer format ">>,>>9"
field qt-alocada as decimal format ">>>>,>>9.9999"
field qt-faturada as decimal format ">>>>,>>9.9999"
field qt-rejeita as decimal format ">>>>,>>9.9999"
field qt-transfer as decimal format ">>,>>>,>>9.9999"
field sc-cuscon as character format "x(8)"
field tipo-atend as integer format "99"
field un as character format "xx"
field user-rej as character format "x(12)"
field vl-cuscontab as decimal format ">,>>>,>>>,>>9.99"
field cod-refer as char format "x(30)"
index id is primary cdd-embarq
                    nr-resumo
                    nome-abrev
                    nr-pedcli
                    nr-sequencia
                    it-codigo
                    nr-entrega.
    
