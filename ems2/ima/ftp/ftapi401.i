{include/i_dbvers.i}
/********************************************************************************************
**   Include...: ftapi401.i                                                                **
**                                                                                         **
**   Versao....: 2.00.00.000 - jun/2000 - Carlos Alberto Soares Pereira                    **
**                                                                                         **
**   Objetivo..: Temp-table API pre-faturamento                                            **
**                                                                                         **
********************************************************************************************/

Define Temp-Table tt-pre-fatur No-undo 
field bairro as character format "x(30)"
field caixa-postal as character format "x(10)"
field cep as character format "x(12)"
field cgc as character format "x(19)"
field char-1 as character format "x(100)"
field char-2 as character format "x(100)"
field check-sum as character format "x(20)"
field cidade as character format "x(25)"
field cidade-cif as character format "x(25)"
field cod-cond-pag as integer format ">>9"
field cod-dep-ext as character format "x(3)"
field cod-entrega as character format "x(12)"
&IF "{&mguni_version}" >= "2.071" &THEN
field cod-estabel as character format "x(05)"
&ELSE
field cod-estabel as character format "x(3)"
&ENDIF
field cod-priori as integer format "99"
field cod-rota as character format "x(12)"
field cod-sit-pre as integer format "99"
field cod-tax as integer format ">>9"
field cod-tipo as character format "x(8)"
field cond-redespa as character format "x(2000)"
field data-1 as date format "99/99/9999"
field data-2 as date format "99/99/9999"
field data-pick as date format "99/99/9999"
field dec-1 as decimal format "->>>>>>>>>>>9.99999999"
field dec-2 as decimal format "->>>>>>>>>>>9.99999999"
field dt-embarque as date format "99/99/9999"
field endereco as character format "x(40)"
field estado as character format "x(04)"
field hora-pick as character format "x(8)"
field identific as character format "x(12)"
field ind-possui-ordens as logical format "Sim/NÆo"
field ind-sit-embarque as integer format "99"
field ins-estadual as character format "x(19)"
field int-1 as integer format "->>>>>>>>>9"
field int-2 as integer format "->>>>>>>>>9"
field log-1 as logical format "Sim/NÆo"
field log-2 as logical format "Sim/NÆo"
field marca-volume as character format "x(20)"
field mo-codigo as integer format ">9"
field motorista as character format "x(25)"
field nat-operacao as character format "9.99-XXX"
field no-ab-reppri as character format "x(12)"
field nome-abrev as character format "x(12)"
field nome-tr-red as character format "x(12)"
field nome-transp as character format "x(12)"
field cdd-embarq as decimal format ">>>>>>>>>>>>>>>9"
field nr-pedcli as character format "x(12)"
field nr-resumo as integer format ">>>>,>>9"
field observacoes as character format "x(2000)"
field pais as character format "x(20)"
field pick-impresso as logical format "Sim/NÆo"
field placa as character format "x(10)"
field uf-placa as character format "!!"
field user-pick as character format "x(12)"
field usuario as character format "x(12)"
index id is primary cdd-embarq nr-resumo dt-embarque nat-operacao nome-transp.
    
Define temp-table tt-integracao-401 no-undo
       field cdd-embarq-ini    as decimal format ">>>>>>>>>>>>>>>9"
       field cdd-embarq-fim    as decimal format ">>>>>>>>>>>>>>>9"
       field nr-resumo-ini     as integer
       field nr-resumo-fim     as integer
       field coletor-ini       as char
       field coletor-fim       as char
       field dt-embarque-ini   as date
       field dt-embarque-fim   as date
       field opcao-inteiro-1  as int
       field opcao-inteiro-2  as int
       field opcao-caracter-1 as char
       field opcao-caracter-2 as char
       field cod-versao-integracao as integer.
