/********************************************************************************************
**   Include...: bc9002.i                                                                  **
**                                                                                         **
**   Versao....: 2.00.00.000 - jun/2000 - Carlos Alberto Soares Pereira                    **
**               2.00.00.001 - jun/2000 - John Cleber Jaraceski                            **
**                                                                                         **
**   Objetivo..: Armazenar os dados para transacao de Contagem Ciclica                     **
**                                                                                         **
********************************************************************************************/

Define {1} Temp-Table tt-contagem-bc NO-UNDO
    Field cod-estabel               As Character    Format 'x(03)':U                    Label "Estabelec"
    Field num-ficha-inventario      As Integer      Format '>>>>>>>9':U                 Label "Nro Ficha"
    Field num-contagem              As Integer      Format '>>>>>>>9':U                 Label "Nro Contagem"
    Field data-transacao            As Date         Format '99/99/9999':U               Label "Data Trans"
    Field dat-contagem              As Date         Format '99/99/9999':U               Label "Data Inven"
    Field cod-item                  As Character    Format 'x(16)':U                    Label "Item"
    Field cod-unico                 As Character    Format 'x(16)':U                    Label "Codig Unico"
    Field cod-refer                 As Character    Format 'x(10)':U                    Label "Refer"
    Field cod-depos                 As Character    Format 'x(03)':U                    Label "Depos"
    Field cod-local                 As Character    Format 'x(10)':U                    Label "Local"
    Field cod-lote                  As Character    Format 'x(10)':U                    Label "Lote"
    Field dat-valid-lote            As Date         Format '99/99/9999':U               Label "Vl Lote"
    Field cod-usuario               As Character    Format 'x(12)':U                    Label "Usuario"
    Field qtd-apontada              As Decimal      Format '>>>>>>9.9999':U Decimals 4  Label "Quantidade"
    Field valor-mat-m               As Decimal      Format '>>>>>>9.9999':U Decimals 4  Label "Valor Mat-M"
    Field valor-mob-m               As Decimal      Format '>>>>>>9.9999':U Decimals 4  Label "Valor Mob-M"
    /*****[ Inicio Campos Utilizados somente para Produto EMS ] *****/
    Field valor-mat-o               As Decimal      Format '>>>>>>9.9999':U Decimals 4  Label "Valor Mat-O"
    Field valor-mat-p               As Decimal      Format '>>>>>>9.9999':U Decimals 4  Label "Valor Mat-P"
    Field valor-mob-o               As Decimal      Format '>>>>>>9.9999':U Decimals 4  Label "Valor Mob-O"
    Field valor-mob-p               As Decimal      Format '>>>>>>9.9999':U Decimals 4  Label "Valor Mob-P"
    Field valor-ggf-m               As Decimal      Format '>>>>>>9.9999':U Decimals 4  Label "Valor Mob-M"
    Field valor-ggf-o               As Decimal      Format '>>>>>>9.9999':U Decimals 4  Label "Valor Mob-O"
    Field valor-ggf-p               As Decimal      Format '>>>>>>9.9999':U Decimals 4  Label "Valor Mob-P"
    /*****[ Fim Campos Utilizados somente para Produto EMS ] *****/
    Field cod-livre-1               As Character    Format 'x(40)':U                    Label "Cod Livre 1"
    Field cod-livre-2               As Character    Format 'x(40)':U                    Label "Cod Livre 2"
    Field cod-livre-3               As Character    Format 'x(40)':U                    Label "Cod Livre 3"
    Field num-livre-1               As Integer      Format '>>>>>>>9':U                 Label "Num Livre 1"     
    Field num-livre-2               As Integer      Format '>>>>>>>9':U                 Label "Num Livre 2"     
    Field num-livre-3               As Integer      Format '>>>>>>>9':U                 Label "Num Livre 3"
    Field dec-livre-1               As Decimal      Format '>>>>>>9.9999':U Decimals 4   Label "Dec Livre 1"
    Field dec-livre-2               As Decimal      Format '>>>>>>9.9999':U Decimals 4   Label "Dec Livre 2"
    Field dec-livre-3               As Decimal      Format '>>>>>>9.9999':U Decimals 4   Label "Dec Livre 3"
    Field log-livre-1               As Logical      Format 'Sim/Nao'                    Label "Log Livre 1"
    Field log-livre-2               As Logical      Format 'Sim/Nao'                    Label "Log Livre 2"
    Field log-livre-3               As Logical      Format 'Sim/Nao'                    Label "Log Livre 3"
    Field dat-livre-1               As Date         Format '99/99/9999'                 Label 'DataLivre 1'
    Field dat-livre-2               As Date         Format '99/99/9999'                 Label 'DataLivre 2'
    Field dat-livre-3               As Date         Format '99/99/9999'                 Label 'DataLivre 3'
    INDEX Item IS PRIMARY Cod-Item
    .

/***********************************************************************************/

/*****[ Documenta‡Æo dos Campos ]**********************************************
/*************[ INICIO Campos Utilizados no Ems e no Magnus I00 ]*************/
Campos:
    1.) cod-estabel...........: C¢digo do Estabelecimento
    2.) num-ficha-inventario..: Numero Ficha de Inventario
    3.) dat-contagem..........: Data da Contagem
    4.) cod-item..............: C¢digo do Item
    5.) cod-refer.............: C¢digo Referencia
    6.) cod-depos.............: C¢digo Dep¢sito
    7.) cod-local.............: C¢digo Localiza‡Æo
    8.) cod-lote..............: Numero Lote
    9.) dat-valid-lote........: Data Validade Lote
   10.) qtd-apontada..........: Quantidade Contada
   11.) valor-mat-m...........: Valor Materiais
   12.) valor-mob-m...........: Valor MÆo-De-obra

/*************[ FINAL  Campos Utilizados no Ems e no Magnus I00 ]*************/

/*****************[ INICIO Campos Utilizados somente no Ems ]****************/
/****************************************************************************/
Campos:
    1.) data-transacao........: Data Transa‡Æo
    2.) valor-mat-o...........: Valor Materiais
    3.) valor-mat-p...........: Valor Materiais
    4.) valor-mob-o...........: Valor MÆo-De-Obra 
    5.) valor-mob-p...........: Valor MÆo-De-Obra 
    6.) valor-ggf-m...........: Valor GGF
    7.) valor-ggf-o...........: Valor GGF
    8.) valor-ggf-p...........: Valor GGF

/*****************[ FINAL  Campos Utilizados somente no Ems ]****************/

/*************[ INICIO Campos Utilizados somente no Magnus I00 ]*************/
/****************************************************************************/
Campos:
    1.) num-contagem..........: Numero da Contagem
    2.) cod-usuario...........: C¢digo do usuario

/*************[ FINAL  Campos Utilizados somente no Magnus I00 ]*************/

/*******************[ INICIO Campos de Uso Interno Datasul ]*****************/
/*******************[      NÆo Devem Ser Alimentados       ]*****************/ 
/****************************************************************************/
Campos:
*******************************************************************************/
