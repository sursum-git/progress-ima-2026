/********************************************************************************************
**   Include...: bc9008.i                                                                  **
**                                                                                         **
**   Versao....: 2.00.00.000 - jun/2000 - Carlos Alberto Soares Pereira                    **
**                                                                                         **
**   Objetivo..: Armazenar os dados para transacao de aviso de embarque                    **
**                                                                                         **
********************************************************************************************/

Define {1} Temp-Table tt-embarque-bc NO-UNDO
    Field num-aviso-embarque        As Integer      Format '>>>>>>>9':U                 Label "Nr Embarque"
    Field nr-resumo                 As Integer      Format '>>>>>>>9':U                 Label "Nr Resumo"    
    Field data-transacao            As Date         Format '99/99/9999':U               Label "Data Trans"
    Field cod-item                  As Character    Format 'x(16)':U                    Label "Item"
    Field cod-unico                 As Character    Format 'x(16)':U                    Label "Codigo Unico"
    Field cod-refer                 As Character    Format 'x(10)':U                    Label "Referencia"
    Field cod-depos                 As Character    Format 'x(03)':U                    Label "Deposito"
    Field cod-local                 As Character    Format 'x(10)':U                    Label "Localizacaog"
    Field cod-lote                  As Character    Format 'x(10)':U                    Label "Lote"
    Field dat-valid-lote            As Date         Format '99/99/9999':U               Label "Valid Lote"
    Field cod-usuario               As Character    Format 'x(12)':U                    Label "Usuario"
    Field qtd-embarcada             As Decimal      Format '>>>>>>9.9999':U Decimals 4   Label "Quantidade"
    Field chave-unica               as Character    format 'x(200)'                     Label "Chave Unica"
    Field cod-livre-1               As Character    Format 'x(40)':U                    Label "Cod Livre 1"
    Field cod-livre-2               As Character    Format 'x(40)':U                    Label "Cod Livre 2"
    Field cod-livre-3               As Character    Format 'x(40)':U                    Label "Cod Livre 3"
    Field num-livre-1               As Integer      Format '>>>>>>>9':U                 Label "Num Livre 1"     
    Field num-livre-2               As Integer      Format '>>>>>>>9':U                 Label "Num Livre 2"     
    Field num-livre-3               As Integer      Format '>>>>>>>9':U                 Label "Num Livre 3"
    Field dec-livre-1               As Decimal      Format '>>>>>>9.9999':U Decimals 4   Label "Dec Livre 1"
    Field dec-livre-2               As Decimal      Format '>>>>>>9.9999':U Decimals 4   Label "Dec Livre 2"
    Field dec-livre-3               As Decimal      Format '>>>>>>9.9999':U Decimals 4   Label "Dec Livre 3"
    Field log-livre-1               As Logical      Format 'Sim/NÆo'                    Label "Log Livre 1"
    Field log-livre-2               As Logical      Format 'Sim/NÆo'                    Label "Log Livre 2"
    Field log-livre-3               As Logical      Format 'Sim/NÆo'                    Label "Log Livre 3"
    Field dat-livre-1               As Date         Format '99/99/9999'                 Label 'DataLivre 1'
    Field dat-livre-2               As Date         Format '99/99/9999'                 Label 'DataLivre 2'
    Field dat-livre-3               As Date         Format '99/99/9999'                 Label 'DataLivre 3'
    INDEX Item IS PRIMARY Cod-Item
    .
    
/***********************************************************************************/


/***********************************************************************************/

/*****[ Documenta‡Æo dos Campos ]**********************************************
/*************[ INICIO Campos Utilizados no Ems e no Magnus I00 ]*************/
Campos:
    1.) num-aviso-embarque....: Numero Embarque
    2.) nr-resumo.............: Numero do resumo 
    3.) data-transacao........: Data da Transa‡Æo
    4.) cod-item..............: C¢digo do Item
    5.) cod-unico.............: C¢digo Unico
    6.) cod-refer.............: C¢digo Referencia
    7.) cod-depos.............: C¢digo Dep¢sito
    8.) cod-local.............: C¢digo Localiza‡Æo
    9.) cod-lote..............: Numero do Lote
   10.) dat-valid-lote........: Data Validade Lote
   11.) cod-usuario...........: C¢digo do Usuario
   12.) qtd-embarcada.........: Quantidade Embaracada

/*************[ FINAL  Campos Utilizados no Ems e no Magnus I00 ]*************/

/*****************[ INICIO Campos Utilizados somente no Ems ]****************/
/****************************************************************************/
Campos:
/*****************[ FINAL  Campos Utilizados somente no Ems ]****************/

/*************[ INICIO Campos Utilizados somente no Magnus I00 ]*************/
/****************************************************************************/
Campos:
/*************[ FINAL  Campos Utilizados somente no Magnus I00 ]*************/

/*******************[ INICIO Campos de Uso Interno Datasul ]*****************/
/*******************[      NÆo Devem Ser Alimentados       ]*****************/ 
/****************************************************************************/
Campos:
*******************************************************************************/

