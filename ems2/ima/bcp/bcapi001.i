/*****************************************/
/*                                       */
/*    DECODE - Descriptografa fontes.    */
/*                                       */
/*****************************************/
/**************************************************************************
**
**   Include: BCAPI001.I - Definicao da temp-table de transacoes de coleta
**                         de dados
**
***************************************************************************/

def temp-table tt-trans no-undo
    field cod-versao-integracao as int
    field i-sequen              as int
    field cd-trans              as char format "x(8)"
    field conteudo-trans        as raw
    field detalhe               as char format "x(60)"
    field nr-trans              as deci format "zzzzzzzzz9"
    field usuario               as char format "x(12)"
    field atualizada            as logical
    field etiqueta              as logical.  /*  No - Gera Transa‡Æo de Movimento
                                                 Yes - Gera Transa‡Æo de Etiqueta*/
