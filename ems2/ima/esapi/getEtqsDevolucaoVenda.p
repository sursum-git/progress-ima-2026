/******************************************************************************************************************
programa:esapi/getEtqsDevolucaoVenda.p
objetivo:retornar em uma tabela temporariaas etiquetas referentes
as vendas que foram devolvidas.
Uma devoluá∆o pode ser parcial ou total.
Sendo total a identificaá∆o das etiquetas deve ser total,
j† com uma devoluá∆o parcial, retorna-se as etiquetas que podem
ter sido devolvidas, mas n∆o h† como saber, a n∆o ser que a quantidade
da etiqueta seja £nica.
********************************************************************************************************************/
DEFINE INPUT  PARAMETER pCodEstabel     AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSerie          AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNota           AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNatOperacao    AS CHARACTER   NO-UNDO.

FOR EACH devol-cli NO-LOCK
    //acrescentar condiá∆o dos parametros
    ,
    EACH it-nota-fisc OF devol-cli NO-LOCK,
    EACH ped-item OF it-nota-fisc NO-LOCK,
    EACH ped-item-rom OF ped-item NO-LOCK,
    EACH ob-etiqueta OF ped-item-rom NO-LOCK
    :
    
END.





