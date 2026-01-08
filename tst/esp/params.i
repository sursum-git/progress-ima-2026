/*****************************************************************************************
programa:params.i
objetivo:Facilitar a busca de parametros sem que precise 
         sempre ficar chamando a boConsParam
*******************************************************************************************/


FUNCTION getDirDanfe RETURNS CHAR (pEstab AS CHAR):

    DEFINE VARIABLE cNomeParam AS CHARACTER   NO-UNDO.
    ASSIGN cNomeParam = 'dir_danfe_' + pEstab.
    {esp/params.i2  cNomeParam "''"}
    


END FUNCTION.

FUNCTION getDirXML RETURNS CHAR(pEstab AS CHAR):

    DEFINE VARIABLE cNomeParam AS CHARACTER   NO-UNDO.
    ASSIGN cNomeParam = 'pasta_xml_' + pEstab.
    {esp/params.i2  cNomeParam "''"}


END FUNCTION.

FUNCTION getTituloEmailNFVenda RETURNS CHAR():

    DEFINE VARIABLE cNomeParam AS CHARACTER   NO-UNDO.
    ASSIGN cNomeParam = 'titulo_email_envio_nf_venda' .
    {esp/params.i2  cNomeParam "''"}


END FUNCTION.

FUNCTION getEndEmailLogNfVenda RETURNS CHAR():

    DEFINE VARIABLE cNomeParam AS CHARACTER   NO-UNDO.
    ASSIGN cNomeParam = 'end_email_log_nf_venda' .
    {esp/params.i2  cNomeParam "'log@imatextil.com.br'"}


END FUNCTION.


FUNCTION  getEstabsIntegraLISA RETURNS CHAR():

    DEFINE VARIABLE cNomeParam AS CHARACTER   NO-UNDO.
    ASSIGN cNomeParam = 'estabs_integra_lisa'.
    {esp/params.i2  cNomeParam "'505'"}
    
 END FUNCTION.

 
 
 
FUNCTION  getCodigoProdutoUnificadoLisa RETURNS CHAR():

    DEFINE VARIABLE cNomeParam AS CHARACTER   NO-UNDO.
    ASSIGN cNomeParam = 'codigo_produto_unificado'.
    {esp/params.i2  cNomeParam "'0'"}
    
 END FUNCTION.
 
 
FUNCTION getEmailErroIntegrPedPortal RETURNS CHAR():    
   
   DEFINE VARIABLE cNomeParam AS CHARACTER   NO-UNDO.
    ASSIGN cNomeParam = 'emails_erro_integracao_pedido_portal'.
    {esp/params.i2  cNomeParam "'ti@imatextil.com.br'"}

END FUNCTION.


