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

//03/12/2025
FUNCTION getArmazemLisa RETURNS CHAR():    
   
   DEFINE VARIABLE cNomeParam AS CHARACTER   NO-UNDO.
    ASSIGN cNomeParam = 'cod_armazem_lisa'.
    {esp/params.i2  cNomeParam "'01'"}

END FUNCTION.

//04/12/2025
FUNCTION getDirERPWindows RETURNS CHAR():    
   
   DEFINE VARIABLE cNomeParam AS CHARACTER   NO-UNDO.
    ASSIGN cNomeParam = 'dir_erp_windows'.
    {esp/params.i2  cNomeParam "'\\192.168.0.137\ERP'"}

END FUNCTION.


FUNCTION getDirERPLinux RETURNS CHAR():    
   
   DEFINE VARIABLE cNomeParam AS CHARACTER   NO-UNDO.
    ASSIGN cNomeParam = 'dir_erp_linux'.
    {esp/params.i2  cNomeParam "'/mnt/datasul/ERP'"}

END FUNCTION.

FUNCTION getDirERP RETURNS CHAR():    
   
   IF OPSYS <> 'UNIX' THEN
   DO:
       RETURN getDirERPWindows().
       
   END.
   ELSE DO:
       RETURN getDirERPLinux().
   END.

END FUNCTION.


//06/01/2026
FUNCTION getUrlLisa RETURNS CHAR():

     DEFINE VARIABLE cNomeParam AS CHARACTER   NO-UNDO.
     ASSIGN cNomeParam = 'url_lisa'.
    {esp/params.i2  cNomeParam "'li195954.protheus.cloudtotvs.com.br'"}


END FUNCTION.
