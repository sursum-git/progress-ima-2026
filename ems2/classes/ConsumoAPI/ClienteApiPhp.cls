 
/*------------------------------------------------------------------------
   File        : clienteApiPhp
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : user
   Created     : Tue Apr 15 13:57:53 BRT 2025
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING ConsumoAPI.*.
USING Arquivo.IArquivo FROM PROPATH.
USING Arquivo.Arquivo FROM PROPATH.
USING Arquivo.EscreverArquivoChaveValor FROM PROPATH.
USING Comum.UtilJson FROM PROPATH.
USING Progress.Json.ObjectModel.JsonObject FROM PROPATH.
USING Comum.UtilTexto FROM PROPATH.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS ConsumoAPI.ClienteApiPhp IMPLEMENTS IClienteApi:
    
    {consumoAPi/IQueryString.i}
    {consumoAPi/IHeaderParam.i} 
    {comum/ttChaveValor.i}
    
    DEFINE VARIABLE cArqBody    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cArqErroPHP AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcErro      AS CHARACTER NO-UNDO. 
  
    //define variable cArquivoParamsCab as character no-undo.
    
    DEFINE PUBLIC PROPERTY cUrl AS CHARACTER NO-UNDO 
        GET.
        SET. 

    DEFINE PUBLIC PROPERTY metodo AS CHARACTER NO-UNDO 
        GET.
        SET. 


    DEFINE PRIVATE PROPERTY oColecaoQueryString AS ColecaoQueryString NO-UNDO 
        GET.
        SET. 
    
    
       

    METHOD PUBLIC VOID setColecaoQueryString(pColecao AS ColecaoQueryString, numTipoAplicacao AS ETipoAplicQueryString  ):
        
        DEFINE VARIABLE cQueryString AS CHARACTER                 NO-UNDO.
        DEFINE VARIABLE separador    AS CHARACTER                 NO-UNDO.
        DEFINE VARIABLE iCont        AS INTEGER                   NO-UNDO.
        
        DEFINE VARIABLE oArquivo     AS IArquivo.
        DEFINE VARIABLE oEscrArq     AS EscreverArquivoChaveValor.
         
        pColecao:retTT(OUTPUT table ttQueryString).
        CASE numTipoAplicacao:            
            WHEN ETipoAplicQueryString:url THEN 
                DO:
                    ASSIGN 
                        cQueryString = ''.
                    FOR EACH ttQueryString BY ttQueryString.ordem:
                        
                        ASSIGN 
                            iCont = iCont + 1.
                        IF iCont = 1 THEN
                            separador = "?".
                        ELSE
                            separador = "&".
                             
                        ASSIGN 
                            cQueryString = UtilTexto:incrValor(cQueryString,ttQueryString.chave + "=" + ttQueryString.valor, separador, NO).
                            
                         
                        
                    END.  
                    ASSIGN 
                        THIS-OBJECT:cUrl = THIS-OBJECT:cUrl + '/' + cQueryString. 
                /*message "url:" cUrl
                    view-as alert-box.*/    
                                          
                END.   
            WHEN ETipoAplicQueryString:Arquivo THEN 
                DO:
                    oArquivo = NEW Arquivo.Arquivo() .
                    oArquivo:logNovoArquivo = YES.
                    oArquivo:setNomeArquivo(THIS-OBJECT:nomeArquivoQueryString,NO).            
            
                    oEscrArq = NEW EscreverArquivoChaveValor().
                    oEscrArq:arquivo = oArquivo.     
                    
                    FOR EACH ttQueryString BY ttQueryString.ordem:
                        oEscrArq:adicionarChaveValor(ttQueryString.chave, ttQueryString.valor).
                        ASSIGN 
                            iCont = iCont + 1.            
                    END.            
                    IF iCont > 0 THEN
                        oEscrArq:criarArquivo(NO).                    
                END.   
        END CASE.
        RETURN.

    END METHOD.

    DEFINE PUBLIC PROPERTY oColecaoHeaderParam AS ColecaoHeaderParam NO-UNDO 
        GET.
        SET(arg AS  ColecaoHeaderParam ).
        
            DEFINE VARIABLE oArquivo AS IArquivo.
            DEFINE VARIABLE oEscrArq AS EscreverArquivoChaveValor.
            
            oArquivo = NEW Arquivo.Arquivo() .
            oArquivo:logNovoArquivo = YES.
            oArquivo:setNomeArquivo(THIS-OBJECT:nomeArquivoHeaderParam,NO).            
            
            oEscrArq = NEW EscreverArquivoChaveValor().
            oEscrArq:arquivo = oArquivo.    
    
            arg:retTT(OUTPUT table ttHeaderParam).
            
            FOR EACH TTHeaderParam BY TTHeaderParam.ordem:
                oEscrArq:adicionarChaveValor(ttHeaderParam.chave, TTHeaderParam.valor).                
            END.
            oEscrArq:criarArquivo(NO).
    
        END SET. 

    DEFINE PUBLIC PROPERTY oEnvio AS Progress.Lang.Object NO-UNDO 
        GET.
        SET(arg AS object):
            DEFINE VARIABLE oJson AS JsonObject NO-UNDO.
            THIS-OBJECT:oEnvio = arg.
            oJson = NEW JsonObject().            
            oJson = CAST(oEnvio,JsonObject).
            ASSIGN cArqBody = SESSION:TEMP-DIRECTORY + 'json_body_' + string(RANDOM(1,99999)) + "-" +  string(TIME) + "json".
            oJson:WriteFile(cArqBody).           
           
        END SET. 

    DEFINE PUBLIC PROPERTY oRetorno AS Progress.Lang.Object NO-UNDO 
        GET.
        SET. 

    DEFINE PUBLIC PROPERTY ssl AS LOGICAL NO-UNDO 
        GET.
        SET. 

    DEFINE PUBLIC PROPERTY tipoDadoEnvio AS ConsumoAPI.ETipoDadoAPI NO-UNDO 
        GET.
        SET. 

    DEFINE PUBLIC PROPERTY tipoDadoRetorno AS ConsumoAPI.ETipoDadoAPI NO-UNDO 
        GET.
        SET. 

    DEFINE PRIVATE PROPERTY executavelPhp AS CHARACTER NO-UNDO 
        GET.
        SET.        
        

    METHOD PUBLIC  VOID setExecutavelPhp(pArquivo AS CHARACTER):        
        
        ASSIGN 
            THIS-OBJECT:executavelPhp = pArquivo.
    
    END METHOD.
    
    METHOD PUBLIC  VOID setExecutavelPhp(pArquivo AS Arquivo):
        
        ASSIGN 
            THIS-OBJECT:executavelPhp = pArquivo:nomeArquivo.         
    
    END METHOD.
    
    
    DEFINE PRIVATE PROPERTY programaClientePHP AS CHARACTER NO-UNDO 
        GET.
        SET. 
        
    METHOD PUBLIC  VOID setProgramaClientePhp(pArquivo AS CHARACTER):        
        
        ASSIGN 
            THIS-OBJECT:programaClientePHP = pArquivo.
    
    END METHOD.
    
    METHOD PUBLIC  VOID setProgramaClientePhp(pArquivo AS Arquivo):
        
        ASSIGN 
            THIS-OBJECT:programaClientePHP = pArquivo:nomeArquivo.         
    
    END METHOD.     
        

    DEFINE PUBLIC PROPERTY nomeArquivoResultado AS CHARACTER NO-UNDO 
        GET.
        SET. 

    DEFINE PUBLIC PROPERTY nomeArquivoQueryString AS CHARACTER NO-UNDO 
        GET.
        SET. 
    
    DEFINE PUBLIC PROPERTY nomeArquivoHeaderParam AS CHARACTER NO-UNDO 
        GET.
        SET.
  
     

    METHOD PUBLIC CHARACTER executar(  ):
        
        /********************************************************************************************************************************
            chamar o programa PHP e passar os parametros
            nome programa php: php/aplicacoes/consumirAPI.php
            Parametros:
                *<arquivo_retorno> qual o nome do arquivo que retornará os dados da requisição
                            *<url> url completa da api que será chamada
                         *<metodo> metodo utilizado para chamar a API. 
                 *<arquivo_config> arquivo no formato chave=valor onde estarão as variaveis de header a serem aplicadas a requisição 
                [arquivo_json] nome do arquivo json a ser enviado no corpo da requisição
                     
                *OBRIGATORIOS
        *********************************************************************************************************************************/
        DEFINE VARIABLE comando AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lcCmd   AS CHARACTER NO-UNDO.
        
        
        THIS-OBJECT:_buscarConfiguracoesIni().
        FIND FIRST ttChaveValor
            WHERE ttChaveValor.chave = 'arquivo_erro_requisicao' NO-ERROR.
        IF AVAILABLE ttChaveValor THEN
            ASSIGN cArqErroPhp = ttChaveValor.valor.    
        
        
        ASSIGN 
            comando = THIS-OBJECT:executavelPhp
            +  ' ' +  THIS-OBJECT:programaClientePHP 
            +  ' ' +  THIS-OBJECT:nomeArquivoResultado 
            +  ' ' + '"' + cUrl + '"'  
            +  ' ' + THIS-OBJECT:metodo
            +  ' ' + THIS-OBJECT:nomeArquivoHeaderParam 
            +  ' ' + IF THIS-OBJECT:metodo <> 'GET' THEN cArqBody ELSE ''.
        /*MESSAGE 'metodo:' THIS-OBJECT:metodo SKIP 
                'arquivo body:' cArqBody
            VIEW-AS ALERT-BOX.*/
        
        //grava em arquivo o comando para facilitar simular problemas no ambiente do client
        ASSIGN 
            lcCmd = comando.
        UtilTexto:convLongChar2Arq(lcCmd,SESSION:TEMP-DIRECTORY + 'cmd-cliApiPhp.txt').
        
        OS-DELETE value(cArqErroPhp).
        OS-COMMAND SILENT VALUE(comando) NO-WAIT NO-ERROR.
        
        //se achar é porque ocorreu um erro e a requisição não pode ser concluída
        IF SEARCH(cArqErroPhp) <> ? THEN 
        DO:
            UtilTexto:convArq2LongChar(lcErro,cArqErroPhp).            
        END.
                 
        
        
        
            
        _atribuirObjRetorno().
        RETURN comando.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PRIVATE VOID _atribuirObjRetorno(  ):
        
        DEFINE VARIABLE oJsonObject AS JsonObject. 
        
        CASE THIS-OBJECT:tipoDadoRetorno:
            
            WHEN ETipoDadoAPI:JSON THEN  
                DO:
                    IF SEARCH(nomeArquivoResultado) <> ? THEN 
                    DO:
                        oJsonObject = UtilJson:convArqJson2ObjJson(nomeArquivoResultado).
                        IF NOT VALID-OBJECT(oJsonObject) THEN 
                            UNDO, THROW NEW Progress.Lang.AppError('A conversão do arquivo não gerou um objeto Json Válido').     
                    END.
                    
                    ELSE
                        UNDO, THROW NEW Progress.Lang.AppError('Arquivo de Resultado não Gerado',0).      
                       
                    IF VALID-OBJECT(oJsonObject) THEN
                        oRetorno = oJsonObject.
                END.
            OTHERWISE 
            DO:
                UNDO, THROW NEW Progress.Lang.AppError('Tipo de Retorno não tratado', 1).
            END.    
                
        END CASE.       
            
        
        RETURN.

    END METHOD.
    
    METHOD PRIVATE VOID _buscarConfiguracoesIni():
    
        DEFINE VARIABLE cArquivoConfig AS CHARACTER NO-UNDO.
        DEFINE VARIABLE oArq           AS Arquivo   NO-UNDO.
    
        oArq = NEW Arquivo().
        oArq:setNomeArquivo('php/aplicacoes/config.ini', YES).
        cArquivoConfig = oArq:nomeArquivo .
    
        INPUT from value(cArquivoConfig) .
    
        REPEAT:        
            CREATE ttChaveValor.
            IMPORT DELIMITER "=" ttChaveValor. 
        END.    
    
        INPUT close.       
        
    
    
    END METHOD.    
  

    METHOD PUBLIC LONGCHAR getErroExecAPI():        

        RETURN  lcErro .

    END METHOD.
    
    
    

END CLASS.