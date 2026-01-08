
/*------------------------------------------------------------------------
    File        : IClienteApi
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : user
    Created     : Tue Apr 15 08:48:57 BRT 2025
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using ConsumoAPI.ETipoDadoAPI from propath.
using consumoAPi.ColecaoQueryString from propath.
using ConsumoAPI.ETipoAplicQueryString from propath.

interface consumoAPi.IClienteApi:  
    
    
  
    method public void setColecaoQueryString(pColecao as ColecaoQueryString, numTipoAplicacao as ETipoAplicQueryString  ).
    
    
    define public property oColecaoHeaderParam as consumoAPi.ColecaoHeaderParam no-undo 
    get.
    set. 

    define public property oEnvio   as Object no-undo 
    get.
    set.
    
    define public property oRetorno as Object no-undo 
    get.
    set. 

    define public property tipoDadoRetorno as ETipoDadoAPI no-undo 
    get.
    set. 

    define public property tipoDadoEnvio as ETipoDadoAPI no-undo 
    get.
    set. 

    define public property ssl as logical no-undo 
    get.
    set. 

    define public property metodo as character no-undo 
    get.
    set. 

    define public property cUrl as character no-undo 
    get.
    set. 

    

    /*------------------------------------------------------------------------------
     Purpose:Executar a Requisição
     Notes:
    ------------------------------------------------------------------------------*/

    method public character executar(  ).
    
    
    
    
       
   
end interface.