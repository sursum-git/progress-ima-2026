
/*------------------------------------------------------------------------
    File        : IColecaoQueryString
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : user
    Created     : Tue Apr 15 09:14:48 BRT 2025
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Comum.IColecao.
using consumoAPi.IHeaderParam from propath.

interface consumoAPI.IColecaoHeaderParam inherits IColecao:  
    
    //definição tabela temporaria
    {consumoAPi/IHeaderParam.i}
    
    
    
    
    //retorno da tabela temporaria
    {comum/IretTT.i ttHeaderParam}
    
  
end interface.