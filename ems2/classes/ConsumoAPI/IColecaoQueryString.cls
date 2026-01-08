
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
using consumoAPi.IQueryString from propath.

interface consumoAPI.IColecaoQueryString inherits IColecao :  
    
    //definição tabela temporaria
    {consumoAPi/IQueryString.i }
    
    
    //retorno da tabela temporaria
    {comum/IretTT.i ttQueryString}
    
  
end interface.