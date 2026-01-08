 
 /*------------------------------------------------------------------------
    File        : ColecaoQueryString
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : user
    Created     : Tue Apr 15 09:27:43 BRT 2025
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using consumoAPI.IColecaoQueryString from propath.
using consumoAPi.IQueryString from propath.
using consumoAPI.IColecaoHeaderParam from propath.
using consumoAPi.IHeaderParam from propath.

routine-level on error undo, throw.

class consumoAPi.ColecaoHeaderParam implements IColecaoHeaderParam: 

     //definição tabela temporaria
    {consumoAPi/IHeaderParam.i }
    
    define variable oHeaderParam as IHeaderParam.
    define public property obj   as Progress.Lang.Object no-undo 
    get.
    set.      

     method public void adicionarObj(arg as Object  ).
     
        this-object:obj = arg.
        oHeaderParam     = cast(this-object:obj,IHeaderParam).
     
     end method.
    method public void _criarTT(  ):
        
        create ttHeaderParam.
        assign ttHeaderParam.ordem = oHeaderParam:ordem
               ttHeaderParam.chave = oHeaderParam:chave
               TTHeaderParam.valor = oHeaderParam:valor                
               .
        return.

    end method.

  
    
    //retorno da tabela temporaria
    {comum/retTT.i ttHeaderParam}
    

end class.