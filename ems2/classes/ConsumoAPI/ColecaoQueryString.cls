 
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
using consumoAPi.QueryString from propath.

routine-level on error undo, throw.

class consumoAPi.ColecaoQueryString implements IColecaoQueryString: 

     //definição tabela temporaria
    {consumoAPi/IQueryString.i}
    
    define variable oQueryString as QueryString.

        

    define public property obj   as Progress.Lang.Object no-undo 
    get.
    private set.
    
    method public void adicionarObj(arg as Object  ):
    
        
        this-object:obj = arg.
        oQueryString = cast(this-object:obj,QueryString).
        this-object:_criarTT().
        delete object this-object:obj.
        
    end method.  
   
   
   
   
   
    

    method public void _criarTT(  ):
        
        create ttQueryString.
        assign ttQueryString.ordem = oQueryString:ordem
               ttQueryString.chave = oQueryString:chave
               TTQueryString.valor = oQueryString:valor 
               .
        return.

    end method.

  
    
    //retorno da tabela temporaria
    {comum/retTT.i ttQueryString}
    

end class.