 
 /*------------------------------------------------------------------------
    File        : ItemRegraFrete
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : administrator
    Created     : Thu Apr 10 17:32:30 BRT 2025
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Persistencia.IPersistencia.
using RegraNegocio.ItemRegraNegocio.IItemRegraNegocioPrioridade.

block-level on error undo, throw. 

class RegraNegocio.Frete.ItemRegraFrete implements IPersistencia, IItemRegraNegocioPrioridade: 
    

    {ttResultado.i}    

    define public property dtHrFimValid as datetime no-undo 
    get.
    set. 

    define public property dtHrIniValid as datetime no-undo 
    get.
    set. 

    define public property idItemRegraNegocio as int64 no-undo 
    get.
    set. 

    define public property idRegraNegocio as int64 no-undo 
    get.
    set. 

    define public property logPrioridadeManual as logical no-undo 
    get.
    set. 

    define public property numPrioridade as int64 no-undo 
    get.
    set. 

    define public property oRegistro as Progress.Lang.Object no-undo 
    get.
    set. 
    
    {retTT.i ttResultado}

    method public int64 encontrar(  ):
        
        undo, throw new Progress.Lang.AppError("METHOD NOT IMPLEMENTED").

    end method.

    method public logical excluir(  ):
        
        undo, throw new Progress.Lang.AppError("METHOD NOT IMPLEMENTED").

    end method.

    method public int64 salvar(  ):
        
        undo, throw new Progress.Lang.AppError("METHOD NOT IMPLEMENTED").

    end method.

end class.