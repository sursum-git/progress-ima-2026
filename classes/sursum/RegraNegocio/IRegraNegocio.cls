
/*------------------------------------------------------------------------
    File        : IRegraNegocio
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : administrator
    Created     : Thu Apr 10 12:20:34 BRT 2025
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using log.*.

interface RegraNegocio.IRegraNegocio:  
    
    
  
    define public property descricaoRegraNegocio as character no-undo 
    get.
    set. 

    define public property nomeRegraNegocio as character no-undo 
    get.
    set. 

    define public property numTipoRegraNegocio as RegraNegocio.ETipoRegraNegocio no-undo 
    get.
    set.     

    define public property oLog as Log.IColecaoLog no-undo 
    get.
    set.
    
    
         
    
    
end interface.