
/*------------------------------------------------------------------------
    File        : ItemRegraNegocio
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : administrator
    Created     : Thu Apr 10 17:01:08 BRT 2025
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.

interface RegraNegocio.ItemRegraNegocio.IItemRegraNegocio: 
    
    
    
    define public property dtHrFimValid as datetime no-undo 
    get.
    set. 

    define public property dtHrIniValid as datetime no-undo 
    get.
    set. 

    define public property idRegraNegocio as int64 no-undo 
    get.
    set.  
    
    define public property idItemRegraNegocio as int64 no-undo 
    get.
    set.   

   

end interface.