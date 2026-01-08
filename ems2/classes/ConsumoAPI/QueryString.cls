 
 /*------------------------------------------------------------------------
    File        : QueryStringPhp
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : user
    Created     : Tue Apr 15 09:25:37 BRT 2025
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using consumoAPi.IQueryString from propath.

routine-level on error undo, throw.

class consumoAPi.QueryString implements IQueryString: 
    
    define public property ordem as int no-undo 
    get.
    set. 

    define public property valor as character no-undo 
    get.
    set. 

    define public property chave as character no-undo 
    get.
    set. 
    
    

end class.