
/*------------------------------------------------------------------------
    File        : IPersistencia
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : administrator
    Created     : Thu Apr 10 16:49:20 BRT 2025
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.

interface Persistencia.IPersistencia:  
    
    
  
    define public property oRegistro as Progress.Lang.Object no-undo 
    get.
    set. 

    method public int64 encontrar(  ).

    method public int64 salvar(  ).    
    
    method public logical excluir(  ).
    
    
    

end interface.