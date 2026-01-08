 
 /*------------------------------------------------------------------------
    File        : RegraNegocioPrioridadeFrete
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : administrator
    Created     : Thu Apr 10 16:06:59 BRT 2025
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using RegraNegocio.IRegraNegocioPrioridade.

block-level on error undo, throw.

class RegraNegocio.Frete.RegraNegocioPrioridadeFrete implements IRegraNegocioPrioridade: 
    
     {RegraNegocio/IRegraNegocioPrioridade.i}  
     {ttResultado.i}    
    
     
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
    
    
    method public void adicionarAtributoValor(  ).

    
    end method.
    
    method public int64 encontrarItemRegraNegocio(  ).
    
    end method.

    method public void retTTResultado( output table ttResultado ).
   
   
    end method.    
    

end class.