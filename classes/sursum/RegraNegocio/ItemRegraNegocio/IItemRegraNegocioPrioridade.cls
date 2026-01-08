
/*------------------------------------------------------------------------
    File        : IItemRegraNegocioPrioridade
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : administrator
    Created     : Thu Apr 10 17:34:35 BRT 2025
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using RegraNegocio.ItemRegraNegocio.IItemRegraNegocio.

interface RegraNegocio.ItemRegraNegocio.IItemRegraNegocioPrioridade inherits IItemRegraNegocio:  
    
    
    {RegraNegocio/IRegraNegocioPrioridade.i}    
    {ttResultado.i}     
  
     
   {IretTT.i ttResultado}
    
    define public property logPrioridadeManual as logical no-undo 
    get.
    set. 

    define public property numPrioridade as int64 no-undo 
    get.
    set. 
    
    
    
    
    
  
end interface.