
/*------------------------------------------------------------------------
    File        : IRegraNegocioPrioridade
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : administrator
    Created     : Thu Apr 10 16:16:49 BRT 2025
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using RegraNegocio.IRegraNegocio.

interface RegraNegocio.IRegraNegocioPrioridade inherits IRegraNegocio:  
    
      
      
      
   
 /*------------------------------------------------------------------------------
     Purpose:possibilitar que os atributos para busca sejam adicionados, juntamente
     com os seus valores
     Notes:
    ------------------------------------------------------------------------------*/

    method public void adicionarAtributoValor(  ).

    /*-----------------------------------------------------------------------------------
     Purpose:a partir dos atributos e valores encontra o Item da regra e retorna o numero
     do id da mesma.     
     Notes: É premisa que todos os itens da regra de negócio tenha um ID como chave primaria 
    ----------------------------------------------------------------------------------------*/

    method public int64 encontrarItemRegraNegocio(  ).

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/   
    
  
end interface.