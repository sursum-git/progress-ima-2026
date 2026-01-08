 
/*------------------------------------------------------------------------
   File        : LerArquivoChaveValor
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : user
   Created     : Mon Apr 21 10:53:44 BRT 2025
   Notes       : 
 ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Arquivo.ILerArquivoChaveValor.

routine-level on error undo, throw.

class Arquivo.LerArquivoChaveValor implements ILerArquivoChaveValor: 
    
    {comum/ttChaveValor.i} 

    define public property arquivo as Arquivo.IArquivo no-undo 
        get.
        set. 

    define public property separador as character no-undo 
        get.
        set. 

    method public void ler(  ):
        
        input from value(arquivo:nomeArquivo).
        repeat:
            
            create ttChaveValor.
            import delimiter ";" ttChaveValor.
            
            
        end.
        
        
        input close.
        
        return.

    end method.

    {comum/retTT.i ttChaveValor}    

end class.