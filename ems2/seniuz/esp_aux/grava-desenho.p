    {esinc/sz-pcl.i}
    
    fn-grava-macro("image\logo-etq10.prn"). 
    fn-grava-macro("image\rlgp11.prn").
    fn-grava-macro("image\rlgp12.prn"). 
    fn-grava-macro("image\rlgp13.prn").
    fn-grava-macro("image\rlgp14.prn"). 
    fn-grava-macro("image\rlgp15.prn"). 
    fn-grava-macro("image\rlgp16.prn"). 
    fn-grava-macro("image\rlgp17.prn"). 
    fn-grava-macro("image\rlgp18.prn"). 
    fn-grava-macro("image\rlgp19.prn"). 
    fn-grava-macro("image\rlgp20.prn"). 


    /*
  
    
 /* Funá∆o para gravar imagens na mem¢ria da impressora */
FUNCTION fn-grava-macro RETURNS CHARACTER
  ( INPUT arq-image AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Gravar Imagens na Mem¢ria da Impressora 
    Notes: Esta funcao Grava as imagens em macros, lembrando que a definiá∆o das
           macros est∆o inclusas nas imagens e foram feitas com um editor bin†rio 
------------------------------------------------------------------------------*/
    DEF VAR c-comando AS CHAR.
    ASSIGN c-comando = "net use lpt2: " + IF SESSION:PRINTER-NAME BEGINS "\\" 
                                          THEN SESSION:PRINTER-NAME
                                          ELSE SESSION:PRINTER-PORT.
    OS-COMMAND SILENT VALUE(c-comando).  

    ASSIGN c-comando = "copy /Y /b " + arq-image + " lpt2". 
    OS-COMMAND SILENT VALUE(c-comando). 

    ASSIGN c-comando = "net use lpt2: /DELETE".
    OS-COMMAND SILENT VALUE(c-comando).  

END FUNCTION.
   
    
    
    
    
    
    */
