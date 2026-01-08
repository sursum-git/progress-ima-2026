
/*------------------------------------------------------------------------
    File        : logar.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : administrator
    Created     : Thu Apr 24 17:20:25 BRT 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

routine-level on error undo, throw.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
define temp-table tt-erros
   field cod-erro  as integer
   field desc-erro as character format "x(256)":U
   field desc-arq  as character.
{utp/ut-glob.i}

//RUN btb/btb910za.w.
/*run btb/btapi910za.p (input "super", 
                      input "super", 
                      output TABLE tt-erros).*/
run \\192.168.0.137\erp\scripts-8080\login-studio.p.                      
assign c-seg-usuario = 'super'.
message c-seg-usuario
view-as alert-box.                  
                      
run esp/esfl002.w.                      