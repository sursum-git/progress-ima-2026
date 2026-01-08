
/*------------------------------------------------------------------------
    File        : exec.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : administrator
    Created     : Fri May 02 10:53:19 BRT 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

routine-level on error undo, throw.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
run \\192.168.0.137\erp\scripts-8080\login-studio.p. 
run esp/implBoCxBanco.p .