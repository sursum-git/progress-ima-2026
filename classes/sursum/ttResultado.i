
/*------------------------------------------------------------------------
    File        : ttResultado.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : administrator
    Created     : Mon Apr 14 14:34:42 BRT 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
define temp-table ttResultado no-undo
    field chave as char
    field valor as char
    index primario is primary is unique chave.