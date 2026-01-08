
/*------------------------------------------------------------------------
    File        : IRegraNegocioPrioridade.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : administrator
    Created     : Thu Apr 10 14:29:46 BRT 2025
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
 //armazena os atributos e valores passados para futura busca
    define temp-table TTAtributos no-undo
        field nomeAtributo      as char
        field valorAtributo     as char
        field tipoDado          as char
        index ind is primary nomeAtributo.
 