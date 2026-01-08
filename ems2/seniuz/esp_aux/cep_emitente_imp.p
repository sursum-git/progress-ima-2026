/* Programa: cep_emitente_imp.p
** Objetivo: Importar altera‡äes de endere‡os para emitentes. 
*/

DEF TEMP-TABLE tt-emitente
    FIELD cod-emitente LIKE emitente.cod-emitente
    FIELD endereco     LIKE emitente.endereco
    FIELD bairro       LIKE emitente.bairro
    FIELD cidade       LIKE emitente.cidade
    FIELD estado       LIKE emitente.estado
    FIELD endereco-cob LIKE emitente.endereco-cob
    FIELD bairro-cob   LIKE emitente.bairro-cob
    FIELD cidade-cob   LIKE emitente.cidade-cob
    FIELD estado-cob   LIKE emitente.estado-cob.
    
input from "c:/temp/cep_valido_2.csv" CONVERT SOURCE "ibm850".
SET ^.

repeat:
   create tt-emitente.
   import delimiter ";" tt-emitente.
end.
input close.

FOR EACH tt-emitente.
    FIND emitente WHERE emitente.cod-emitente = tt-emitente.cod-emitente.
    ASSIGN emitente.endereco     = tt-emitente.endereco
           emitente.bairro       = tt-emitente.bairro
           emitente.cidade       = tt-emitente.cidade
           emitente.estado       = tt-emitente.estado
           emitente.endereco-cob = tt-emitente.endereco-cob
           emitente.bairro-cob   = tt-emitente.bairro-cob  
           emitente.cidade-cob   = tt-emitente.cidade-cob  
           emitente.estado-cob   = tt-emitente.estado-cob.  
END.

