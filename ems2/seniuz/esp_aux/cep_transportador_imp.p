/* Programa: cep_transportador_imp.p
** Objetivo: Importar altera‡äes de endere‡os para transportadores. 
*/

DEF TEMP-TABLE tt-transportador
    FIELD cod-transp   LIKE transporte.cod-transp
    FIELD endereco     LIKE transporte.endereco
    FIELD bairro       LIKE transporte.bairro
    FIELD cidade       LIKE transporte.cidade
    FIELD estado       LIKE transporte.estado.
    
input from "c:/temp/cep_val_transp_2.csv".
SET ^.

repeat:
   create tt-transportador.
   import delimiter ";" tt-transportador.
end.
input close.

FOR EACH tt-transportador.
    FIND transporte WHERE transporte.cod-transp = tt-transportador.cod-transp.
    ASSIGN transporte.endereco = tt-transportador.endereco
           transporte.bairro   = tt-transportador.bairro
           transporte.cidade   = tt-transportador.cidade
           transporte.estado   = tt-transportador.estado.
END.

