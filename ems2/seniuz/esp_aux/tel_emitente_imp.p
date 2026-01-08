/* Programa: tel_emitente_imp.p
** Objetivo: Importar altera‡äes de telefones para emitentes. 
*/

DEF TEMP-TABLE tt-emitente
    FIELD cod-emitente LIKE emitente.cod-emitente
    FIELD tel1         AS CHAR FORMAT "x(15)"
    FIELD tel2         AS CHAR FORMAT "x(15)".
    
input from "c:/temp/tel_emitente1.csv" CONVERT SOURCE "ibm850".
SET ^.

repeat:
   create tt-emitente.
   import delimiter ";" tt-emitente.
end.
input close.

FOR EACH tt-emitente.
    FIND emitente WHERE emitente.cod-emitente = tt-emitente.cod-emitente.
    ASSIGN emitente.telefone[1] = tt-emitente.tel1
           emitente.telefone[2] = tt-emitente.tel2.
END.

