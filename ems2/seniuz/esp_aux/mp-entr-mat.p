/* Programa: mp-entr-mat
** Atualizaá∆o manual de dados de an†lise de algod∆o.
*/

/* ------ para mostrar o(s) fornecedor(es), cujas notas tem esse nß -------*/
/*
FOR EACH mp-entr-mat WHERE mp-entr-mat.nro-docto = 342 NO-LOCK.
    FIND emitente WHERE emitente.cod-emitente = mp-entr-mat.cod-emit NO-LOCK.
    DISP mp-entr-mat.nro-docto
         mp-entr-mat.cod-emit
         mp-entr-mat.dt-recebimento
         emitente.nome-abrev.
END.
*/

FOR EACH mp-entr-mat WHERE mp-entr-mat.nro-docto = 342 
                       AND mp-entr-mat.cod-emit  = 14416 NO-LOCK.
    FOR EACH mp-fardo WHERE mp-fardo.nr-cdr = mp-entr-mat.nr-cdr.
        ASSIGN mp-fardo.sl1          = 14.5
               mp-fardo.sl2          = 29.8
               mp-fardo.ur           = 48.6
               mp-fardo.cd-compr     = 1
               mp-fardo.letra        = "A"
               mp-fardo.cd-tipo      = 3
               mp-fardo.cd-coloracao = 4
               mp-fardo.situacao     = 3.
    END.
    
END.

