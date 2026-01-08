FOR EACH mp-fardo WHERE mp-fardo.nr-cdr = 19605.
    DISP mp-fardo.padrao.
    /* assign mp-fardo.padrao = "xx xxxxxxxxxxxx".*/
END.

    
FOR EACH mp-entr-mat WHERE mp-entr-mat.nr-cdr = 19605.
    UPDATE mp-entr-mat.procedencia
           mp-entr-mat.padrao.
END.
