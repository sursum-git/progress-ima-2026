/*output to "spool/carlosrs.txt".*/
for each estrutura where estrutura.es-codigo = "130226" no-lock.
    DISP estrutura.it-codigo
         estrutura.es-codigo
         estrutura.quant-usada
         estrutura.quant-usada / 2
           format "->>>,>>9.99999"
        .
end.
/*output close.*/

