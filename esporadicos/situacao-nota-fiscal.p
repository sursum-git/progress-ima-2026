OUTPUT TO c:\temp\nota-fiscal-situacao.txt.

FOR EACH nota-fiscal
    WHERE nr-nota-fis = '0042180'
    AND  cod-estabel = '1'
    AND serie = '3':
    DISP {diinc/i04di087.i 4 ind-sit-nota} FORMAT 'x(50)'
         {diinc/i02di135.i 4 idi-forma-emis-nf-eletro} FORMAT 'x(50)'
         {diinc/i01di135.i 4 idi-sit-nf-eletro} FORMAT 'x(50)'
         {diinc/i09di037.i 4 ind-tip-nota}  FORMAT 'x(50)'.
    FOR EACH sit-nf-eletro 
        WHERE cod-estabel    = nota-fiscal.cod-estabel
        AND   cod-serie      = nota-fiscal.serie
        AND   cod-nota-fisc  = nota-fiscal.nr-nota-fis
        NO-LOCK:
        PUT sit-nf-eletro.idi-sit-nf-eletro.

    END.
END.
