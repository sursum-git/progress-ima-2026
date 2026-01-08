DEF VAR c-situacao AS CHAR FORMAT "x(15)".
OUTPUT TO "c:/lixo/lixo.txt".
PUT "Est;" "Ser;" "Numero;" "Dt-Emiss;" "Nat-Oper;" "Situacao;" "Dt-Confirm;"
    "Dt-At-Ctb;" "Dt-At-Est;" "Dt-At-ObF;" "Dt-At-CR" SKIP.

FOR EACH nota-fiscal WHERE nota-fiscal.dt-cancela = ?
                       AND (nota-fiscal.dt-confirma = ? OR
                            nota-fiscal.dt-at-ct    = ? OR
                                        dt-at-est   = ? OR
                                        dt-at-ofest = ? OR
                                        dt-atual-cr = ?)
                     NO-LOCK.
    {esinc/i-dsrb.i nota-fiscal.ind-sit-nota nota-fiscal.ind-sit-nota c-situacao} 
    ASSIGN c-situacao = string(nota-fiscal.ind-sit-not,'99') + '-' + c-situacao.
    PUT nota-fiscal.cod-estabel  ";"
        nota-fiscal.serie ";"
        nota-fiscal.nr-nota-fis ";"
        nota-fiscal.dt-emis-nota ";"
        nota-fiscal.nat-operacao ";"
        c-situacao ";"
        nota-fiscal.dt-confirma ";"
        nota-fiscal.dt-at-ct ";"
        nota-fiscal.dt-at-est ";"
        nota-fiscal.dt-at-ofest ";"
        nota-fiscal.dt-atual-cr
        SKIP.
END.
OUTPUT CLOSE.
