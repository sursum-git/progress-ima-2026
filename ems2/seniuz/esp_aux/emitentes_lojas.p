DEF VAR l-tem-ind AS LOG FORMAT "Sim/Nao".
DEF VAR l-tem-eld AS LOG FORMAT "Sim/Nao".
DEF VAR l-tem-brp AS LOG FORMAT "Sim/Nao".
DEF VAR l-tem-sbn AS LOG FORMAT "Sim/Nao".

OUTPUT TO c:/temp/Eld_Brp_Sbn.csv.
PUT "Codigo;Cliente;Eldorado;Barro Preto;Sao Benedito" SKIP.

FOR EACH emitente WHERE emitente.identific <> 2 NO-LOCK.
    ASSIGN l-tem-eld = NO
           l-tem-brp = NO
           l-tem-sbn = NO.
    FIND FIRST nota-fiscal WHERE nota-fiscal.nome-ab-cli = emitente.nome-abrev
                             AND nota-fiscal.cod-estabel  = "3"
                           NO-LOCK NO-ERROR.
    IF AVAIL nota-fiscal THEN DO:
       ASSIGN l-tem-ind = YES.
       FIND FIRST nota-fiscal WHERE nota-fiscal.nome-ab-cli = emitente.nome-abrev
                                AND nota-fiscal.cod-estabel  = "4"
                              NO-LOCK NO-ERROR.
       IF AVAIL nota-fiscal THEN
          ASSIGN l-tem-brp = YES.
       FIND FIRST nota-fiscal WHERE nota-fiscal.nome-ab-cli = emitente.nome-abrev
                                AND nota-fiscal.cod-estabel  = "5"
                              NO-LOCK NO-ERROR.
       IF AVAIL nota-fiscal THEN
          ASSIGN l-tem-sbn = YES.

       IF l-tem-brp OR l-tem-sbn THEN
          PUT emitente.cod-emitente ";"
              emitente.nome-emit ";"
              l-tem-ind ";"
              l-tem-brp ";"
              l-tem-sbn
              SKIP.
    END.
END.
OUTPUT CLOSE.
