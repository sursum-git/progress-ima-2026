FOR EACH nota-fiscal  NO-LOCK
    WHERE nota-fiscal.cod-estabel = '5'
    AND   nota-fiscal.serie = '3'
    AND nota-fiscal.nr-nota-fis >= '0088202'
    AND nota-fiscal.nr-nota-fis <= '0088277':
    FIND FIRST ped-venda 
        WHERE ped-venda.nr-pedido =  int(nota-fiscal.nr-pedcli)
        NO-LOCK NO-ERROR.
    IF AVAIL ped-venda THEN DO:                                
       FOR EACH ped-repre OF ped-venda:
           CREATE fat-repre.
           ASSIGN fat-repre.cod-estabel = ped-venda.cod-estabel
                  fat-repre.serie       = nota-fiscal.serie
                  fat-repre.nr-fatura   = nota-fiscal.nr-fatura
                  fat-repre.perc-comis  = ped-repre.perc-comis
                  fat-repre.comis-emis  = 0
                  fat-repre.idi-tip-comis-agent = 1
                  fat-repre.idi-tip-comis-agent = 1
                  fat-repre.idi-liber-pagto-comis-agent = 1
                  fat-repre.nome-ab-rep  = ped-repre.nome-ab-rep .

       END.
    END.

END.

/*
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod-estabel                      char        im
   20 serie                            char        im
   30 nr-fatura                        char        im
   50 nome-ab-rep                      char        im
   60 perc-comis                       deci-8      m
   70 comis-emis                       inte        m
   80 char-1                           char
   90 char-2                           char
  100 dec-1                            deci-8
  110 dec-2                            deci-8
  120 int-1                            inte
  130 int-2                            inte
  140 log-1                            logi
  150 log-2                            logi
  160 data-1                           date
  170 data-2                           date
  180 check-sum                        char
  190 cod-classificador                char        im
  200 vl-base-calc-comis               deci-2      m
  210 idi-tip-comis-agent              inte        m
  220 idi-tip-base-comis-agent         inte        m
  230 idi-liber-pagto-comis-agent      inte        m
  240 val-comis                        deci-10     m
  250 val-comis-emis                   deci-10     m


*/
