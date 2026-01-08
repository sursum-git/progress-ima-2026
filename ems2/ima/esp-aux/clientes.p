DEF TEMP-TABLE tt-cli
    FIELD cod-emit LIKE emitente.cod-emit
    FIELD ativo     AS LOGICAL
    FIELD inativo   AS LOGICAL
    FIELD novo      AS LOGICAL
    FIELD resgatado AS LOGICAL
    FIELD vl-fat    AS DECIMAL FORMAT ">,>>>,>>9.99"
    FIELD vl-dev    AS DECIMAL FORMAT ">,>>>,>>9.99".

DEF VAR da-dt-ini AS DATE.
DEF VAR da-dt-fin AS DATE.
DEF VAR de-desconto      LIKE ems2ima.it-nota-fisc.val-desconto-total.
DEF VAR de-desc-12       AS DEC.

ASSIGN da-dt-ini = 11.01.2017
       da-dt-fin = 11.30.2017.

FOR EACH emitente WHERE
         emitente.identific <> 2 NO-LOCK.
    DISP emitente.cod-emit.
    PAUSE 0.

    FIND tt-cli WHERE
         tt-cli.cod-emit = emitente.cod-emit NO-ERROR.
         
    IF NOT AVAIL tt-cli THEN DO.
       CREATE tt-cli.
       ASSIGN tt-cli.cod-emit = emitente.cod-emit.
       ASSIGN tt-cli.ativo = NO
              tt-cli.novo = NO
              tt-cli.inativo = YES
              tt-cli.resgatado = NO.

       // Procura na Empresa Selecionada, se estiver na MED, o bacno "ems2ima"
       // ‚ o Alias de TODAS as empresas 
       FIND FIRST ems2ima.nota-fiscal WHERE
                  ems2ima.nota-fiscal.cod-emit = tt-cli.cod-emit AND
                  ems2ima.nota-fiscal.dt-emis >= da-dt-ini AND
                  ems2ima.nota-fiscal.dt-emis <= da-dt-fin AND
                  ems2ima.nota-fiscal.dt-cancela = ?
                  NO-LOCK NO-ERROR.
       IF AVAIL ems2ima.nota-fiscal THEN DO.
          ASSIGN tt-cli.ativo = YES
                 tt-cli.novo = NO
                 tt-cli.inativo = NO
                 tt-cli.resgatado = NO.

          FIND FIRST ems2ima.nota-fiscal WHERE
                     ems2ima.nota-fiscal.cod-emit = tt-cli.cod-emit AND
                     ems2ima.nota-fiscal.dt-emis < da-dt-ini AND
                     ems2ima.nota-fiscal.dt-cancela = ?
                     NO-LOCK NO-ERROR.
          IF NOT AVAIL ems2ima.nota-fiscal THEN
             ASSIGN tt-cli.ativo = NO
                    tt-cli.novo = YES
                    tt-cli.inativo = NO
                    tt-cli.resgatado = NO.
          ELSE DO.
             IF ems2ima.nota-fiscal.dt-emis < da-dt-ini - 90 THEN
                ASSIGN tt-cli.ativo = NO
                       tt-cli.novo = NO
                       tt-cli.inativo = NO
                       tt-cli.resgatado = YES.
          END.
       END.
       ELSE DO.
           // Se nÆo achar, procura na outra Empresa
           FIND FIRST dbaux.nota-fiscal WHERE
                      dbaux.nota-fiscal.cod-emit = tt-cli.cod-emit AND
                      dbaux.nota-fiscal.dt-emis >= da-dt-ini AND
                      dbaux.nota-fiscal.dt-emis <= da-dt-fin AND
                      dbaux.nota-fiscal.dt-cancela = ? NO-LOCK NO-ERROR.
           IF AVAIL dbaux.nota-fiscal THEN DO.
              ASSIGN tt-cli.ativo = YES
                     tt-cli.novo = NO
                     tt-cli.inativo = NO
                     tt-cli.resgatado = NO.
              FIND FIRST dbaux.nota-fiscal WHERE
                         dbaux.nota-fiscal.cod-emit = tt-cli.cod-emit AND
                         dbaux.nota-fiscal.dt-emis < da-dt-ini AND
                         dbaux.nota-fiscal.dt-cancela = ? NO-LOCK NO-ERROR.
              IF NOT AVAIL dbaux.nota-fiscal THEN
                 ASSIGN tt-cli.ativo = NO
                        tt-cli.novo = YES
                        tt-cli.inativo = NO
                        tt-cli.resgatado = NO.
              ELSE DO.
                 IF dbaux.nota-fiscal.dt-emis < da-dt-ini - 90 THEN
                    ASSIGN tt-cli.ativo = NO
                           tt-cli.novo = NO
                           tt-cli.inativo = NO
                           tt-cli.resgatado = YES.
              END.
           END.
       END.
    END.
END.

// Ver faturamento Empresa selecionada
FOR EACH ems2ima.nota-fiscal WHERE
         ems2ima.nota-fiscal.dt-emis >= da-dt-ini AND
         ems2ima.nota-fiscal.dt-emis <= da-dt-fin AND 
         ems2ima.nota-fiscal.dt-cancela = ? NO-LOCK.

    FIND tt-cli WHERE
         tt-cli.cod-emit = ems2ima.nota-fiscal.cod-emit NO-ERROR.
    IF NOT AVAIL tt-cli THEN NEXT.

    FOR EACH ems2ima.it-nota-fisc OF ems2ima.nota-fiscal NO-LOCK.
        ASSIGN tt-cli.vl-fat = tt-cli.vl-fat + ems2ima.it-nota-fisc.vl-tot-item +
                               DECIMA(SUBSTR(ems2ima.it-nota-fisc.char-2,1500,10)).
    END.
END.

// Ver faturamento na outra empresa
FOR EACH dbaux.nota-fiscal WHERE
         dbaux.nota-fiscal.dt-emis >= da-dt-ini AND
         dbaux.nota-fiscal.dt-emis <= da-dt-fin AND 
         dbaux.nota-fiscal.dt-cancela = ? NO-LOCK.

    FIND tt-cli WHERE
         tt-cli.cod-emit = dbaux.nota-fiscal.cod-emit NO-ERROR.
    IF NOT AVAIL tt-cli THEN NEXT.

    FOR EACH dbaux.it-nota-fisc OF dbaux.nota-fiscal NO-LOCK.
        ASSIGN tt-cli.vl-fat = tt-cli.vl-fat + dbaux.it-nota-fisc.vl-tot-item +
                               DECIMA(SUBSTR(dbaux.it-nota-fisc.char-2,1500,10)).
    END.
END.

// Ver Devolu‡Æo Empresa Selecionada
FIND FIRST ems2ima.ped-venda NO-LOCK NO-ERROR.

FOR EACH ems2ima.docum-est WHERE
         ems2ima.docum-est.cod-estab =  ems2ima.ped-venda.cod-estabel AND
         ems2ima.docum-est.dt-trans >= da-dt-ini AND
         ems2ima.docum-est.dt-trans <= da-dt-fin NO-LOCK.

    FIND tt-cli WHERE
         tt-cli.cod-emit = ems2ima.docum-est.cod-emit NO-ERROR.
    IF NOT AVAIL tt-cli THEN NEXT.

    FIND ems2ima.natur-oper WHERE
         ems2ima.natur-oper.nat-operacao = ems2ima.docum-est.nat-operacao NO-LOCK NO-ERROR.
    IF NOT AVAIL ems2ima.natur-oper THEN NEXT.
    IF ems2ima.natur-oper.tipo-compra <> 3 THEN NEXT. /* Devolu‡Æo de Cliente */

    FOR EACH ems2ima.item-doc-est OF ems2ima.docum-est NO-LOCK.
        FIND movadm.nota-fiscal WHERE
             movadm.nota-fiscal.cod-estabel  = ems2ima.docum-est.cod-estabel   AND
             movadm.nota-fiscal.serie        = ems2ima.item-doc-est.serie-comp AND
             movadm.nota-fiscal.nr-nota-fis  = ems2ima.item-doc-est.nro-comp   NO-LOCK NO-ERROR.

        ASSIGN de-desc-12 = 0.
        FOR EACH ems2ima.it-nota-fisc OF movadm.nota-fiscal WHERE
                 ems2ima.it-nota-fisc.nr-seq-fat = ems2ima.item-doc-est.seq-comp NO-LOCK.

            ASSIGN de-desconto = IF DEC(SUBSTR(ems2ima.it-nota-fisc.char-2,1500,10)) = 0 
                                 THEN ems2ima.it-nota-fisc.val-desconto-total 
                                 ELSE DEC(SUBSTR(ems2ima.it-nota-fisc.char-2,1500,10)).

            ASSIGN de-desc-12 = ((de-desconto / ems2ima.it-nota-fisc.qt-faturada[1]) * ems2ima.item-doc-est.quantidade).
        END.

        ACCUMULATE ems2ima.item-doc-est.preco-total[1] (TOTAL).
        ACCUMULATE de-desc-12 (TOTAL).
    END.
    ASSIGN tt-cli.vl-dev = tt-cli.vl-dev + (ACCUM TOTAL ems2ima.item-doc-est.preco-total[1]) +
                           (ACCUM TOTAL de-desc-12).
END.    


// Ver Devolu‡Æo Outra Empresa
FIND FIRST dbaux.ped-venda NO-LOCK NO-ERROR.

FOR EACH dbaux.docum-est WHERE
         dbaux.docum-est.cod-estab =  dbaux.ped-venda.cod-estabel AND
         dbaux.docum-est.dt-trans >= da-dt-ini AND
         dbaux.docum-est.dt-trans <= da-dt-fin NO-LOCK.

    FIND tt-cli WHERE
         tt-cli.cod-emit = dbaux.docum-est.cod-emit NO-ERROR.
    IF NOT AVAIL tt-cli THEN NEXT.

    FIND dbaux.natur-oper WHERE
         dbaux.natur-oper.nat-operacao = ems2ima.docum-est.nat-operacao NO-LOCK NO-ERROR.
    IF NOT AVAIL dbaux.natur-oper THEN NEXT.
    IF dbaux.natur-oper.tipo-compra <> 3 THEN NEXT. /* Devolu‡Æo de Cliente */

    FOR EACH dbaux.item-doc-est OF dbaux.docum-est NO-LOCK.
        FIND dbaux.nota-fiscal WHERE
             dbaux.nota-fiscal.cod-estabel  = dbaux.docum-est.cod-estabel   AND
             dbaux.nota-fiscal.serie        = dbaux.item-doc-est.serie-comp AND
             dbaux.nota-fiscal.nr-nota-fis  = dbaux.item-doc-est.nro-comp   NO-LOCK NO-ERROR.

        ASSIGN de-desc-12 = 0.
        FOR EACH dbaux.it-nota-fisc OF movadm.nota-fiscal WHERE
                 dbaux.it-nota-fisc.nr-seq-fat = dbaux.item-doc-est.seq-comp NO-LOCK.

            ASSIGN de-desconto = IF DEC(SUBSTR(dbaux.it-nota-fisc.char-2,1500,10)) = 0 
                                 THEN dbaux.it-nota-fisc.val-desconto-total 
                                 ELSE DEC(SUBSTR(dbaux.it-nota-fisc.char-2,1500,10)).

            ASSIGN de-desc-12 = ((de-desconto / dbaux.it-nota-fisc.qt-faturada[1]) * ems2ima.item-doc-est.quantidade).
        END.

        ACCUMULATE dbaux.item-doc-est.preco-total[1] (TOTAL).
        ACCUMULATE de-desc-12 (TOTAL).
    END.
    ASSIGN tt-cli.vl-dev = tt-cli.vl-dev + (ACCUM TOTAL dbaux.item-doc-est.preco-total[1]) +
                           (ACCUM TOTAL de-desc-12).
END.    

OUTPUT TO c:\temp\t1.txt.
FOR EACH tt-cli.
    FIND emitente WHERE
         emitente.cod-emit = tt-cli.cod-emit NO-LOCK NO-ERROR.

    FIND repres WHERE
         repres.cod-rep = emitente.cod-rep NO-LOCK NO-ERROR.

    PUT tt-cli.cod-emit     ";"
        emitente.nome-emit  ";"
        emitente.cod-rep    ";"
        IF AVAIL repres
           THEN repres.nome-abrev
           ELSE "INVµLIDO"  ";"
        tt-cli.ativo        ";"
        tt-cli.inativo      ";"
        tt-cli.novo         ";"
        tt-cli.resgatado    ";"
        tt-cli.vl-fat       ";"
        tt-cli.vl-dev       ";"
        SKIP.
END.

