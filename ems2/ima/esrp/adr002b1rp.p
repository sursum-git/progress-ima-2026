DEF BUFFER repres FOR repres.
DEF BUFFER emitente FOR emitente.

DEFINE TEMP-TABLE tt-param NO-UNDO
    FIELD destino          AS INTEGER
    FIELD arquivo          AS char format "x(35)"
    FIELD usuario          AS char format "x(12)"
    FIELD data-exec        AS date
    FIELD hora-exec        AS integer
    FIELD classifica       AS integer
    FIELD desc-classifica  AS char format "x(40)"
    FIELD modelo-rtf       AS char format "x(35)"
    FIELD l-habilitaRtf    AS LOG
    FIELD cod-emitente-ini LIKE emitente.cod-emitente
    FIELD cod-emitente-fim LIKE emitente.cod-emitente
    FIELD nome-abrev-ini   LIKE emitente.nome-abrev
    FIELD nome-abrev-fim   LIKE emitente.nome-abrev
    FIELD cod-rep-ini      LIKE emitente.cod-rep
    FIELD cod-rep-fim      LIKE emitente.cod-rep
    FIELD nome-ab-rep-ini  LIKE repres.nome-abrev
    FIELD nome-ab-rep-fim  LIKE repres.nome-abrev
    FIELD cidade-ini       LIKE emitente.cidade
    FIELD cidade-fim       LIKE emitente.cidade
    FIELD uf-ini           LIKE emitente.estado
    FIELD uf-fim           LIKE emitente.estado
    FIELD cod-ramo-ini     LIKE ext-emitente.cod-ramo-ativ
    FIELD cod-ramo-fim     LIKE ext-emitente.cod-ramo-ativ
    FIELD rs-dias          AS INT
    FIELD dt-ini           AS DATE
    FIELD dt-fim           AS DATE
    FIELD ativo            AS LOG
    FIELD inativo          AS LOG
    FIELD semcompra        AS LOG
    FIELD ordenar          AS INT
    FIELD tp-relatorio     AS INTEGER.

DEF TEMP-TABLE tt-cli
    FIELD cod-emit LIKE emitente.cod-emit
    FIELD ativo       AS LOGICAL FORMAT "X"
    FIELD inativo     AS LOGICAL FORMAT "X"
    FIELD novo        AS LOGICAL FORMAT "X"
    FIELD resgatado   AS LOGICAL FORMAT "X"
    FIELD frequente   AS LOGICAL FORMAT "X"
    FIELD qtd-compras AS INTEGER
    FIELD vl-fat      AS DECIMAL FORMAT ">,>>>,>>9.99"
    FIELD vl-dev      AS DECIMAL FORMAT ">,>>>,>>9.99".

DEF INPUT PARAMETER TABLE FOR tt-param.

DEF VAR h-acomp        AS HANDLE  NO-UNDO.
DEF VAR de-desconto    LIKE ems2ima.it-nota-fisc.val-desconto-total.
DEF VAR de-desc-12     AS DEC.
DEF VAR de-fator       AS DECIMAL.
DEF VAR i-dias-inat    AS INTEGER INIT 90.

FIND FIRST tt-param NO-LOCK NO-ERROR.
FIND FIRST ems2ima.para-ped NO-LOCK NO-ERROR.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Processando...").

FOR EACH emitente WHERE
         emitente.identific <> 2 NO-LOCK.

    RUN pi-acompanhar IN h-acomp (INPUT emitente.cod-emitente).

    FIND tt-cli WHERE
         tt-cli.cod-emit = emitente.cod-emit NO-ERROR.
         
    IF NOT AVAIL tt-cli THEN DO.
       CREATE tt-cli.
       ASSIGN tt-cli.cod-emit = emitente.cod-emit.
       ASSIGN tt-cli.ativo = NO
              tt-cli.novo = NO
              tt-cli.inativo = YES
              tt-cli.resgatado = NO
              tt-cli.frequente = NO.

       // Procura na Empresa Selecionada, se estiver na MED, o bacno "ems2ima"
       // ‚ o alias de TODAS as empresas 
       FIND FIRST ems2ima.nota-fiscal WHERE
                  ems2ima.nota-fiscal.cod-emit = tt-cli.cod-emit AND
                  ems2ima.nota-fiscal.dt-emis >= tt-param.dt-ini - i-dias-inat AND
                  ems2ima.nota-fiscal.dt-emis <= tt-param.dt-fim AND
                  ems2ima.nota-fiscal.dt-cancela = ?
                  NO-LOCK NO-ERROR.
       IF AVAIL ems2ima.nota-fiscal THEN DO.
          ASSIGN tt-cli.ativo = YES
                 tt-cli.novo = NO
                 tt-cli.inativo = NO
                 tt-cli.resgatado = NO
                 tt-cli.frequente = NO.

          FIND LAST ems2ima.nota-fiscal WHERE
                    ems2ima.nota-fiscal.cod-emit = tt-cli.cod-emit AND
                    ems2ima.nota-fiscal.dt-emis < tt-param.dt-ini AND
                    ems2ima.nota-fiscal.dt-cancela = ?
                    NO-LOCK NO-ERROR.
          IF NOT AVAIL ems2ima.nota-fiscal THEN DO.  // s¢ comprou no periodo selecionado
             ASSIGN tt-cli.ativo = NO
                    tt-cli.novo = YES
                    tt-cli.inativo = NO
                    tt-cli.resgatado = NO
                    tt-cli.frequente = NO.
          END.
          ELSE DO.
             IF ems2ima.nota-fiscal.dt-emis < tt-param.dt-ini - i-dias-inat THEN
                ASSIGN tt-cli.ativo = NO
                       tt-cli.novo = NO
                       tt-cli.inativo = NO
                       tt-cli.resgatado = YES
                       tt-cli.frequente = NO.
          END.
       END.
       ELSE DO.
           // Se nÆo achar, procura na outra Empresa
           FIND FIRST dbaux.nota-fiscal WHERE
                      dbaux.nota-fiscal.cod-emit = tt-cli.cod-emit AND
                      dbaux.nota-fiscal.dt-emis >= tt-param.dt-ini - i-dias-inat AND
                      dbaux.nota-fiscal.dt-emis <= tt-param.dt-fim AND
                      dbaux.nota-fiscal.dt-cancela = ? NO-LOCK NO-ERROR.
           IF AVAIL dbaux.nota-fiscal THEN DO.
              ASSIGN tt-cli.ativo = YES
                     tt-cli.novo = NO
                     tt-cli.inativo = NO
                     tt-cli.resgatado = NO
                     tt-cli.frequente = NO.
              FIND LAST dbaux.nota-fiscal WHERE
                        dbaux.nota-fiscal.cod-emit = tt-cli.cod-emit AND
                        dbaux.nota-fiscal.dt-emis < tt-param.dt-ini AND
                        dbaux.nota-fiscal.dt-cancela = ? NO-LOCK NO-ERROR.
              IF NOT AVAIL dbaux.nota-fiscal THEN DO.
                 ASSIGN tt-cli.ativo = NO
                        tt-cli.novo = YES
                        tt-cli.inativo = NO
                        tt-cli.resgatado = NO
                        tt-cli.frequente = NO.
              END.
              ELSE DO.
                 IF dbaux.nota-fiscal.dt-emis < tt-param.dt-ini - i-dias-inat THEN
                    ASSIGN tt-cli.ativo = NO
                           tt-cli.novo = NO
                           tt-cli.inativo = NO
                           tt-cli.resgatado = YES
                           tt-cli.frequente = NO.
              END.
           END.
       END.
    END.
END.

// Ver faturamento Empresa selecionada
FOR EACH ems2ima.nota-fiscal WHERE
         ems2ima.nota-fiscal.dt-emis >= tt-param.dt-ini - i-dias-inat AND
         ems2ima.nota-fiscal.dt-emis <= tt-param.dt-fim AND 
         ems2ima.nota-fiscal.dt-cancela = ? NO-LOCK.

    FIND ems2ima.natur-oper WHERE
         ems2ima.natur-oper.nat-operacao = ems2ima.nota-fiscal.nat-operacao NO-LOCK NO-ERROR.
    IF NOT AVAIL ems2ima.natur-oper THEN NEXT.

    IF ems2ima.natur-oper.tipo = 1 THEN NEXT. /* Movto de Entr */
    IF ems2ima.natur-oper.cod-esp <> "DP" THEN NEXT. /* Somente Pedidos que Geraram Duplicatas */

    FIND estabelec WHERE
         estabelec.cgc = nota-fiscal.cgc NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN NEXT.

    IF nota-fiscal.nome-abrev-tri <> "" AND 
       nota-fiscal.nome-abrev-tri = nota-fiscal.nome-ab-cli THEN NEXT. /* Nota fiscal Triangular */

    FIND tt-cli WHERE
         tt-cli.cod-emit = ems2ima.nota-fiscal.cod-emit NO-ERROR.
    IF NOT AVAIL tt-cli THEN NEXT.

    ASSIGN tt-cli.qtd-compras = tt-cli.qtd-compras + 1.
  
    IF ems2ima.nota-fiscal.dt-emis >= tt-param.dt-ini THEN DO.
       FOR EACH ems2ima.it-nota-fisc OF ems2ima.nota-fiscal NO-LOCK.
           ASSIGN tt-cli.vl-fat = tt-cli.vl-fat + ems2ima.it-nota-fisc.vl-tot-item +
                                  DECIMA(SUBSTR(ems2ima.it-nota-fisc.char-2,1500,10)).
       END.
    END.
END.

// Ver faturamento na outra empresa
FOR EACH dbaux.nota-fiscal WHERE
         dbaux.nota-fiscal.dt-emis >= tt-param.dt-ini - i-dias-inat AND
         dbaux.nota-fiscal.dt-emis <= tt-param.dt-fim AND 
         dbaux.nota-fiscal.dt-cancela = ? NO-LOCK.

    FIND dbaux.natur-oper WHERE
         dbaux.natur-oper.nat-operacao = dbaux.nota-fiscal.nat-operacao NO-LOCK NO-ERROR.
    IF NOT AVAIL dbaux.natur-oper THEN NEXT.

    IF dbaux.natur-oper.cod-esp <> "DP" THEN NEXT. /* Somente Pedidos que Geraram Duplicatas */
    IF dbaux.natur-oper.tipo = 1 THEN NEXT. /* Movto de Entr */

    FIND estabelec WHERE
         estabelec.cgc = dbaux.nota-fiscal.cgc NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN NEXT.

    IF dbaux.nota-fiscal.nome-abrev-tri <> "" AND 
       dbaux.nota-fiscal.nome-abrev-tri = dbaux.nota-fiscal.nome-ab-cli THEN NEXT. /* Nota fiscal Triangular */

    FIND tt-cli WHERE
         tt-cli.cod-emit = dbaux.nota-fiscal.cod-emit NO-ERROR.
    IF NOT AVAIL tt-cli THEN NEXT.

    ASSIGN tt-cli.qtd-compras = tt-cli.qtd-compras + 1.
    IF dbaux.nota-fiscal.dt-emis >= tt-param.dt-ini THEN DO.
       FOR EACH dbaux.it-nota-fisc OF dbaux.nota-fiscal NO-LOCK.
           ASSIGN tt-cli.vl-fat = tt-cli.vl-fat + dbaux.it-nota-fisc.vl-tot-item +
                                  DECIMA(SUBSTR(dbaux.it-nota-fisc.char-2,1500,10)).
       END.
    END.
END.

FOR EACH tt-cli WHERE
         tt-cli.ativo SHARE-LOCK.
    IF tt-cli.qtd-compra >= 4 THEN
       ASSIGN tt-cli.ativo = NO
              tt-cli.frequente = YES.
END.

// Ver Devolu‡Æo Empresa Selecionada
FIND FIRST ems2ima.ped-venda NO-LOCK NO-ERROR.

FOR EACH ems2ima.docum-est WHERE
         ems2ima.docum-est.cod-estab =  ems2ima.ped-venda.cod-estabel AND
         ems2ima.docum-est.dt-trans >= tt-param.dt-ini AND
         ems2ima.docum-est.dt-trans <= tt-param.dt-fim NO-LOCK.

    FIND ems2ima.natur-oper WHERE
         ems2ima.natur-oper.nat-operacao = ems2ima.docum-est.nat-operacao NO-LOCK NO-ERROR.
    IF NOT AVAIL ems2ima.natur-oper THEN NEXT.
    IF ems2ima.natur-oper.tipo-compra <> 3 THEN NEXT. /* Devolu‡Æo de Cliente */

    FIND tt-cli WHERE
         tt-cli.cod-emit = ems2ima.docum-est.cod-emit NO-ERROR.
    IF NOT AVAIL tt-cli THEN NEXT.

    FIND emitente WHERE
         emitente.cod-emit = tt-cli.cod-emit NO-LOCK NO-ERROR.
    FIND estabelec WHERE
         estabelec.cgc = emitente.cgc NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN NEXT.

    FOR EACH ems2ima.item-doc-est OF ems2ima.docum-est NO-LOCK.
        FIND movadm.nota-fiscal WHERE
             movadm.nota-fiscal.cod-estabel  = ems2ima.docum-est.cod-estabel   AND
             movadm.nota-fiscal.serie        = ems2ima.item-doc-est.serie-comp AND
             movadm.nota-fiscal.nr-nota-fis  = ems2ima.item-doc-est.nro-comp   NO-LOCK NO-ERROR.

        ASSIGN de-desc-12 = 0.
        FOR EACH ems2ima.it-nota-fisc OF movadm.nota-fiscal WHERE
                 ems2ima.it-nota-fisc.nr-seq-fat = ems2ima.item-doc-est.seq-comp NO-LOCK.

            ASSIGN de-fator = (ems2ima.item-doc-est.preco-total[1] / (ems2ima.it-nota-fisc.vl-preuni * ems2ima.it-nota-fisc.qt-faturada[1])).

            ASSIGN de-desconto = IF DEC(SUBSTR(ems2ima.it-nota-fisc.char-2,1500,10)) = 0 
                                 THEN ems2ima.it-nota-fisc.val-desconto-total 
                                 ELSE DEC(SUBSTR(ems2ima.it-nota-fisc.char-2,1500,10)).

            ASSIGN de-desc-12 = de-desconto * de-fator.
        END.

        ASSIGN de-desc-12 = 0.

        ACCUMULATE ems2ima.item-doc-est.preco-total[1] (TOTAL).
        ACCUMULATE de-desc-12 (TOTAL).
    END.
    ASSIGN tt-cli.vl-dev = tt-cli.vl-dev + (ACCUM TOTAL ems2ima.item-doc-est.preco-total[1]) +
                           (ACCUM TOTAL de-desc-12).
END.    


// Ver Devolu‡Æo Outra Empresa
FIND FIRST dbaux.ped-venda NO-LOCK NO-ERROR.

FOR EACH dbaux.docum-est WHERE
         dbaux.docum-est.cod-estab = dbaux.ped-venda.cod-estabel AND
         dbaux.docum-est.dt-trans >= tt-param.dt-ini AND
         dbaux.docum-est.dt-trans <= tt-param.dt-fim NO-LOCK.

    FIND tt-cli WHERE
         tt-cli.cod-emit = dbaux.docum-est.cod-emit NO-ERROR.
    IF NOT AVAIL tt-cli THEN NEXT.

    FIND dbaux.natur-oper WHERE
         dbaux.natur-oper.nat-operacao = dbaux.docum-est.nat-operacao NO-LOCK NO-ERROR.
    IF NOT AVAIL dbaux.natur-oper THEN NEXT.
    IF dbaux.natur-oper.tipo-compra <> 3 THEN NEXT. /* Devolu‡Æo de Cliente */

    FIND emitente WHERE
         emitente.cod-emit = tt-cli.cod-emit NO-LOCK NO-ERROR.
    FIND estabelec WHERE
         estabelec.cgc = emitente.cgc NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN NEXT.

    FOR EACH dbaux.item-doc-est OF dbaux.docum-est NO-LOCK.
        FIND dbaux.nota-fiscal WHERE
             dbaux.nota-fiscal.cod-estabel  = dbaux.docum-est.cod-estabel   AND
             dbaux.nota-fiscal.serie        = dbaux.item-doc-est.serie-comp AND
             dbaux.nota-fiscal.nr-nota-fis  = dbaux.item-doc-est.nro-comp   NO-LOCK NO-ERROR.

        ASSIGN de-desc-12 = 0.
        FOR EACH dbaux.it-nota-fisc OF movadm.nota-fiscal WHERE
                 dbaux.it-nota-fisc.nr-seq-fat = dbaux.item-doc-est.seq-comp NO-LOCK.

            ASSIGN de-fator = (dbaux.item-doc-est.preco-total[1] / (dbaux.it-nota-fisc.vl-preuni * dbaux.it-nota-fisc.qt-faturada[1])).

            ASSIGN de-desconto = IF DEC(SUBSTR(dbaux.it-nota-fisc.char-2,1500,10)) = 0 
                                 THEN dbaux.it-nota-fisc.val-desconto-total 
                                 ELSE DEC(SUBSTR(dbaux.it-nota-fisc.char-2,1500,10)).

            ASSIGN de-desc-12 = de-desconto * de-fator.
        END.
        ASSIGN de-desc-12 = 0.

        ACCUMULATE dbaux.item-doc-est.preco-total[1] (TOTAL).
        ACCUMULATE de-desc-12 (TOTAL).
    END.
    ASSIGN tt-cli.vl-dev = tt-cli.vl-dev + (ACCUM TOTAL dbaux.item-doc-est.preco-total[1]) +
                           (ACCUM TOTAL de-desc-12).
END.    

DISCONNECT dbaux.

OUTPUT TO c:\temp\ClientesAtivosInativos.csv.
PUT "CodCliente;NomeCliente;CodRepres;NomeRepres;Ativo;Inativo;Novo;Resgatado;Frequente;Vlr Faturado;Vlr Devolvido" SKIP.
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
        tt-cli.frequente    ";"
        tt-cli.vl-fat       ";"
        tt-cli.vl-dev       ";"
        SKIP.
END.
OUTPUT CLOSE.

MESSAGE 'Gerado o Arquivo c:\temp\ClientesAtivosInativos.csv'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

RUN pi-finalizar IN h-acomp. 

