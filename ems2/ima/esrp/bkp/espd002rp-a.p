/* Programa: ESPD002
** Modulo..: Clientes
** Objetivo: Analisa Clientes e determina a Situa‡Æo dos mesmos
** Autor...: Antonio Geraldo de Souza - JANEIRO/2019
**
*/

{esp/espd002.i}

/* Parametros de entrada logica obrigatoria */
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita                   AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.       

DEF BUFFER empresa FOR mgcad.empresa.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

DEF BUFFER repres FOR repres.
DEF BUFFER emitente FOR emitente.

DEF TEMP-TABLE tt-cli
    FIELD cod-emit    LIKE emitente.cod-emit
    FIELD cod-rep     LIKE emitente.cod-rep
    FIELD ativo       AS LOGICAL FORMAT "X"
    FIELD inativo     AS LOGICAL FORMAT "X"
    FIELD novo        AS LOGICAL FORMAT "X"
    FIELD resgatado   AS LOGICAL FORMAT "X"
    FIELD frequente   AS LOGICAL FORMAT "X"
    FIELD qtd-compras AS INTEGER.

DEF VAR h-acomp        AS HANDLE  NO-UNDO.
DEF VAR de-desconto    LIKE ems2ima.it-nota-fisc.val-desconto-total.
DEF VAR de-desc-12     AS DEC.
DEF VAR de-fator       AS DECIMAL.
DEF VAR i-dias-inat    AS INTEGER.

FIND FIRST tt-param NO-LOCK NO-ERROR.
FIND FIRST ems2ima.para-ped NO-LOCK NO-ERROR.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Processando...").


ASSIGN i-dias-inat = 120.
FIND im-param WHERE 
     im-param.cod-param = 'QTD_DIAS_INAT_CLIENTE' NO-LOCK NO-ERROR.
IF AVAIL im-param THEN
   ASSIGN i-dias-inat = INTEGER(im-param.val-param).


ASSIGN tt-param.dt-ini = TODAY - 30
       tt-param.dt-fim = TODAY.

// o banco dbaux ‚ conectado no espd002.w
FOR EACH emitente WHERE
         emitente.identific     <> 2                          AND 
         emitente.cod-emitente  >= tt-param.cod-emitente-ini  AND
         emitente.cod-emitente  <= tt-param.cod-emitente-fim  AND
         emitente.nome-abrev    >= tt-param.nome-abrev-ini    AND
         emitente.nome-abrev    <= tt-param.nome-abrev-fim    AND
         emitente.cod-rep       >= tt-param.cod-rep-ini       AND
         emitente.cod-rep       <= tt-param.cod-rep-fim       AND
         emitente.cidade        >= tt-param.cidade-ini        AND
         emitente.cidade        <= tt-param.cidade-fim        AND
         emitente.estado        >= tt-param.uf-ini            AND
         emitente.estado        <= tt-param.uf-fim   NO-LOCK.

    RUN pi-acompanhar IN h-acomp (INPUT emitente.cod-emitente).

    IF emitente.identific = 3 AND
       NOT CAN-FIND (FIRST ems2ima.nota-fiscal OF emitente) AND
       NOT CAN-FIND (FIRST dbaux.nota-fiscal OF emitente) THEN NEXT.

    FIND tt-cli WHERE
         tt-cli.cod-emit = emitente.cod-emit NO-ERROR.
         
    IF NOT AVAIL tt-cli THEN DO.
       CREATE tt-cli.
       ASSIGN tt-cli.cod-emit = emitente.cod-emit
              tt-cli.cod-rep = emitente.cod-rep.
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

          FIND LAST dbaux.nota-fiscal WHERE
                    dbaux.nota-fiscal.cod-emit = tt-cli.cod-emit AND
                    dbaux.nota-fiscal.dt-emis < tt-param.dt-ini AND
                    dbaux.nota-fiscal.dt-cancela = ?
                    NO-LOCK NO-ERROR.

          FIND LAST ems2ima.nota-fiscal WHERE
                    ems2ima.nota-fiscal.cod-emit = tt-cli.cod-emit AND
                    ems2ima.nota-fiscal.dt-emis < tt-param.dt-ini AND
                    ems2ima.nota-fiscal.dt-cancela = ?
                    NO-LOCK NO-ERROR.
          IF NOT AVAIL ems2ima.nota-fiscal AND
             NOT AVAIL dbaux.nota-fiscal THEN DO.  // s¢ comprou no periodo selecionado
             ASSIGN tt-cli.ativo = NO
                    tt-cli.novo = YES
                    tt-cli.inativo = NO
                    tt-cli.resgatado = NO
                    tt-cli.frequente = NO.
          END.
          ELSE DO.
             IF (AVAIL ems2ima.nota-fiscal AND ems2ima.nota-fiscal.dt-emis < tt-param.dt-ini - i-dias-inat) OR
                (AVAIL dbaux.nota-fiscal   AND dbaux.nota-fiscal.dt-emis < tt-param.dt-ini - i-dias-inat) THEN
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

              FIND LAST ems2ima.nota-fiscal WHERE
                        ems2ima.nota-fiscal.cod-emit = tt-cli.cod-emit AND
                        ems2ima.nota-fiscal.dt-emis < tt-param.dt-ini AND
                        ems2ima.nota-fiscal.dt-cancela = ?
                        NO-LOCK NO-ERROR.

              FIND LAST dbaux.nota-fiscal WHERE
                        dbaux.nota-fiscal.cod-emit = tt-cli.cod-emit AND
                        dbaux.nota-fiscal.dt-emis < tt-param.dt-ini AND
                        dbaux.nota-fiscal.dt-cancela = ? NO-LOCK NO-ERROR.
              IF NOT AVAIL dbaux.nota-fiscal AND
                 NOT AVAIL ems2ima.nota-fiscal THEN DO.
                 ASSIGN tt-cli.ativo = NO
                        tt-cli.novo = YES
                        tt-cli.inativo = NO
                        tt-cli.resgatado = NO
                        tt-cli.frequente = NO.
              END.
              ELSE DO.
                 IF (AVAIL dbaux.nota-fiscal   AND dbaux.nota-fiscal.dt-emis < tt-param.dt-ini - i-dias-inat) OR
                    (AVAIL ems2ima.nota-fiscal AND ems2ima.nota-fiscal.dt-emis < tt-param.dt-ini - i-dias-inat) THEN     
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
END.


FOR EACH tt-cli WHERE
         tt-cli.ativo SHARE-LOCK.
    IF tt-cli.qtd-compra >= 4 THEN
       ASSIGN tt-cli.ativo = NO
              tt-cli.frequente = YES.
END.


FIND FIRST para-ped NO-LOCK NO-ERROR.

FOR EACH tt-cli NO-LOCK.
    RUN pi-acompanhar IN h-acomp (INPUT tt-cli.cod-emit).

    FIND ext-emitente WHERE
         ext-emitente.cod-emit = tt-cli.cod-emit SHARE-LOCK NO-ERROR.

    IF tt-cli.ativo     THEN ASSIGN ext-emitente.situacao = 1.
    IF tt-cli.inativo   THEN ASSIGN ext-emitente.situacao = 2.
    IF tt-cli.novo      THEN ASSIGN ext-emitente.situacao = 3.
    IF tt-cli.resgatado THEN ASSIGN ext-emitente.situacao = 4.
    IF tt-cli.frequente THEN ASSIGN ext-emitente.situacao = 5.

    IF tt-cli.cod-rep = 1 THEN
       ASSIGN ext-emitente.situacao = 2.

    FIND LAST ems2ima.nota-fiscal WHERE
              ems2ima.nota-fiscal.cod-emit = tt-cli.cod-emit AND
              ems2ima.nota-fiscal.dt-cancela = ?
              NO-LOCK NO-ERROR.
    FIND LAST dbaux.nota-fiscal WHERE
              dbaux.nota-fiscal.cod-emit = tt-cli.cod-emit AND
              dbaux.nota-fiscal.dt-cancela = ?
              NO-LOCK NO-ERROR.

    IF para-ped.estab-padrao = '1' THEN DO.
       IF AVAIL ems2ima.nota-fiscal THEN 
          ASSIGN ext-emitente.dt-ult-fat-ima = ems2ima.nota-fiscal.dt-emis.

       IF AVAIL dbaux.nota-fiscal THEN 
          ASSIGN ext-emitente.dt-ult-fat-med = dbaux.nota-fiscal.dt-emis.
    END.
    ELSE DO.
        IF AVAIL ems2ima.nota-fiscal THEN
           ASSIGN ext-emitente.dt-ult-fat-med = ems2ima.nota-fiscal.dt-emis.
        IF AVAIL dbaux.nota-fiscal THEN 
           ASSIGN ext-emitente.dt-ult-fat-ima = dbaux.nota-fiscal.dt-emis.
    END.

END.

IF CONNECTED("dbaux") THEN
   DISCONNECT dbaux.


RUN pi-finalizar IN h-acomp. 

