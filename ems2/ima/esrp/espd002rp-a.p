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
DEF VAR de-desconto    LIKE it-nota-fisc.val-desconto-total.
DEF VAR de-desc-12     AS DEC.
DEF VAR de-fator       AS DECIMAL.
DEF VAR i-dias-inat    AS INTEGER.

DEFINE VARIABLE hBoEspFatur AS HANDLE   NO-UNDO.

FIND FIRST tt-param NO-LOCK NO-ERROR.
FIND FIRST para-ped NO-LOCK NO-ERROR.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Processando...").


ASSIGN i-dias-inat = 0.
FIND im-param WHERE 
     im-param.cod-param = 'QTD_DIAS_INAT_CLIENTE' NO-LOCK NO-ERROR.
IF AVAIL im-param THEN
   ASSIGN i-dias-inat = INTEGER(im-param.val-param).

ASSIGN tt-param.dt-ini = TODAY - 30
       tt-param.dt-fim = TODAY.

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
         emitente.estado        <= tt-param.uf-fim            NO-LOCK.

    RUN pi-acompanhar IN h-acomp (INPUT emitente.cod-emitente).

    IF emitente.identific = 3 AND
       NOT CAN-FIND (FIRST nota-fiscal OF emitente) THEN NEXT.

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
       FIND FIRST nota-fiscal WHERE
                  nota-fiscal.cod-emit = tt-cli.cod-emit AND
                  nota-fiscal.dt-emis >= tt-param.dt-ini - i-dias-inat AND
                  nota-fiscal.dt-emis <= tt-param.dt-fim AND
                  nota-fiscal.dt-cancela = ?
                  NO-LOCK NO-ERROR.

       
      
       IF AVAIL nota-fiscal THEN DO.
          ASSIGN tt-cli.ativo = YES
                 tt-cli.novo = NO
                 tt-cli.inativo = NO
                 tt-cli.resgatado = NO
                 tt-cli.frequente = NO.

          FIND LAST nota-fiscal WHERE
                    nota-fiscal.cod-emit = tt-cli.cod-emit AND
                    nota-fiscal.dt-emis < tt-param.dt-ini AND
                    nota-fiscal.dt-cancela = ?
                    NO-LOCK NO-ERROR.
          IF NOT AVAIL nota-fiscal THEN DO.  // s¢ comprou no periodo selecionado
             ASSIGN tt-cli.ativo = NO
                    tt-cli.novo = YES
                    tt-cli.inativo = NO
                    tt-cli.resgatado = NO
                    tt-cli.frequente = NO.
          END.
          ELSE DO.
             IF (AVAIL nota-fiscal AND nota-fiscal.dt-emis < tt-param.dt-ini - i-dias-inat) THEN
                ASSIGN tt-cli.ativo         = NO
                       tt-cli.novo          = NO
                       tt-cli.inativo       = NO
                       tt-cli.resgatado     = YES
                       tt-cli.frequente     = NO.
          END.
       END.
    END.
/*     MESSAGE                                        */
/*          "emitente:"    emitente.cod-emitente SKIP */
/*          "Ativo:"       tt-cli.ativo     SKIP      */
/*          "Novo:"        tt-cli.novo      SKIP      */
/*          "Inativo:"     tt-cli.inativo   SKIP      */
/*          "Resgatado:"   tt-cli.resgatado SKIP      */
/*          "frequente:"   tt-cli.frequente SKIP      */
/*                                                    */
/*     VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.      */
END.


// Ver faturamento Empresa selecionada
FOR EACH nota-fiscal WHERE
         nota-fiscal.dt-emis >= tt-param.dt-ini - i-dias-inat AND
         nota-fiscal.dt-emis <= tt-param.dt-fim AND 
         nota-fiscal.dt-cancela = ? NO-LOCK.

    FIND natur-oper WHERE
         natur-oper.nat-operacao = nota-fiscal.nat-operacao NO-LOCK NO-ERROR.
    IF NOT AVAIL natur-oper THEN NEXT.

    IF natur-oper.tipo = 1 THEN NEXT. /* Movto de Entr */
    IF natur-oper.cod-esp <> "DP" THEN NEXT. /* Somente Pedidos que Geraram Duplicatas */

    FIND estabelec WHERE
         estabelec.cgc = nota-fiscal.cgc NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN NEXT.

    IF nota-fiscal.nome-abrev-tri <> "" AND 
       nota-fiscal.nome-abrev-tri = nota-fiscal.nome-ab-cli THEN NEXT. /* Nota fiscal Triangular */

    FIND tt-cli WHERE
         tt-cli.cod-emit = nota-fiscal.cod-emit NO-ERROR.
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

    FIND emitente WHERE
         emitente.cod-emit = tt-cli.cod-emit SHARE-LOCK NO-ERROR.

    FIND ext-emitente WHERE
         ext-emitente.cod-emit = tt-cli.cod-emit SHARE-LOCK NO-ERROR.

    IF tt-cli.ativo     THEN ASSIGN ext-emitente.situacao = 1.
    IF tt-cli.inativo   THEN ASSIGN ext-emitente.situacao = 2.
    IF tt-cli.novo      THEN ASSIGN ext-emitente.situacao = 3.
    IF tt-cli.resgatado THEN ASSIGN ext-emitente.situacao = 4.
    IF tt-cli.frequente THEN ASSIGN ext-emitente.situacao = 5.

    IF tt-cli.cod-rep = 1 THEN
       ASSIGN ext-emitente.situacao = 2.

    // Verifica se o Cliente est  Desativado
    IF emitente.ind-cre-cli = 3 THEN
      ASSIGN ext-emitente.situacao = 6.  // Desativado

    RUN esbo/esbo_fatur.p PERSIST SET hBoEspFatur .
    RUN iniciarBos      IN hBoEspFatur.
    RUN getDtUltFatCli  IN hBoEspFatur(emitente.cod-emitente,OUTPUT ext-emitente.dt-ult-fat-med).
    RELEASE ext-emitente.

    RUN finalizarBos IN hBoEspFatur.
    IF VALID-HANDLE(hBoEspFatur) THEN DO:
       DELETE PROCEDURE hBoEspFatur .
    END.
END.

RUN pi-finalizar IN h-acomp. 

