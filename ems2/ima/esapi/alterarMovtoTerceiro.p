/*******************************************************************
programa:esapi/alterarMovtoTerceiro.p
objetivo: Alterar um movto de retorno na tabela componente.
autor: Tadeu silva
data:02/2024
********************************************************************/

DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
DEFINE INPUT  PARAMETER pQt    AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER cErro  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE dNovaQt        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dQtAnterior    AS DECIMAL     NO-UNDO.

FIND componente EXCLUSIVE-LOCK
    WHERE rowid(componente) = pRowid NO-ERROR.
FIND ITEM OF item-doc-est NO-LOCK NO-ERROR.
IF AVAIL componente THEN DO ON ERROR UNDO:
   ASSIGN dQtAnterior = componente.quantidade.
   FIND LAST saldo-terc 
      WHERE componente.serie-comp      = saldo-terc.serie-docto
      AND   componente.nro-comp        = saldo-terc.nro-docto
      AND   componente.nat-comp        = saldo-terc.nat-operacao
      AND   componente.it-codigo       = saldo-terc.it-codigo
      AND   componente.cod-refer       = saldo-terc.cod-refer
      AND   componente.seq-comp        = saldo-terc.sequencia
      AND   componente.cod-emitente    = saldo-terc.cod-emitente NO-LOCK NO-ERROR.
   IF NOT AVAIL saldo-terc THEN DO:
       ASSIGN cErro = "NÆo foi poss¡vel encontrar registro de terceiros".
       RETURN 'nok'.
   END.

   ASSIGN componente.dec-2         = componente.quantidade
          componente.log-2         = YES
          componente.char-2        = STRING(NOW,"99/99/9999 hh:mm:ss")
          .
   IF pQt < 0 THEN DO: //quantidade menor que zero quer reduzir o saldo e por conseguencia aumentar a quantidade do movto de retorno.
      ASSIGN componente.quantidade    = componente.quantidade + ABS(pQt)
             componente.qt-do-forn    = componente.quantidade .
      /*MESSAGE 'quantidade a ser acrescentada: ' pQt SKIP
          'novo valor do lancamento:' componente.quantidade
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
   END.
   ELSE DO:
      IF pQt > componente.quantidade THEN DO:
         ASSIGN cErro = "Quantidade a ser diminuida(" + STRING(pQt) + " ‚ maior que a quantidade do movto(" + STRING(componente.quantidade) + ")" .
         RETURN ERROR.
      END.


      ASSIGN  componente.quantidade       = componente.quantidade - pQt
              componente.qt-do-forn       = componente.quantidade
              .
      /*MESSAGE 'quantidade a ser diminuida: ' pQt SKIP
          'novo valor do lancamento:' componente.quantidade
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

   END.

  /* MESSAGE 'qt.componente anterior:' dQtAnterior                             SKIP
           'qt.componente atual:'   componente.quantidade                    SKIP
           'valor icms nao trib:' componente.icm-ntrib[1]
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
   IF componente.quantidade <> 0 THEN
   ASSIGN componente.icm-ntrib[1]         = componente.icm-ntrib[1]   / dQtAnterior   * componente.quantidade 
          componente.icm-ntrib[2]         = componente.icm-ntrib[2]   / dQtAnterior   * componente.quantidade
          componente.ipi-ntrib[1]         = componente.ipi-ntrib[1]   / dQtAnterior   * componente.quantidade
          componente.ipi-ntrib[2]         = componente.ipi-ntrib[2]   / dQtAnterior   * componente.quantidade
          componente.preco-total[1]       = componente.preco-total[1] / dQtAnterior   * componente.quantidade
          componente.preco-total[2]       = componente.preco-total[2] / dQtAnterior   * componente.quantidade
          componente.valor-ipi[1]         = componente.valor-ipi[1]
          componente.valor-ipi[2]         = componente.valor-ipi[2]
          componente.peso-liq             = componente.peso-liq       / dQtAnterior *  componente.quantidade
          .
   ELSE DO:
       ASSIGN componente.icm-ntrib[1]     = 0
          componente.icm-ntrib[2]         = 0
          componente.ipi-ntrib[1]         = 0
          componente.ipi-ntrib[2]         = 0
          componente.preco-total[1]       = 0
          componente.preco-total[2]       = 0
          componente.valor-ipi[1]         = 0
          componente.valor-ipi[2]         = 0
          componente.peso-liq             = 0.

   END.
   FIND CURRENT saldo-terc EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL saldo-terc THEN DO:
      ASSIGN saldo-terc.quantidade = saldo-terc.quantidade + dQtAnterior - componente.quantidade.
                
   END.
   ELSE DO:
       ASSIGN cErro = "NÆo foi poss¡vel encontrar registro de terceiros".
   END.
END.
