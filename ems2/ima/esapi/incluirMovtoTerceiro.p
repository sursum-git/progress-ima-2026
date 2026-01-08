/*******************************************************************
programa:esapi/incluirMovtoTerceiro.p
objetivo: Incluir um movto de retorno na tabela componente.
autor: Tadeu silva
data:02/2024
********************************************************************/

DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
DEFINE INPUT  PARAMETER pQt    AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER pData  AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER pSerie AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNat   AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cErro  AS CHARACTER   NO-UNDO.

IF pNat = '' THEN
   ASSIGN pNat = '19207i'.


FIND LAST saldo-terc NO-LOCK
    WHERE rowid(saldo-terc) = pRowid NO-ERROR.
IF AVAIL saldo-terc THEN DO TRANSACTION ON ERROR UNDO:
   CREATE componente.
   ASSIGN componente.serie-comp      = saldo-terc.serie-docto   
          componente.nro-comp        = saldo-terc.nro-docto     
          componente.nat-comp        = saldo-terc.nat-operacao  
          componente.it-codigo       = saldo-terc.it-codigo     
          componente.cod-refer       = saldo-terc.cod-refer     
          componente.seq-comp        = saldo-terc.sequencia     
          componente.cod-emitente    = saldo-terc.cod-emitente .

   ASSIGN componente.nro-docto       = '999999'
          componente.serie-docto     =  pSerie
          componente.sequencia       = 0
          componente.nat-operacao    = pNat.

   FIND it-nota-fisc NO-LOCK
       WHERE  it-nota-fisc.serie            = saldo-terc.serie-docto
       AND    it-nota-fisc.nr-nota-fis      = saldo-terc.nro-docto
       //AND    it-nota-fisc.cod-emitente     = saldo-terc.cod-emitente
       AND    it-nota-fisc.nat-operacao     = saldo-terc.nat-operacao
       AND    it-nota-fisc.nr-seq-fat       = saldo-terc.sequencia
       NO-ERROR.

   IF AVAIL it-nota-fisc THEN DO:
      FIND ITEM OF it-nota-fisc NO-LOCK NO-ERROR.

     /* MESSAGE 'qt.dentro da inclusao:' pQt
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
      ASSIGN componente.cd-trib-icm          = it-nota-fisc.cd-trib-icm
             componente.cd-trib-iss          = it-nota-fisc.cd-trib-iss
             componente.cd-trib-ipi          = it-nota-fisc.cd-trib-ipi
             componente.aliquota-icm         = it-nota-fisc.aliquota-icm
             componente.aliquota-ipi         = it-nota-fisc.aliquota-ipi
             componente.aliquota-iss         = it-nota-fisc.aliquota-iss
             componente.class-fiscal         = it-nota-fisc.class-fiscal
             componente.peso-liq             = round(pQt * ITEM.peso-liq,2)
             componente.quantidade           = pQt
             componente.qt-do-forn           = pQt
             componente.un                   = it-nota-fisc.un[1]
             componente.icm-ntrib[1]         = it-nota-fisc.vl-icmsnt-it / it-nota-fisc.qt-faturada[1] * pQt
             componente.icm-ntrib[2]         = it-nota-fisc.vl-icmsnt-it / it-nota-fisc.qt-faturada[1] * pQt
             componente.ipi-ntrib[1]         = it-nota-fisc.vl-ipint-it  / it-nota-fisc.qt-faturada[1] * pQt
             componente.ipi-ntrib[2]         = it-nota-fisc.vl-ipint-it  / it-nota-fisc.qt-faturada[1] * pQt
             componente.preco-total[1]       = it-nota-fisc.vl-tot-item  / it-nota-fisc.qt-faturada[1] * pQt
             componente.preco-total[2]       = it-nota-fisc.vl-tot-item  / it-nota-fisc.qt-faturada[1] * pQt
             componente.cod-depos            = it-nota-fisc.cod-depos
             componente.lote                 = it-nota-fisc.cod-refer
             componente.ct-codigo            = '19000017'
             componente.componente           = 2
             componente.dt-retorno           = pData
             componente.atualiza-pa          = YES
             componente.narrativa            = ''
             componente.valor-ipi[1]         = it-nota-fisc.vl-ipi-it / it-nota-fisc.qt-faturada[1] * pQt
             componente.valor-ipi[2]         = it-nota-fisc.vl-ipi-it / it-nota-fisc.qt-faturada[1] * pQt
             componente.baixa-ce             = NO
             componente.origem               = 3
             componente.ind-tipo-valor       = 1
             componente.tipo-valor           = 1
             .

   END.
   ELSE DO:
      ASSIGN cErro = 'Nota de Remessa n∆o encontrada no saldo de terceiros do ERP'.
      RETURN 'nok'.
   END.

   FIND CURRENT saldo-terc EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL saldo-terc THEN DO:
      ASSIGN saldo-terc.quantidade = saldo-terc.quantidade - pQt .

   END.
   FIND CURRENT saldo-terc NO-LOCK NO-ERROR.

END.

