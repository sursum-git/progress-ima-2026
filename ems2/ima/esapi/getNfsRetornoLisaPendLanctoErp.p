/****************************************************************************
programa:esapi/getNfsRetornoLisaPendLanctoErp.p
objetivo: a partir dos Json's de retorno do endpoint 'notaretorno' verificar
quais notas fiscais ainda n∆o foram lanáadas no ERP.
autor: Tadeu Silva
data: 01/2024
***************************************************************************/

{esapi/getNfsRetornoLisaPendLanctoErp.i}
DEFINE INPUT  PARAMETER pNF        AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNrPedido  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pDtIniEmis AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER pDtFimEmis AS DATE        NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttNfPend .

DEFINE VARIABLE nfIni       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE nfFim       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE nrPedidoIni AS CHARACTER   NO-UNDO.
DEFINE VARIABLE nrPedidoFim AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cErro       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE h-acomp     AS HANDLE      NO-UNDO.

DEFINE VARIABLE rowidNf     AS ROWID       NO-UNDO.

IF pNf <> '' THEN
   ASSIGN nfIni = pNf
          nfFim = pNf.
ELSE
   ASSIGN nfIni = ''
          nfFim = 'zzzzzzzzzzz'.


IF pNrPedido <> '' THEN
   ASSIGN nrPedidoIni = pNrPedido
          nrPedidoFim = pNrPedido.
ELSE
   ASSIGN nrPedidoIni = ''
          nrPedidoFim = 'zzzzzzzzzzz'.

RUN utp/ut-acomp.p PERSIST SET h-acomp.
RUN pi-inicializar IN h-acomp('Buscando Retornos LISA').


FOR EACH retornos_lisa
    WHERE retornos_lisa.dt_nf        >= pDtIniEmis 
    AND   retornos_lisa.dt_nf        <= pDtFimEmis 
    AND   retornos_lisa.nr_nota_fis  >= nfIni
    AND   retornos_lisa.nr_nota_fis  <= nfFim
    AND   retornos_lisa.nr_pedido    >= nrPedidoIni
    AND   retornos_lisa.nr_pedido    <= nrPedidoFim 
    AND   retornos_lisa.num_situacao = 0 NO-LOCK:
    //DISP retornos_lisa.chave .
   RUN pi-acompanhar  IN h-acomp('NF:' + retornos_lisa.nr_nota_fis ).  
   FIND docum-est NO-LOCK
       WHERE docum-est.cod-chave-aces-nf-eletro = retornos_lisa.chave
       USE-INDEX chave-aces-nf NO-ERROR.
   IF NOT AVAIL docum-est OR (AVAIL docum-est AND  docum-est.ce-atu = NO )THEN DO:
      RUN pi-acompanhar  IN h-acomp('N∆o Encontrado Docto p/ NF:' + retornos_lisa.nr_nota_fis ).  
      


         
      CREATE ttNfPend.
      ASSIGN ttnfPend.codEstabel    = retornos_lisa.cod_estabel
             ttNfPend.serie         = retornos_lisa.serie
             ttnfPend.nrNotafis     = retornos_lisa.nr_nota_fis
             ttNfPend.nrPedido      = retornos_lisa.nr_Pedido
             ttNfPend.dtEmisNota    = retornos_lisa.dt_nf
             ttNfPend.chave         = retornos_lisa.chave  
             ttNfPend.logRe1001     = AVAIL docum-est
             ttNfPend.dtTransacao   = ttNfPend.dtEmisNota
             ttNfPend.prePedido     = retornos_lisa.pre_pedido
             ttNfPend.retornoLisaId = retornos_lisa.retorno_lisa_id
             .   

      FIND ped-venda NO-LOCK
          WHERE   ped-venda.nr-pedido   = int(retornos_lisa.nr_pedido) NO-ERROR.
      IF AVAIL ped-venda THEN DO:
         RUN esapi/getNFVendaPedido.p(ROWID(ped-venda),OUTPUT rowidNf, OUTPUT cErro).
         IF rowidNF = ? THEN
            ASSIGN ttNfPend.descrErro = "A nota fiscal de venda deste pedido foi cancelada ou n∆o existe"
                                      .
        
      END.
   END.
END.
RUN pi-finalizar IN h-acomp.








