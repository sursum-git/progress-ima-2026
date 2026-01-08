{esapi/getNfsRetornoLisaPendLanctoErp.i}
{esp/util.i}
{esapi/integrarRetornoERp.i}
DEFINE INPUT PARAMETER TABLE FOR ttNfPend.
DEFINE INPUT PARAMETER TABLE FOR ttItemNf.
DEFINE OUTPUT  PARAMETER lErro      AS LOGICAL     NO-UNDO.
DEFINE INPUT  PARAMETER pAtuRe1001  AS LOGICAL     NO-UNDO.
//DEFINE OUTPUT PARAMETER cArqErro AS CHARACTER   NO-UNDO.

/* Variaveis do Recebimento Fiscal ---                                  */
DEF TEMP-TABLE tt-docum-est NO-UNDO LIKE docum-est 
    FIELD r-rowid AS ROWID.
/*DEF TEMP-TABLE tt-item-doc-est NO-UNDO LIKE item-doc-est
    FIELD r-rowid AS ROWID.*/

DEF TEMP-TABLE tt-item-terc NO-UNDO
    FIELD rw-saldo-terc   AS ROWID
    FIELD quantidade      LIKE saldo-terc.quantidade
    FIELD preco-total     LIKE componente.preco-total EXTENT 0
    FIELD desconto        LIKE componente.desconto    EXTENT 0    
    FIELD cod-depos       LIKE saldo-terc.cod-depos
    FIELD nr-ord-prod     LIKE saldo-terc.nr-ord-prod
    FIELD nat-of          LIKE item-doc-est.nat-of.

DEFINE VARIABLE dQtTotal        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dVlTotal        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dQtRet          AS DECIMAL     NO-UNDO.
DEFINE VARIABLE qt-saldo-disp   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cErroCor        AS CHARACTER   NO-UNDO.

DEFINE VARIABLE nfOri           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE refOri          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iSeq            AS INTEGER     NO-UNDO.

DEFINE VARIABLE rowidSaldoTerc  AS ROWID       NO-UNDO.


DEFINE VARIABLE cSeriePadrao    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNatOperPadrao  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE codEmitPadrao   AS INTEGER     NO-UNDO.


DEFINE VARIABLE hBoMsg AS HANDLE      NO-UNDO.

RUN esbo/boMsg.p PERSIST SET hBomsg.

{esp/exportarTabelacsv3.i ttNfPend " " " " "  "ttNfPendIntegrarRetornoERP" }
{esp/exportarTabelacsv3.i ttitemNf " " " " "  "ttitemNFIntegrarRetornoERP" }

RUN esapi/getVarsSaldoTercLisa.p(OUTPUT cSeriePadrao,OUTPUT cNatOperPadrao,OUTPUT codEmitPadrao).
  
OUTPUT TO VALUE(SESSION:TEMP-DIRECT + 'log_integracao_retorno_erro.csv').
FOR EACH ttNfPend .
    PUT UNFORM "Inicio PEDIDO:"  ttNfPend.nrPedido SKIP.
    FIND pedidos_lisa
        WHERE string(pedidos_lisa.nr_pedido) = ttNfPend.nrPedido NO-LOCK NO-ERROR.
    IF NOT AVAIL pedidos_lisa THEN DO:
       PUT UNFORM "nÆo achou o pedido lisa e encerrou o processo de integra‡Æo" SKIP.
       NEXT.
    END.
    EMPTY TEMP-TABLE tt-docum-est.
    EMPTY TEMP-TABLE tt-item-terc.
    PUT UNFORM "Achou o pedido lisa e vai fazer a busca nos itens..." SKIP.
    FOR EACH itens_pedido_lisa 
        WHERE pedidos_lisa.pedido_lisa_id = itens_pedido_lisa.pedido_lisa_id :        
        FOR EACH ttitemNf 
            WHERE ttitemNf.itemPedidoLisaId = itens_pedido_lisa.ITEM_pedido_lisa_id
            AND   ttItemNf.qtFaturada > 0.
            
           ASSIGN dQtRet = IF ttITemNf.qtRetorno >= ttItemNf.qtFaturada OR ttItemNf.qtRetorno = 0 THEN ttItemNf.qtFaturada ELSE ttITemNf.qtRetorno .
           
           
           IF ttItemNf.nfSubstituta <> '' THEN
              ASSIGN nfOri  = ttItemNf.nfSubstituta.
           ELSE
              //ASSIGN nfOri  = string(int(itens_pedido_lisa.nf_origem),'9999999').
              ASSIGN nfOri  = string(int(ttitemNf.nfOriginal),'9999999').
           IF ttItemNf.codReferSub <> '' THEN
              ASSIGN  refOri = ttItemNf.codReferSub .
           ELSE
              ASSIGN  refOri = itens_pedido_lisa.cod_refer .

           ASSIGN iSeq = ttItemNf.sequencia.

           
           .

           RUN esapi/getSaldoTercDoctoLisa2.p(                                                                                                                                           
                 nfOri,                                                                                                                                                                     
                 itens_pedido_lisa.it_codigo,                                                                                                                                               
                 iSeq,                                                                                                                                                                      
                 OUTPUT qt-Saldo-Disp,                                                                                                                                                      
                 OUTPUT rowidSaldoTerc ).
                 
                 
                 
           PUT "ID:"  ttItemNf.id
               "Item:" ttItemNf.itCodigo 
               "ref:" ttItemNf.codRefer
               "qt:" dQtRet 
               "nf:" nfOri
               "seq" iSeq
               "ref:" refOri
               "qt.disp:" qt-saldo-disp
               "qt.faturada:"  ttItemNf.qtFaturada
               SKIP                    
               .
                 
                                                                                                                                                                                            
           IF qt-saldo-disp > 0 THEN DO:                                                                                                                                                  
              IF qt-saldo-disp  < dQtRet THEN DO:                                                                                                                                         
                 ASSIGN cErroCor = "Item:" + itens_pedido_lisa.it_codigo + " - Seq.:" + STRING(iSeq) + "- Saldo do documento:" + nfOri + "(" + STRING(qt-saldo-disp) + ")"                
                               + "menor que a quantidade a ser baixada:" + STRING(dQtRet).                                                                                                
                 RUN setMsg IN hboMsg(1,cErroCor,'erro').                                                                                                                                 
                                                                                                                                                                                          
              END.                                                                                                                                                                        
              FIND saldo-terc NO-LOCK                                                                                                                                                     
                  WHERE rowid(saldo-terc) = rowidSaldoTerc NO-ERROR.                                                                                                                      
              FIND FIRST componente OF saldo-terc WHERE componente.componente = 1 NO-LOCK NO-ERROR.                                                                                       
              IF NOT AVAIL componente THEN DO:                                                                                                                                            
                 ASSIGN cErroCor = "Item:" + itens_pedido_lisa.it_codigo + " - Seq.:" + STRING(iseq) + "- Saldo do documento:" + nfOri + "- NAO ENCONTRADO COMPONENTE DE ENVIO".          
                 RUN setMsg IN hboMsg(1,cErroCor,'erro').                                                                                                                                 
                                                                                                                                                                                          
              END.                                                                                                                                                                        
              ELSE DO:                                                                                                                                                                    
                  CREATE tt-item-terc.                                                                                                                                                    
                  ASSIGN tt-item-terc.rw-saldo-terc = rowidSaldoTerc                                                                                                                      
                         tt-item-terc.quantidade    = dQtRet                                                                                                                              
                         tt-item-terc.preco-total   = componente.preco-total[1] / componente.quantidade  * dQtRet                                                                         
                         tt-item-terc.desconto      = 0                                                                                                                                   
                         tt-item-terc.cod-depos     = 'ARM'                                                                                                                               
                         dQtTotal                   = dQtTotal + tt-item-terc.quantidade                                                                                                  
                         dVlTotal                   = dVlTotal + tt-item-terc.preco-total .                                                                                               
                                                                                                                                                                                          
              END.                                                                                                                                                                        
                                                                                                                                                                                          
           END.                                                                                                                                                                           
           ELSE DO:                                                                                                                                                                       
               ASSIGN cErroCor = "Item:" + itens_pedido_lisa.it_codigo + " - Seq.:" + string(iSeq) +  " - Saldo do documento NÆo Encontrado:" + nfOri .                                   
               RUN setMsg IN hboMsg(1,cErroCor,'erro').                                                                                                                                   
           END.
        END.
    END.
    RUN getErro IN hBoMsg(OUTPUT lErro).
    IF lErro THEN DO:
       RUN expttMsg IN hboMsg('c:\temp\erros_integracao_retorno').
       RETURN 'nok'.
    END.

    CREATE tt-docum-est.
    ASSIGN tt-docum-est.cod-estabel                 = '505'
           tt-docum-est.cod-emitente                = codEmitPadrao
           tt-docum-est.serie-docto                 = cSeriePadrao
           tt-docum-est.nro-docto                   = string(ttNfPend.nrnotaFis,'9999999')
           tt-docum-est.nat-operacao                = '19207i' 
           tt-docum-est.dt-emissao                  = ttNfPend.dtEmis
           tt-docum-est.dt-trans                    = ttNfPend.dtTransacao
           tt-docum-est.cod-chave-aces-nf-eletro    = ttNfPend.chave
          .

    ASSIGN tt-docum-est.tot-valor                   = dVlTotal 
           tt-docum-est.tot-peso                    = dQtTotal
           tt-docum-est.valor-mercad                = dVlTotal
           tt-docum-est.base-icm                    = dVlTotal  
           tt-docum-est.base-ipi                    = dVlTotal 
           tt-docum-est.despesa-nota                = 0
           .  
    RUN esapi/cria-nota-re1001-ret.p(INPUT TABLE tt-docum-est,INPUT TABLE tt-item-terc).
    FIND docum-est NO-LOCK
       WHERE docum-est.cod-estabel  = tt-docum-est.cod-estabel                
       AND   docum-est.cod-emitente = tt-docum-est.cod-emitente               
       AND   docum-est.serie-docto  = tt-docum-est.serie-docto                
       AND   docum-est.nro-docto    = tt-docum-est.nro-docto                  
       AND   docum-est.nat-operacao = tt-docum-est.nat-operacao               
       NO-ERROR.
    IF AVAIL docum-est THEN DO:
      IF NOT CAN-FIND(FIRST item-doc-est OF docum-est) THEN DO:
         FIND CURRENT docum-est EXCLUSIVE-LOCK.
         DELETE docum-est.
         RETURN  "nok":u.
      END.
    END.    

    IF pAtuRe1001 THEN
      RUN esapi/atu-nota-re1001.p(INPUT TABLE tt-docum-est).
    ASSIGN lErro = RETURN-VALUE  = 'adm-error'.
    
END.


/*
Pedido Lisa
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
    1 pedido_lisa_id                   inte        i
    9 cod_estabel                      char        i
   10 nr_pedido                        inte        i
   20 pre_pedido                       inte        i
   30 log_integracao_pendente          logi        i
   40 cod_sit_pedido                   char
   50 dt_inclusao                      date
   60 hr_inclusao                      char
   70 obs                              char
   80 obs_separacao                    char
   90 log_nfe_enviada                  logi
  100 log_sep_enviada_api              logi
  110 pedido_lisa                      inte
  120 dt_expedicao                     date
  130 hora_expedicao                   char
  140 dt_romaneio                      date
  150 nf_cliente                       char        i
  160 peso_bruto                       deci-2
  170 peso_liquido                     deci-2
  180 qt_caixa                         inte
  190 cod_romaneio                     char
  200 cod_sit_pedido_lisa              char        i

                
                
                
itens pedido lisa
Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 item_pedido_lisa_id              int6        i
   11 pedido_lisa_id                   int6        i
   20 it_codigo                        char        i
   30 cod_refer                        char        i
   40 qt_solicitada                    deci-2
   50 qt_separada                      deci-2
   60 qt_diferenca                     deci-2
   70 nf_origem                        char        i
   80 id_nf_origem                     char
   90 serie_nf_origem                  char        i
  100 item_nf_origem                   char
  110 nf_retorno                       char        i
  120 serie_nf_retorno                 char        i
  130 qt_faturada                      deci-2

*/











//28284
//19207i
//2
//505
//arm

