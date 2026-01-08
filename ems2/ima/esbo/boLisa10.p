/**************************************************************************
Programa:esbo/boLisa10.p
Autor: Tadeu Silva 
Objetivo:Manter a tabela pedidos_lisa 
Data: 12/2023
Modificacoes:
03/2024 tadeu - melhorias nas variaveis do programa
*****************************************************************************/

&SCOPED-DEFINE tabela  pedidos_lisa
&SCOPED-DEFINE cpId    pedido_lisa_id
&SCOPED-DEFINE exceptCps EXCEPT {&cpId}
&SCOPED-DEFINE CpsUnico  cod_estabel,nr_pedido
&SCOPED-DEFINE cpChavePai  
&SCOPED-DEFINE varChavePai  

&SCOPED-DEFINE seqId   seq_pedido_lisa
&SCOPED-DEFINE ttReg   ttReg 
&SCOPED-DEFINE boMsg   HBoMsg


//essa include boPadraoCRUD.i deve ficar logo ap¢s o scoped-define
{esbo/boPadraoCRUD.i}

{esp/util.i}
{esp/setProp.i  {&ttReg} }
{lisa/extrairTtJsonPrePedido.i}

PROCEDURE validacoes:

   DEFINE INPUT  PARAMETER pAcao AS CHARACTER   NO-UNDO. //incluir,alterar,excluir,comum
   DEFINE VARIABLE lAchou AS LOGICAL     NO-UNDO.

   FIND FIRST {&ttReg} NO-ERROR.

   RUN limparTTMsg IN {&boMsg}.
   
   CASE pAcao:
       WHEN 'incluir' THEN DO:
           RUN verifExist(OUTPUT lAChou).
           IF lAchou THEN DO:
              RUN setMsg IN {&boMsg}(2,'J† existe um registro com valores iguais para os campos:' + '{&CpsUnico}','erro').
              RETURN 'nok'.
           END.
       END.
       WHEN 'alterar' THEN DO:

       END.
       WHEN 'excluir' THEN DO:

       END.
       WHEN 'comum' THEN DO:
         IF NOT CAN-FIND(FIRST {&ttReg}) THEN DO:
            RUN setMsg IN {&bomsg}(1,'Tabela Tempor†ria n∆o preenchida','erro').
            RETURN 'nok'.
         END.
       END.


   END CASE.

END PROCEDURE.


PROCEDURE inclusaoExtracaoTTJson:

    DEFINE INPUT PARAMETER TABLE FOR ttPedido.

    //fazer o de para e a inclus∆o
    FIND FIRST ttPedido NO-ERROR.
    IF AVAIL ttPedido THEN DO:
       RUN criarReg(
            ttPedido.codEstabel,
            ttPedido.PedidoCliente,            
            ttPedido.PrePedido,
            YES,
            string(ttPedido.Situacao),
            ttPedido.dtInclusao,       
            ttPedido.horaInclusao,          
            ttPedido.Obs,
            ttPedido.ObsSeparacao,
            ttPedido.logNfeEnviada,
            ttPedido.logEnviadoAPi,     
            ttPedido.pedidoLisa,
            ttPedido.dtExpedido,          
            ttPedido.horaExpedido,
            ttPedido.dtRomaneio,
            ttPedido.nfCliente,            
            ttPedido.PesoBruto,
            ttPedido.PesoLiquido,
            ttPedido.QtCaixa,
            ttPedido.codRomaneio,
            string(ttPedido.situacaoPed)
           ).
    END.     

END PROCEDURE.

PROCEDURE criarReg:

    DEFINE INPUT  PARAMETER pCodEstabel             LIKE pedidos_lisa.cod_Estabel           NO-UNDO.
    DEFINE INPUT  PARAMETER pNrPedido               LIKE pedidos_lisa.nr_pedido             NO-UNDO.
    DEFINE INPUT  PARAMETER pPrePredido             LIKE pedidos_lisa.pre_pedido            NO-UNDO.
    DEFINE INPUT  PARAMETER pLogIntegracaoPendente  AS LOGICAL     NO-UNDO.                 
    DEFINE INPUT  PARAMETER pCodSitPedido           LIKE pedidos_lisa.cod_sit_pedido        NO-UNDO.
    DEFINE INPUT  PARAMETER pDtInclusao             AS DATE        NO-UNDO.                 
    DEFINE INPUT  PARAMETER pHrInclusao             LIKE pedidos_lisa.hr_inclusao           NO-UNDO.
    DEFINE INPUT  PARAMETER pObs                    LIKE pedidos_lisa.obs                   NO-UNDO.
    DEFINE INPUT  PARAMETER pObsSeparacao           LIKE pedidos_lisa.obs_separacao         NO-UNDO.
    DEFINE INPUT  PARAMETER pLogNfeEnviada          AS LOGICAL     NO-UNDO.                 
    DEFINE INPUT  PARAMETER pLogSepEnviadaApi       AS LOGICAL     NO-UNDO.                 
    DEFINE INPUT  PARAMETER pPedidoLisa             LIKE pedidos_lisa.pedido_lisa           NO-UNDO.
    DEFINE INPUT  PARAMETER pDtExpedicao            AS DATE        NO-UNDO.                 
    DEFINE INPUT  PARAMETER pHoraExpedicao          LIKE pedidos_lisa.hora_expedicao        NO-UNDO.
    DEFINE INPUT  PARAMETER pDtRomaneio             AS DATE        NO-UNDO.                 
    DEFINE INPUT  PARAMETER pNfCliente              LIKE pedidos_lisa.nf_cliente            NO-UNDO.
    DEFINE INPUT  PARAMETER pPesoBruto              LIKE pedidos_lisa.peso_bruto            NO-UNDO.
    DEFINE INPUT  PARAMETER pPesoLiquido            LIKE pedidos_lisa.peso_liquido          NO-UNDO.
    DEFINE INPUT  PARAMETER pQtCaixa                LIKE pedidos_lisa.qt_caixa              NO-UNDO.
    DEFINE INPUT  PARAMETER pCodRomaneio            LIKE pedidos_lisa.cod_romaneio          NO-UNDO.
    DEFINE INPUT  PARAMETER pCodSitPedidoLisa       LIKE pedidos_lisa.cod_sit_pedido_lisa   NO-UNDO.

          
    EMPTY TEMP-TABLE {&ttReg}.

    CREATE {&ttReg}.                   
    ASSIGN {&ttReg}.cod_estabel                    =     pCodEstabel             
           {&ttReg}.nr_pedido                      =     pNrPedido               
           {&ttReg}.pre_pedido                     =     pPrePredido             
           {&ttReg}.LOG_integracao_pendente        =     pLogIntegracaoPendente  
           {&ttReg}.cod_sit_pedido                 =     pCodSitPedido           
           {&ttReg}.dt_inclusao                    =     pDtInclusao             
           {&ttReg}.hr_inclusao                    =     pHrInclusao             
           {&ttReg}.obs                            =     pObs                    
           {&ttReg}.obs_separacao                  =     pObsSeparacao           
           {&ttReg}.LOG_nfe_enviada                =     pLogNfeEnviada          
           {&ttReg}.LOG_sep_enviada_api            =     pLogSepEnviadaApi       
           {&ttReg}.pedido_lisa                    =     pPedidoLisa             
           {&ttReg}.dt_expedicao                   =     pDtExpedicao            
           {&ttReg}.hora_expedicao                 =     pHoraExpedicao          
           {&ttReg}.dt_romaneio                    =     pDtRomaneio             
           {&ttReg}.nf_cliente                     =     pNfCliente              
           {&ttReg}.peso_bruto                     =     pPesoBruto              
           {&ttReg}.peso_liquido                   =     pPesoLiquido            
           {&ttReg}.qt_caixa                       =     pQtCaixa                
           {&ttReg}.cod_romaneio                   =     pCodRomaneio            
           {&ttReg}.cod_sit_pedido_lisa            =     pCodSitPedidoLisa       
           .




END PROCEDURE.


