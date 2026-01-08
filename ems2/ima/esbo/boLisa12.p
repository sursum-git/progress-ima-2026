/**************************************************************************
Programa:esbo/boLisa12.p
Autor: Tadeu Silva 
Objetivo:Manter a tabela etq_item_pedido_lisa 
Data: 12/2023
Modificacoes:
*****************************************************************************/


&SCOPED-DEFINE tabela  etq_item_pedido_lisa
&SCOPED-DEFINE cpId    etq_item_pedido_lisa_id
&SCOPED-DEFINE cpChavePai item_Pedido_Lisa_Id
&SCOPED-DEFINE exceptCps EXCEPT {&cpId} {&cpChavePai}
&SCOPED-DEFINE CpsUnico  {&cpChavePai},it_codigo,cod_refer,rolo
&SCOPED-DEFINE varChavePai itemPedidoLisaId

&SCOPED-DEFINE seqId   seq_etq_item_pedido_lisa
&SCOPED-DEFINE ttReg   ttReg 
&SCOPED-DEFINE boMsg   HBoMsg

//essa include boPadraoCRUD.i deve ficar ap¢s o scoped-define, nunca antes                     
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
              RUN setMsg IN {&boMsg}(2,'J† existe um registro com valores iguais para os campos:' + '{&CpsUnico}','erro' ).
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

    DEFINE INPUT PARAMETER TABLE FOR ttPedItemEtq.
    {esp/exportarTabelacsv3.i ttPedItemEtq " " " " "  "ttPedItemEtqBoLisa12" }
    FIND ttPedItemEtq NO-ERROR.
    IF AVAIL ttPedItemEtq THEN DO:
       RUN criarReg(
         ttPedItemEtq.itCodigo,              
         ttPedItemEtq.codRefer,              
         ttPedItemEtq.rolo,
         ttPedItemEtq.idLisa,
         ttPedItemEtq.NrContainer,
         ttPedItemEtq.NrSeq,
         ttPedItemEtq.Data,
         ttPedItemEtq.Hora, 
         ttPedItemEtq.endereco,       
         ttPedItemEtq.Quantidade     
         ).                 
    END.

    

END PROCEDURE.

PROCEDURE criarReg:

    
    DEFINE INPUT  PARAMETER pItCodigo         LIKE  etq_item_pedido_lisa.it_codigo                NO-UNDO.
    DEFINE INPUT  PARAMETER pCodRefer         LIKE  etq_item_pedido_lisa.cod_refer                NO-UNDO.
    DEFINE INPUT  PARAMETER pRolo             LIKE  etq_item_pedido_lisa.rolo                     NO-UNDO.
    DEFINE INPUT  PARAMETER pIdLisa           LIKE  etq_item_pedido_lisa.id_lisa                  NO-UNDO.
    DEFINE INPUT  PARAMETER pNrContainer      LIKE  etq_item_pedido_lisa.nr_container             NO-UNDO.
    DEFINE INPUT  PARAMETER pNrSequencia      LIKE  etq_item_pedido_lisa.nr_sequencia             NO-UNDO.
    DEFINE INPUT  PARAMETER pData             LIKE  etq_item_pedido_lisa.data                     NO-UNDO.
    DEFINE INPUT  PARAMETER pHora             LIKE  etq_item_pedido_lisa.hora                     NO-UNDO.
    DEFINE INPUT  PARAMETER pEndereco         LIKE  etq_item_pedido_lisa.endereco                 NO-UNDO.
    DEFINE INPUT  PARAMETER pQuantidade       LIKE  etq_item_pedido_lisa.quantidade               NO-UNDO.

    EMPTY TEMP-TABLE {&ttReg}.
    CREATE {&ttReg}.  
    ASSIGN {&ttReg}.item_pedido_lisa_id     =  {&varChavePai}
           {&ttReg}.it_codigo               =  pItCodigo        
           {&ttReg}.cod_refer               =  pCodRefer        
           {&ttReg}.rolo                    =  pRolo
           {&ttReg}.id_lisa                 =  pIdLisa          
           {&ttReg}.nr_container            =  pNrContainer     
           {&ttReg}.nr_sequencia            =  pNrSequencia     
           {&ttReg}.data                    =  pData            
           {&ttReg}.hora                    =  pHora            
           {&ttReg}.endereco                =  pEndereco        
           {&ttReg}.quantidade              =  pQuantidade      
        .

    
END PROCEDURE.


