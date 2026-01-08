/**************************************************************************
Programa:esbo/boLisa11.p
Autor: Tadeu Silva 
Objetivo:Manter a tabela itens_pedido_lisa 
Data: 12/2023
Modificacoes:
26/03/2024 - tsp01 - foi necess†rio retirar a validaá∆o de registro £nico na inclus∆o
devido a lisa estar enviando multiplos registros com a chave
pedido_lisa_id,it_codigo,cod_refer,nf_origem,item_nf_origem devido a variaá∆o de endereáamento,
porÇm, o endereáamento n∆o vem na tabela de itens, apenas  na de etiqueta.
foi implementado na procedure inserir tambÇm o recurso de soma de quantidade para registros repetidos.

*****************************************************************************/

&SCOPED-DEFINE tabela  itens_pedido_lisa
&SCOPED-DEFINE cpId    item_pedido_lisa_id
&SCOPED-DEFINE cpChavePai pedido_Lisa_Id
&SCOPED-DEFINE exceptCps EXCEPT {&cpId} {&cpChavePai} 
&SCOPED-DEFINE CpsUnico   {&cpChavePai} ,it_codigo,cod_refer,nf_origem,ITEM_nf_origem
&SCOPED-DEFINE varChavePai pedidoLisaId



&SCOPED-DEFINE seqId   seq_item_pedido_lisa
&SCOPED-DEFINE ttReg   ttReg 
&SCOPED-DEFINE boMsg   HBoMsg

// essa include deve ficar sempre logo ap¢s  o scoped-defined
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
           /*
           tsp01
           RUN verifExist(OUTPUT lAChou).
           IF lAchou THEN DO:
              RUN setMsg IN {&boMsg}(2,'J† existe um registro com valores iguais para os campos:' + '{&CpsUnico}','erro' ).
              RETURN 'nok'.
           END.*/
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

    DEFINE INPUT PARAMETER TABLE FOR ttPedItemFat.
    //{esp/exportarTabelacsv3.i ttpeditemfat " " " " "ttpeditemfat_bolisa11" } 
    //fazer o de para e a inclus∆o
    FIND FIRST ttPedItemFat NO-ERROR.
    IF AVAIL ttPedItemFat THEN  DO:

       RUN criarReg(
           {&varChavePai},
           ttPedItemFat.itCodigo,         
           ttPedItemFat.codRefer,
           0,
           0,       
           0,      
           ttPedItemFat.nfOrigem,
           ttPedItemFat.idNfOrigem,       
           ttPedItemFat.serieNfOrigem,    
           ttPedItemFat.itemNfOrigem,     
           ttPedItemFat.nfRetorno,        
           ttPedItemFat.serieNfRetorno,   
           ttPedItemFat.qtFaturada
           ).
    END.

END PROCEDURE.

PROCEDURE criarReg:


    DEFINE INPUT  PARAMETER pPedidoLisaId   LIKE itens_pedido_lisa.pedido_lisa_id      NO-UNDO.
    DEFINE INPUT  PARAMETER pItCodigo       LIKE itens_pedido_lisa.it_codigo           NO-UNDO.
    DEFINE INPUT  PARAMETER pCodRefer       LIKE itens_pedido_lisa.cod_Refer           NO-UNDO.
    DEFINE INPUT  PARAMETER pQtSolicitada   LIKE itens_pedido_lisa.qt_solicitada       NO-UNDO.
    DEFINE INPUT  PARAMETER pQtSeparada     LIKE itens_pedido_lisa.qt_separada         NO-UNDO.
    DEFINE INPUT  PARAMETER pQtDiferenca    LIKE itens_pedido_lisa.qt_diferenca        NO-UNDO.
    DEFINE INPUT  PARAMETER pNfOrigem       LIKE itens_pedido_lisa.nf_origem           NO-UNDO.
    DEFINE INPUT  PARAMETER pIdNFOrigem     LIKE itens_pedido_lisa.id_nf_origem        NO-UNDO.
    DEFINE INPUT  PARAMETER pSerieNFOrigem  LIKE itens_pedido_lisa.serie_nf_origem     NO-UNDO.
    DEFINE INPUT  PARAMETER pItemNFOrigem   LIKE itens_pedido_lisa.item_nf_origem      NO-UNDO.
    DEFINE INPUT  PARAMETER pNFRetorno      LIKE itens_pedido_lisa.nf_retorno          NO-UNDO.
    DEFINE INPUT  PARAMETER pSerieNfRetorno LIKE itens_pedido_lisa.serie_nf_retorno    NO-UNDO.
    DEFINE INPUT  PARAMETER pQtFaturada     LIKE itens_pedido_lisa.qt_faturada         NO-UNDO.

    EMPTY TEMP-TABLE {&ttReg}.
    CREATE {&ttReg}.  
    ASSIGN {&ttReg}.pedido_lisa_id    =  pPedidoLisaId   
           {&ttReg}.it_codigo         =  pItCodigo       
           {&ttReg}.cod_refer         =  pCodRefer       
           /*{&ttReg}.qt_solicitada     =  pQtSolicitada   
           {&ttReg}.qt_separada       =  pQtSeparada     
           {&ttReg}.qt_diferenca      =  pQtDiferenca    */
           {&ttReg}.nf_origem         =  pNfOrigem       
           {&ttReg}.id_nf_origem      =  pIdNFOrigem     
           {&ttReg}.serie_nf_origem   =  pSerieNFOrigem  
           {&ttReg}.item_nf_origem    =  pItemNFOrigem   
           {&ttReg}.nf_retorno        =  pNFRetorno      
           {&ttReg}.serie_nf_retorno  =  pSerieNfRetorno 
           {&ttReg}.qt_faturada       =  pQtFaturada
          .



END PROCEDURE.


/*

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

