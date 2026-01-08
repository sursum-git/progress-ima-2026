/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i BC9008D 2.00.00.005}  /*** 010005 ***/
/***********************************************************************************
** Copyright DATASUL S.A. (2000)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
**********************************************************************************/
/**********************************************************************************
**     Programa...: bc9008d.p  
**
**     Finalidade.: Adapter do aviso de embarque do EMS2
**
**     Criado em..: Setembro 2000
**
**     Versao.....: 2.00.00.000 Luciano Leonhardt
**********************************************************************************/
{include/i_dbvers.i}
{bcp/bc9102.i}   /* Definicao da temp-table de erros do coleta de dados          */
{bcp/bc9008.i}
{bcp/bcx1000.i}

Define Input        Parameter pNumTrans    as integer No-undo.
Define Input        Parameter pRawConteudo as raw     No-undo.
Define Input-Output Parameter Table        For tt-erro.

DEF VAR h_bcx1000 AS HANDLE             NO-UNDO.

.

DEF TEMP-TABLE tt1-it-pre-fat NO-UNDO LIKE it-pre-fat
    INDEX ch-pedido IS PRIMARY nr-embarque 
                                nr-resumo 
                                nome-abrev 
                                nr-pedcli 
                                nr-sequencia 
                                it-codigo 
                                cod-refer 
                                nr-entrega.

DEF BUFFER B-BC-ETIQUETA FOR BC-ETIQUETA.

CREATE tt-embarque-bc.
RAW-TRANSFER prawconteudo TO tt-embarque-bc.

Find FIRST  B-BC-ETIQUETA
     Where  B-BC-ETIQUETA.PROGRESSIVO = tt-embarque-bc.chave-unica
     NO-LOCK No-Error.

IF NOT AVAIL B-BC-ETIQUETA 
THEN DO:
  
   CREATE TT-ERRO.
   ASSIGN TT-ERRO.CD-ERRO  = 0
          TT-ERRO.MENSAGEM = "ETIQUETA INFORMADA NAO EXISTE" 
                             + "(PROG:" + tt-embarque-bc.chave-unica + ")".          

   RETURN "NOK".

END.                                    

RUN bcp/bcx1000.p PERSISTENT SET h_bcx1000.

RUN pi-busca-it-pre-fat IN h_bcx1000 (tt-embarque-bc.num-aviso-embarque,
                                      tt-embarque-bc.nr-resumo,
                                      tt-embarque-bc.nr-resumo,
                                      tt-embarque-bc.cod-item,
                                      tt-embarque-bc.cod-refer,
                                      tt-embarque-bc.cod-lote,
                                      tt-embarque-bc.cod-local,
                                      OUTPUT TABLE tt-it-pre-fat).
DELETE OBJECT h_bcx1000.
        
FIND FIRST tt-it-pre-fat
     NO-LOCK NO-ERROR.

IF NOT AVAIL TT-IT-PRE-FAT
THEN DO:

    CREATE TT-ERRO.
    ASSIGN TT-ERRO.CD-ERRO  = 0
           TT-ERRO.MENSAGEM = "NAO ENCONTRADO ITEM DO EMBARQUE PARA ATUALIZACAO  ITEM:" + 
            TT-EMBARQUE-BC.COD-ITEM + "  REF: " + TT-EMBARQUE-BC.COD-REFER + " (BCX0003AC)"
                              .              
    RETURN "NOK".
    
END.

FOR EACH tt-it-pre-fat:
    
    FIND BC-ETIQUETA
         WHERE ROWID(BC-ETIQUETA) = ROWID(b-BC-ETIQUETA)
         share-LOCK NO-ERROR.

    FIND bc-trans 
         WHERE bc-trans.nr-trans = pNumTrans
         NO-LOCK NO-ERROR.

    ASSIGN 
           BC-ETIQUETA.nr-embarque       = TT-IT-PRE-FAT.NR-EMBARQUE  
           BC-ETIQUETA.NR-RESUMO         = TT-IT-PRE-FAT.NR-RESUMO    
              
           BC-ETIQUETA.NR-SEQ-FAT        = TT-IT-PRE-FAT.NR-SEQUENCIA 
           BC-ETIQUETA.NOME-ABREV        = TT-IT-PRE-FAT.NOME-ABREV   
           BC-ETIQUETA.NR-PEDCLI         = TT-IT-PRE-FAT.NR-PEDCLI    
           
           BC-ETIQUETA.NR-ENTREGA        = TT-IT-PRE-FAT.NR-ENTREGA   
        
           BC-ETIQUETA.COD-ESTADO        = 3 /* etiqueta SEPARADA */

           BC-ETIQUETA.DT-SEPARACAO      = TT-EMBARQUE-BC.DATA-TRANSACAO
           BC-ETIQUETA.HR-SEPARACAO      = tt-embarque-bc.cod-livre-1 
           BC-ETIQUETA.usuar-separacao   = tt-embarque-bc.cod-usuario

           BC-ETIQUETA.NR-TRANS          = BC-TRANS.NR-TRANS
           BC-ETIQUETA.CD-TRANS          = BC-TRANS.CD-TRANS
           .

END.

/* alocacao com sucesso efetivar a etiqueta */
 
RETURN "OK".
