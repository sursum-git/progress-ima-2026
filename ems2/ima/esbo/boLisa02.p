/**************************************************************************
Programa:esbo/boLisa02.p
Autor: Tadeu Silva 
Objetivo:Manter a tabela romaneios_retorno_lisa
Data: 03/2024
Modificacoes:
*****************************************************************************/

&SCOPED-DEFINE tabela  romaneios_retorno_lisa
&SCOPED-DEFINE cpId    romaneio_retorno_lisa_id
&SCOPED-DEFINE cpChavePai retorno_lisa_id
&SCOPED-DEFINE exceptCps EXCEPT {&cpId} {&cpChavePai}
&SCOPED-DEFINE CpsUnico  {&cpChavePai}, it_codigo, cod_refer, num_rolo 
&SCOPED-DEFINE varChavePai retornoLisaId

&SCOPED-DEFINE seqId   seq_romaneio_retorno_lisa
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


/*
PROCEDURE inclusaoExtracaoTTJson:

    DEFINE INPUT PARAMETER TABLE FOR ttRomaneio .
    //{esp/exportarTabelacsv3.i ttPedItemEtq " " " " "  "ttPedItemEtqBoLisa12" }
    FIND ttPedItemEtq NO-ERROR.
    IF AVAIL ttPedItemEtq THEN DO:
       RUN criarReg(
         ttRomaneio.it_Codigo,              
         ttRomaneio.cod_Refer,              
         ttRomaneio.num_rolo,
         ttRomaneio.id_etq_lisa,
         ttRomaneio.nr_seq_lisa,
         ttRomaneio.data,
         ttRomaneio.hora
         ).                 
    END.

    

END PROCEDURE.

PROCEDURE criarReg:

    
    DEFINE INPUT  PARAMETER pItCodigo         LIKE   {&ttReg}.it_codigo                             NO-UNDO.
    DEFINE INPUT  PARAMETER pCodRefer         LIKE   {&ttReg}.cod_refer                             NO-UNDO.
    DEFINE INPUT  PARAMETER pNumRolo          LIKE   {&ttReg}.num_rolo                              NO-UNDO.
    DEFINE INPUT  PARAMETER pIdEtqLisa        LIKE   {&ttReg}.id_etq_lisa                           NO-UNDO.
    DEFINE INPUT  PARAMETER pNrSeqLisa        LIKE   {&ttReg}.nr_seq_lisa                           NO-UNDO.
    DEFINE INPUT  PARAMETER pData             LIKE   {&ttReg}.data                                  NO-UNDO.
    DEFINE INPUT  PARAMETER pHora             LIKE   {&ttReg}.hora                                  NO-UNDO.
                                                     

    EMPTY TEMP-TABLE {&ttReg}.
    CREATE {&ttReg}.  
    ASSIGN {&ttReg}.retorno_lisa_id      = {&cpChavePai}
           {&ttReg}.it_codigo            =  pItCodigo        
           {&ttReg}.cod_refer            =  pCodRefer   
           {&ttReg}.num_rolo             =  pNumRolo    
           {&ttReg}.id_etq_lisa          =  pIdEtqLisa  
           {&ttReg}.nr_seq_lisa          =  pNrSeqLisa  
           {&ttReg}.data                 =  pData       
           {&ttReg}.hora                 =  pHora .

    
END PROCEDURE.
*/

/*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 romaneio_retorno_lisa_id         inte        i
   20 retorno_lisa_id                  inte        i
   30 it_codigo                        char        i
   40 cod_refer                        char        i
   50 num_rolo                         inte        i
   60 id_etq_lisa                      char        i
   70 nr_seq_lisa                      inte        i
   80 data                             date        i
   90 hora                             char        i
*/
