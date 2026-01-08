/*******************************************************************************************
programa:esapi/extrairDadosNFRetorno.p
objetivo: Extrair os dados do json enviado pela LISA para posterior importaá∆o no ERP
autor: Tadeu Silva
data: N∆o informada
modificaá‰es:
tsp001 - 03/07/2025 - mudanáa do tratamento do campo de XML devido a modificaá∆o do formato do
json enviado. O problema comeáou a ocorrer no màs 06/20225

*********************************************************************************************/


{esapi/analisarJsonObject2.i}


{esapi/extrairDadosNFRetorno.i}

{esp/util.i}
{esp/params.i}
{lisa/codProdUnif.i}
DEFINE INPUT PARAMETER TABLE FOR ttJson .
//DEFINE INPUT  PARAMETER pIdTrans AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER logJaProcessado AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR  ttRomaneioErro .
//DEFINE OUTPUT PARAMETER cErro           AS CHARACTER   NO-UNDO.


DEFINE TEMP-TABLE ttReg NO-UNDO LIKE retornos_lisa .
DEFINE TEMP-TABLE ttRomaneio     LIKE romaneios_retorno_lisa .
DEFINE TEMP-TABLE ttRomAux       LIKE ttRomaneio.



DEFINE VARIABLE lcXML       AS LONGCHAR NO-UNDO.

DEFINE VARIABLE chavesRef       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE chaveNfe        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCont           AS INTEGER     NO-UNDO.
DEFINE VARIABLE cTipo           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cArquivo        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDataEmis       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cErroProgress   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE vlNF            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE retOk           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE idRetorno       AS INTEGER     NO-UNDO.

//DEFINE VARIABLE hboMsg          AS HANDLE      NO-UNDO.
CREATE ttReg.
ASSIGN ttReg.nr_pedido      = getChaveTTJson('payload', 'pedidoCliente')
       ttReg.pre_pedido     = int(getChaveTTJson('payload', 'prePedido'))
       ttReg.dt_hr_recbto   = DATETIME(getChaveTTJson('', 'dt_hr_recbto'))
       ttReg.arquivo_json   = getChaveTTJson('', 'nome_arquivo')
       .
       
IF ttReg.nr_Pedido = '' THEN
DO:  ASSIGN 
       ttReg.nr_pedido      = getChaveTTJson('items', 'pedidoCliente')
       ttReg.pre_pedido     = int(getChaveTTJson('items', 'prePedido')).
    
END.
       
       
       
       
       
       
/*RUN esp/instProg.p('esbo/boMsg.p'        , OUTPUT hBoMsg).
RUN setMsg IN hBoMsg(1,'Nr_pedido='    + ttReg.Nr_pedido,'log').
RUN setMsg IN hBoMsg(1,'Pre_pedido='   + string(ttReg.pre_pedido),'log').
RUN setMsg IN hBoMsg(1,'dt_hr_recbto=' + STRING(dt_hr_recbto,"99/99/9999 hh:mm:ss"),'log').
*/
IF ttReg.nr_pedido = "" THEN DO:
   
   RETURN "NOK".

END.
  
ASSIGN lcXML = getChaveTTJson('payload', 'xml').
/*MESSAGE STRING(lcXML)
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

    
//tsp001 -inicio
IF lcXml = 'jsonarray' THEN DO:
   ASSIGN lcXML = getChaveTTJson('xml', 'xml').
END.   
//tsp001 -fim
    
    
IF lcXMl = ''  THEN  DO:
   ASSIGN lcXML = getChaveTTJson('items', 'xml').
    
END.


    
IF lcXMl = '' THEN DO:
   RETURN 'nok'.
END.
    
 
//se o tamanho for menor que 100 quer dizer que Ç o nome do arquivo e n∆o o conteudo que esta na tag
IF length(lcXML) < 100 THEN DO:
   /*MESSAGE 'longchar'
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
   ASSIGN cArquivo = STRING(lcXMl).
   IF SEARCH(cArquivo) <> ? THEN
      COPY-LOB FROM FILE cArquivo TO lcXml.   
   ELSE DO:
      //ASSIGN cErro = 'arquivo n∆o encontrado:' +  cArquivo .
      RETURN 'nok'.                                      
   END.
      
END.

ASSIGN lcXML = REPLACE(lcXML,"\/","/")
       lcXML = REPLACE(lcXML,'\"','"')
       . 


RUN esapi/extrairDadosXmlNfRetorno.p(INPUT-OUTPUT TABLE ttReg,
                                     INPUT lcXMl,
                                     OUTPUT retOk,
                                     OUTPUT cErroProgress).
IF retOk = FALSE THEN DO:
   ASSIGN lcXML = cErroProgress
          ttReg.LOG_erro_xml = YES .
   RETURN 'nok'.
END.
FIND FIRST ttReg NO-ERROR.

//copiando o conteudo do xml para o banco de dados
COPY-LOB FROM lcXML TO ttReg.conteudo_xml.

FIND retornos_lisa NO-LOCK
    WHERE retornos_lisa.cod_estabel  = ttReg.cod_estabel
    AND   retornos_lisa.serie        = ttReg.serie
    AND   retornos_lisa.nr_nota_fis  = ttReg.nr_nota_fis
    NO-ERROR .
IF NOT AVAIL retornos_lisa THEN DO:
   RUN criarRetornoLISA.
   RUN getEtqsRomaneio.
   RUN criarRomaneiosRetorno.

   ASSIGN logJaProcessado = NO.
END.
ELSE
   ASSIGN logJaProcessado = YES.
   

/*

Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 retorno_lisa_id                  inte        i
   20 nr_pedido                        inte        i
   30 pre_pedido                       inte        i
   40 conteudo_xml                     clob
   50 num_situacao                     inte        i
   60 cod_estabel                      char        i
   70 serie                            char        i
   80 nr_nota_fis                      char        i
   90 protocolo                        char        i
  100 protocolos_nfs_refer             char[50]
  110 dt_hr_registro                   datetm      i
*/


PROCEDURE getEtqsRomaneio:

    DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cData AS CHARACTER   NO-UNDO.
    
    RUN geTtNos('separacao','','json', OUTPUT TABLE ttNos).
    FOR EACH ttNos:
        ASSIGN iCont = iCont + 1.
        CREATE ttRomaneio.
        ASSIGN ttRomaneio.romaneio_retorno_lisa_id = iCont.
        FOR EACH ttJson
            WHERE ttJson.agrupJson = ttNos.id.
            CASE ttJson.tag:
                WHEN 'produto' THEN
                    ASSIGN ttRomaneio.it_Codigo = ttJson.valor.
                WHEN 'lote' THEN
                    ASSIGN ttRomaneio.cod_Refer = ttJson.valor.
                WHEN 'rolo' THEN
                    ASSIGN ttRomaneio.num_rolo  = int(ttJson.valor).
                WHEN 'id' THEN
                    ASSIGN ttRomaneio.id_etq_lisa =  ttJson.valor .
                WHEN 'item' THEN
                    ASSIGN ttRomaneio.nr_seq_lisa = int(ttJson.valor) .
                WHEN 'hora' THEN
                   ASSIGN ttRomaneio.hora =  ttJson.valor .
                WHEN 'data' THEN DO:
                   ASSIGN cData = ttJson.valor .
                   IF cData <> '' AND length(cData) = 8 THEN DO:
                      ASSIGN ttRomaneio.data = convDt4A2M2D(cData).
                   END.
                END.
                WHEN 'quantidade' THEN
                   ASSIGN ttRomaneio.quantidade =  dec(replace(ttJson.valor,'.',',') ).
            END CASE.
            
            IF lCodigoProdUnificado THEN  DO:
               ASSIGN ttRomaneio.it_codigo = ENTRY(1,ttRomaneio.it_codigo,"-") 
                      ttRomaneio.cod_refer = ENTRY(2,ttRomaneio.it_codigo,"-")   
                      .                 
            END.

        END.
    END.
    //{esp/exportarTabelacsv3.i ttNos " " " " "  "ttNos01" }
    //{esp/exportarTabelacsv3.i ttRomaneio " " " " "  "ttRomaneio01" }
  

END PROCEDURE.

PROCEDURE criarRomaneiosRetorno:

  DEFINE VARIABLE hBoLisa02 AS HANDLE      NO-UNDO.
  DEFINE VARIABLE cErro     AS CHARACTER   NO-UNDO.

  RUN esbo/bolisa02.p PERSIST SET hBoLisa02.

  RUN iniciar                IN hBolisa02.
  RUN setIdPai               IN hBoLisa02(idRetorno).
  EMPTY TEMP-TABLE ttRomaneioErro.

  FOR EACH ttRomaneio:
      EMPTY TEMP-TABLE ttRomAux.
      CREATE ttRomAux.
      BUFFER-COPY ttRomaneio TO ttRomAux.
      RUN setTTReg IN hBoLisa02(INPUT TABLE ttRomAux).
      RUN incluir  IN hBoLisa02.
      RUN getErro  IN hBoLisa02(OUTPUT cErro).
      IF cErro <> '' THEN DO:
         CREATE ttRomaneioErro.
         BUFFER-COPY ttRomaneio TO ttRomaneioErro.
         ASSIGN ttRomaneioErro.descrErro = cErro.
      END.                                       
  END.                                           
  RUN finalizar IN hBoLisa02.

  


END PROCEDURE.

PROCEDURE criarRetornoLISA:

    DEFINE VARIABLE cDataEmis AS DATE        NO-UNDO.
    FIND FIRST ttReg NO-ERROR.

    IF AVAIL ttReg THEN DO:

       CREATE retornos_lisa.
       BUFFER-COPY ttReg TO retornos_lisa.
       ASSIGN retornos_lisa.retorno_lisa_id = NEXT-VALUE(seq_retorno_lisa)
              retornos_lisa.dt_hr_registro  = NOW 
              idRetorno = retornos_lisa.retorno_lisa_id
              /*retornos_lisa.vl_nota         = ttReg.vl_nota
              retornos_lisa.dt_nf           = ttReg.dt_Nf
              retornos_lisa.dt_hr_recbto    = ttReg.dt_hr_recbto 
              retornos_lisa.arquivo_json    = ttReg.arquivo_json*/
              .

    END.


END PROCEDURE.
