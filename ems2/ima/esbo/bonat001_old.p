/*************************************************************************
Programa: bonat001- BO de automatizaá∆o da natureza de operaá∆o a partir
da tabela param_nat_operacao e dos dados passados como parametro do 
pedido de venda

06/2023 - acrescimo de TTD e opá∆o pelo simples como critÇrios de busca - Tadeu
***************************************************************************/
{utp/ut-glob.i}
{esp/util.i}
DEFINE TEMP-TABLE  ttMensagem no-undo
    FIELD tipo  AS CHAR
    FIELD ordem AS INT
    FIELD descricao AS CHAR FORMAT 'x(200)'.

DEFINE VARIABLE i AS INTEGER NO-UNDO INIT 0.
DEFINE VARIABLE hLog02 AS HANDLE      NO-UNDO.
DEFINE VARIABLE lHtml  AS LOGICAL     NO-UNDO INIT YES.
DEFINE VARIABLE cHtml  AS CHARACTER   NO-UNDO FORMAT 'x(8000)'.
DEFINE BUFFER bfEmitente FOR emitente.
DEFINE STREAM s1.
RUN especificos\esporadicos\log02.p PERSISTENT SET  hLog02.
RUN arquivoSaida IN hLog02('bonat001').
RUN gerarLog IN hLog02(NO).

FUNCTION getDescrEnquadTTD RETURNS CHAR(codigo AS CHAR).

    DEFINE VARIABLE cLista   AS CHAR     NO-UNDO.
    DEFINE VARIABLE cRetorno AS CHARACTER   NO-UNDO.

    RUN getVlParametro('lista_ttd',OUTPUT cLista).
    IF cLista = '' THEN
       ASSIGN cLista = '11'.
    IF codigo = '' THEN
       ASSIGN codigo = '0'.
    FIND opcoes_lista
        WHERE opcoes_lista.lista_id             = INT(cLista)
        AND   int(opcoes_lista.cod_opcao)       = int(codigo)
        NO-LOCK NO-ERROR.

    RETURN IF AVAIL opcoes_lista THEN opcoes_lista.DESC_opcao ELSE '' .
END FUNCTION.


PROCEDURE gerarLogHtml:
    DEFINE VARIABLE lLogHtml AS LOGICAL     NO-UNDO.
    ASSIGN lHtml  = lLogHtml.
END. 

PROCEDURE verificarEstadoParam:

    DEFINE INPUT  PARAMETER pCodParamNatOperacao AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pEstado              AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER lNext                AS LOGICAL     NO-UNDO INIT NO.
    DEFINE BUFFER bfUfParamNatOperacao2 FOR ufs_params_nat_operacao.
    DEFINE VARIABLE cMensagem AS CHARACTER   NO-UNDO FORMAT 'x(4000)' INIT ''.
    
    FIND FIRST bfUfParamNatOperacao2
          WHERE bfUfParamNatOperacao2.cod_param_nat_operacao =   pCodParamNatOperacao 
          AND   bfUfParamNatOperacao2.uf = pEstado NO-LOCK NO-ERROR.
    IF NOT AVAIL bfUfParamNatOperacao2 THEN DO:
       ASSIGN lNext = YES.
    END.
END PROCEDURE.

PROCEDURE retornarTipoAtividadeCNAE:
    DEFINE INPUT  PARAMETER pCodCnae            AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER iIndTipoAtividade   AS INT         NO-UNDO.
    DEFINE OUTPUT PARAMETER iClassificacao      AS INT         NO-UNDO .

    FIND FIRST cnaes 
        WHERE cnaes.cod_cnae = pCodCnae  NO-LOCK NO-ERROR.
          IF AVAIL cnaes THEN DO:
             ASSIGN iIndTipoAtividade    = int(cnaes.ind_tipo_atividade).
             IF cnaes.ind_tipo_atividade <> '4' THEN
                ASSIGN iClassificacao    = 1.
             ELSE
                ASSIGN iClassificacao    = 2.   
          END.
END.

PROCEDURE retornarFinalidadeCNAE:
    DEFINE INPUT  PARAMETER pCodCnae            AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER iCodFinalidade      AS INT         NO-UNDO INIT ''.
    DEFINE OUTPUT PARAMETER iUtilizar           AS INTEGER     NO-UNDO INIT ''.
    FIND FIRST cnaes 
        WHERE cnaes.cod_cnae = pCodCnae  NO-LOCK NO-ERROR.
          IF AVAIL cnaes THEN DO:
             ASSIGN iCodFinalidade       = INT(cnaes.cod_finalidade_venda).
             IF cnaes.cod_finalidade_venda <> '2' THEN DO:
                ASSIGN iUtilizar    = 1.
             END.
             ELSE 
                ASSIGN iUtilizar    = 2.   
          END.
END PROCEDURE.

PROCEDURE retornarTipoAtividadeCliente:
    DEFINE INPUT  PARAMETER pCliente        AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER iTipoAtividade  AS INT         NO-UNDO INIT ''.
    DEFINE OUTPUT PARAMETER codCnae         AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iUtilizar           AS INTEGER     NO-UNDO.
    FOR EACH emitente_cnae NO-LOCK
        WHERE emitente_cnae.cod_emitente = pCliente
        AND emitente_cnae.eliminado = NO
         BY emitente_cnae.cod_tipo_cnae.
        /*MESSAGE emitente_cnae.cod_cnae
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
         ASSIGN codCnae = emitente_cnae.cod_cnae.
         RUN retornarTipoAtividadeCNAE(emitente_cnae.cod_cnae, OUTPUT iTipoAtividade, OUTPUT iUtilizar).
         IF iUtilizar  = 1 THEN LEAVE.
    END.

END PROCEDURE.


PROCEDURE retornarFinalidadeCliente:
    DEFINE INPUT  PARAMETER pCliente    AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER iFinalidade AS INTEGER     NO-UNDO INIT ''.
    DEFINE OUTPUT PARAMETER codCnae     AS CHAR        NO-UNDO INIT ''.
    DEFINE VARIABLE iUtilizar           AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lAchou              AS LOGICAL     NO-UNDO INIT NO.
    FOR EACH emitente_cnae NO-LOCK
        WHERE emitente_cnae.cod_emitente = pCLiente
        AND emitente_cnae.eliminado = NO
         BY emitente_cnae.cod_tipo_cnae.
         ASSIGN lAchou = YES.
         ASSIGN codCnae = emitente_cnae.cod_cnae. 
         RUN retornarFinalidadeCNAE(emitente_cnae.cod_cnae, OUTPUT iFinalidade, OUTPUT iUtilizar).
         IF iFinalidade = 0 THEN DO:
            RUN criarMensagem('erro',"Cliente com CNAE N∆o Classificado").
         END.
         IF iUtilizar  = 1 THEN LEAVE.
    END.
    IF lAchou = NO THEN
       RUN criarMensagem('erro',"Favor verificar o cadastro do Cliente " + string(pCliente) + " , pois n∆o existe CNAE informado").
END PROCEDURE.



PROCEDURE buscarNatOperacao:

DEFINE INPUT  PARAMETER pFinalidadeVenda        AS INTEGER     NO-UNDO.
/*DEFINE INPUT  PARAMETER pVendaTriangular        AS LOGICAL     NO-UNDO.*/
DEFINE INPUT  PARAMETER pcodEstab               AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCliente                AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pNomeAbrevTriang        LIKE ped-venda.nome-abrev-tri NO-UNDO.
/*DEFINE INPUT  PARAMETER pItem                   AS CHARACTER   NO-UNDO.*/
DEFINE OUTPUT PARAMETER pNatOperacao            AS CHARACTER   NO-UNDO INIT ''.
DEFINE OUTPUT PARAMETER pCodParamNatOperacao    AS INTEGER     NO-UNDO INIT 0 .

/*OUTPUT STREAM s1 TO c:\temp\logparamNatPedvenda.txt.*/

DEFINE BUFFER bfParamNatOperacao FOR PARAM_nat_operacao.


DEFINE VARIABLE lVendaTriangular        AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE lVarejo                 AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE lAtacado                AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE lIndustria              AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE lServico                AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE lSuframa                AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE lEntrouTriang           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE dAliquota               AS DECIMAL     NO-UNDO.
DEFINE VARIABLE lNext                   AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE lAchou                  AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE lAchouTriang            AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE lSuframaTriang          AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lNextTriang             AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lContrIcms              AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE lContrIcmsTriang        AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE lOpcaoSimples           AS LOGICAL     NO-UNDO.

DEFINE VARIABLE origemItem              AS INTEGER     NO-UNDO.
//DEFINE VARIABLE cTipoAtividade        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iTipoAtividade          AS INTEGER     NO-UNDO.
DEFINE VARIABLE indUfTriang             AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRelUf                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE numTipAtiv              AS INTEGER     NO-UNDO.
DEFINE VARIABLE codCnae                 AS CHAR       NO-UNDO.
DEFINE VARIABLE cEstado                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEstadoTriang           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDescFinVenda           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDescTipoAtiv           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMensagem               AS CHARACTER   NO-UNDO FORMAT 'X(500)'.
DEFINE VARIABLE cLabelSuframa           AS CHARACTER   NO-UNDO FORMAT 'x(50)' .
DEFINE VARIABLE cLabelContrib           AS CHARACTER   NO-UNDO FORMAT 'x(50)' .
DEFINE VARIABLE cLabelVendaTriang       AS CHARACTER   NO-UNDO FORMAT 'x(50)' .
DEFINE VARIABLE cLabelEstado            AS CHARACTER   NO-UNDO FORMAT 'x(50)' .
DEFINE VARIABLE cLabelRelacEstado       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEnquadTTD              AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cArquivoLog AS CHARACTER   NO-UNDO.
ASSIGN cArquivoLog = 'p:\log_nat_operacao\' + c-seg-usuario + "_" +  STRING(TIME) + '.html'.
RUN limparErros.
RUN buscarDescFinalidadeVenda(INPUT pFinalidadeVenda ,OUTPUT cDescFinVenda ).
FIND FIRST emitente 
    WHERE emitente.cod-emitente = pCliente NO-LOCK NO-ERROR.



FIND FIRST ext-emitente OF emitente NO-LOCK NO-ERROR.


/*inicio html*/
ASSIGN chtml = '<html><head><title>Log Busca da Natureza de Operacao por regras Cadastradas</title></head><body><h1> Parametros do Pedido</h1>' +
               '<h2>Finalidade de Venda:' + cDescFinVenda + '</h2>' +
               '<h2>Estab.:'  + pCodEstab + '</h2>' + 
               '<h2>Cliente:' + string(pCliente) + '</h2>'.
IF AVAIL emitente THEN DO:
    ASSIGN cEstado       = emitente.estado
           lOpcaoSimples = IF SUBSTR(char-1,133,1) = 'S' THEN YES ELSE NO.
    IF AVAIL ext-emitente THEN DO:
       ASSIGN cEnquadTTD = ext-emitente.enquad_ttd.
    END.
    ELSE
       ASSIGN cEnquadTTD = '0'.
    IF cEnquadTTD = '' THEN
       ASSIGN cEnquadTTD = '0'.

    /*verifica se o estado do cliente e igual ou diferente do estab do estab corrente */
    ASSIGN lSuframa = emitente.cod-suframa <> ''.
    RUN verificarRelUFCli(pCodEstab,
                             emitente.estado,
                             OUTPUT iRelUF).

   RUN retornarTipoAtividadeCliente(emitente.cod-emitente, OUTPUT iTipoAtividade, OUTPUT codCnae).
   RUN buscarDescTipoAtividade(INPUT iTipoAtividade ,OUTPUT cDescTipoAtiv ).
   ASSIGN lContrIcms  = emitente.contrib-icms.
   ASSIGN  cLabelEstado = IF irelUF = 1 THEN 'Dentro' ELSE 'Fora'
           cLabelContrib = IF lContrIcms THEN "SIM" ELSE "NAO"
           cLabelSuframa = IF lSuframa THEN "SIM"  ELSE "NAO". 
           cHtml = cHtml + '<h2>Estado:' + cEstado + ' - Contribuinte:' + cLabelContrib +
          " - Suframa:" + cLabelSuframa + ' - Tipo de Atividade do Cliente:' + cDescTipoAtiv + '</h2><h2>Codigo CNAE do Cliente:' + codCNAE +
           ' - Relac. Estado:' + cLabelEstado + '</h2><h2>Optante pelo Simples?' + string(lOpcaoSimples,'Sim/N∆o')    + '</h2>' +
            '<h2>Enquadramento TTD:' + getDescrEnquadTTD(cEnquadTTD) + '<hr>'.
END.
ELSE DO :
  ASSIGN cHtml = cHTML + "<hr><h2>Cliente NAO encontrado</h2><hr>".
END.

IF pNomeAbrevTriang <> '' THEN DO:
   RUN buscarDadosClienteTriang(pNomeAbrevTriang,pCodEstab, OUTPUT lContrIcmsTriang, OUTPUT indUfTriang, OUTPUT lSuframaTriang, OUTPUT cEstadoTriang ).
   ASSIGN lVendaTriangular = YES.
    ASSIGN cHtml = chtml + '<h2>Nome Abrev. Cliente Triang.:' + pNomeAbrevTriang + '</h2>' .  
   ASSIGN  cLabelContrib        =  IF lContrIcmsTriang THEN "SIM" ELSE "NAO"
              cLabelRelacEstado =  IF indUfTriang = 1 THEN "DENTRO" ELSE "FORA"
              cLabelSuframa     =  IF lSuframaTriang THEN 'SIM' ELSE 'NAO'
              cHtml             = cHtml + '<h2>Dados cliente Triangular: Contribuinte ICMS? ' + cLabelContrib + ' Relacao Estado:' + cLabelRelacEstado 
                               + ' Suframa? ' + cLabelSuframa + ' Estado:' + cEstadoTriang + '</h2><hr>'.
END.
ELSE DO:
   ASSIGN cHtml = cHTML + "<hr><h2>Cliente TRIANGULAR NAO encontrado</h2><hr>".
END.



IF AVAIL emitente THEN DO:
   
   IF  iTipoAtividade = 0 THEN DO:
        ASSIGN cHtml = cHtml + '<h3>Tipo de Atividade zerada <\h3>'.
       IF codCnae <> '' THEN
          RUN criarMensagem('erro',"Cliente com CNAE:" + codCnae + " - sem Tipo de Atividade").
       ELSE
          RUN criarMensagem('erro',"Cliente sem CNAE cadastrado").
       
   END.
   ELSE DO:
       /*IF c-seg-usuario = 'super' THEN
           MESSAGE 'finalid:' pFinalidadeVenda SKIP
                  'estab:' pCodEstab SKIP
                  'contrib:' lContrIcms SKIP
                  'UF:' iRelUF SKIP
                  'V.Triangular:' lVendaTriangular SKIP
                  'Tp.Ativ:' iTipoAtividade SKIP
                  'suframa:' lSuframa SKIP
               VIEW-AS ALERT-BOX INFO BUTTONS OK.*/


       PARAMPrinc:
       FOR EACH PARAM_nat_operacao NO-LOCK
              WHERE param_nat_operacao.log_inativo             = NO 
               AND  param_nat_operacao.cod_finalidade_venda    = pFinalidadeVenda 
               AND  param_nat_operacao.cod_estab               = pCodEstab
               AND  PARAM_nat_operacao.LOG_contribuinte_icms   = lContrIcms
               AND  PARAM_nat_operacao.ind_cliente_uf          = iRelUF
               AND  PARAM_nat_operacao.LOG_venda_triangular    = lVendaTriangular 
               AND  param_nat_operacao.cod_tipo_atividade      = iTipoAtividade
               AND  PARAM_nat_operacao.log_suframa             = lSuframa
               .


           IF PARAM_nat_operacao.cod_estab = '505' AND  (PARAM_nat_operacao.enquadr_fiscal         <> int(cEnquadTTD) OR PARAM_nat_operacao.LOG_simples     <> lopcaoSimples )THEN NEXT.


           RUN buscarDescTipoAtividade(INPUT param_nat_operacao.cod_tipo_atividade ,OUTPUT cDescTipoAtiv ).
           /*IF c-seg-usuario= 'super' THEN
              MESSAGE  'estado cliente principal:' emitente.cod-emitente SKIP
                       emitente.estado SKIP
                       cEstado
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
           RUN verificarEstadoParam(param_nat_operacao.cod_param_nat_operacao,cEstado,OUTPUT lNext).
           ASSIGN cLabelEstado = IF lNext THEN "NAO" ELSE "SIM"
                  cLabelVendaTriang =  IF lVendaTriangular THEN "SIM" ELSE "NAO"
                  cHtml = cHtml + '<h2>REGRA: ' + string(PARAM_nat_operacao.cod_param_nat_operacao) + ' - NATUREZA:' + string(PARAM_nat_operacao.cod_nat_operacao) 
                         + ' - ACHOU ESTADO?' + cLabelEstado + '</h2>' .
           IF lNext = NO THEN DO:
              IF lVendaTriangular = YES THEN DO:
                  RUN incluirlog IN hlog02('','Ç cliente triangular').
                  ASSIGN cHtml = cHtml + '<table border="1"><tr><td>Regra Triang.</td><td>Natureza Operacao</td><td>Estado OK Triang.?</td></tr>'.      
                  ASSIGN lEntrouTriang = NO
                         lAchouTriang = NO.
                  paramTriang:
                  FOR EACH bfParamNatOperacao NO-LOCK
                     WHERE bfParamNatOperacao.LOG_inativo           = NO
                     AND   bfParamNatOperacao.LOG_contribuinte_icms = lContrIcmsTriang
                     AND   bfParamNatOperacao.ind_cliente_uf        = indUfTriang
                     AND   bfParamNatOperacao.LOG_suframa           = lSuframaTriang
                     AND   bfParamNatOperacao.cod_param_nat_operacao_pai = PARAM_nat_operacao.cod_param_nat_operacao.
                     ASSIGN lEntrouTriang = YES.
                     RUN incluirlog IN hlog02('','encontrei a  regra do cliente triangular').
                     RUN buscarDescFinalidadeVenda(INPUT bfParamNatOperacao.cod_finalidade_venda ,OUTPUT cDescFinVenda ).
                     RUN buscarDescTipoAtividade(INPUT bfParamnatOperacao.cod_tipo_atividade ,OUTPUT cDescTipoAtiv ).
                     RUN verificarEstadoParam(bfParamNatOperacao.cod_param_nat_operacao,cEstadoTriang,OUTPUT lNextTriang).
                     ASSIGN  cLabelEstado      =  IF lNextTriang THEN "NAO" ELSE "SIM".
                     ASSIGN cHtml = cHtml + '<tr><td>'  + string(bfParamNatOperacao.cod_param_nat_operacao)   + '</td><td>' + bfParamnatOperacao.cod_nat_operacao + '</td><td>' +
                                    cLabelEstado + '</td></tr>'.
                     IF LNextTriang = NO THEN DO:
                        /*IF c-seg-usuario = 'super' THEN
                           MESSAGE 'achei o estado' cEstadoTriang
                               VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

                        ASSIGN  pNatOperacao           = bfParamNatOperacao.cod_nat_operacao
                                pCodParamNatOperacao   = bfParamNatOperacao.cod_param_nat_operacao
                                lAchouTriang           = YES.
                        RUN limparErros.
                          
                        LEAVE paramTriang.
                     END.
                     ELSE DO:
                       /* IF c-seg-usuario = 'super' THEN
                           MESSAGE ' nao achei o estado' cEstadoTriang SKIP
                               'vou buscar outra'
                               VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
                     END.
                  END.
                  /*IF c-seg-usuario = 'super' THEN
                     MESSAGE 'achou triang?' lAchouTriang SKIP
                              'cod.param.:' param_nat_operacao.cod_param_nat_operacao
                         VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
                  IF lEntrouTriang = NO THEN DO:
                     ASSIGN chtml = cHtml + '<tr><td colspan= "3">NENHUM REGISTRO COM OS DADOS DO CLIENTE TRIANGULAR FOI ENCONTRADO</td></tr>'.
                  END.
                  ASSIGN cHtml = cHtml + "</table>".
                  /*IF c-seg-usuario = 'super' THEN 
                      MESSAGE 'achou triang?' lAchouTriang SKIP
                              'cod.param.:' param_nat_operacao.cod_param_nat_operacao
                         VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
                  IF lAchouTriang = NO THEN DO:
/*                     IF c-seg-usuario = 'super' THEN                                   */
/*                       MESSAGE 'dentro do nao achou' SKIP                              */
/*                               'achou triang?' lAchouTriang SKIP                       */
/*                               'cod.param.:' param_nat_operacao.cod_param_nat_operacao */
/*                          VIEW-AS ALERT-BOX INFO BUTTONS OK.                           */
                    RUN incluirlog IN hlog02('',' Ç cliente triangular e n∆o achou regra').
                    /*
                    RUN criarmensagem('erro', "codigo 1 -N∆o foi encontrada regra para Finalidade:" 
                                      + cDescFinVenda + " - Estab.:" + pCodEstab 
                                      + " - Contribuinte ICMS:" + string(emitente.contrib-icms,"SIM/NAO") 
                                      + " - Dentro do Estado:" + IF iRelUf = 1 THEN "SIM" ELSE "NAO" + " - venda triangular:" 
                                      + string(lVendaTriangular,"SIM/NAO") + " - Tipo Atividade:" 
                                      + cDescTipoAtiv + " - Estado:" + emitente.estado 
                                      + " - cliente suframa:" + STRING(lSuframa,"SIM/NAO")
                                      + " - Cliente Triang:" + string(pNomeAbrevTriang) 
                                      +  " - Contr.Icms Triang.:" + string(lContrIcmsTriang,"SIM/NAO")  
                                      + " - Cliente triang. suframa:" + STRING(lSuframaTriang,"SIM/NAO")
                                      + " - Estado triang:" + cEstadoTriang
                                      ).*/
                    RUN criarMensagem('erro','N∆o foi encontrado Regra. Favor informar ao setor fiscal o arquivo:' + cArquivoLog + 
                                      ' para detalhamento dos dados necess†rios para criaá∆o da regra para atender o pedido corrente ').
                  END.
                  ELSE DO:
                    /*ASSIGN cHtml = cHtml + "</table></td></tr>".*/
                    LEAVE paramPrinc.
                  END.

              END.  
              ELSE DO:
                 ASSIGN cHtml = cHtml + "<h3>NAO eh triangular</h3>".
                 RUN incluirlog IN hlog02('','nao Ç cliente triangular').
                 ASSIGN pNatOperacao           = PARAM_nat_operacao.cod_nat_operacao
                        pCodParamNatOperacao   = param_nat_operacao.cod_param_nat_operacao
                       lAchou = YES.
                LEAVE paramPrinc.
              END.
           END.
       END.
       IF lAchou = NO AND lAchouTriang = NO THEN DO:
          ASSIGN cHtml = cHtml + "<tr><td><h1>N∆o foi encontrada nenhuma regra</h1></td></tr>".  
          RUN incluirlog IN hlog02('','regra n∆o encontrada').
          /*RUN criarmensagem('erro', "codigo 2 -N∆o foi encontrada regra para Finalidade:" 
                            + cDescFinVenda + " - Estab.:" + pCodEstab 
                            + " - Contribuinte ICMS:" + string(emitente.contrib-icms,"SIM/NAO") 
                            + " - Dentro do Estado:" + IF iRelUf = 1 THEN "SIM" ELSE "NAO"
                            + " - Cliente triang. suframa:" + STRING(lSuframa,"SIM/NAO") 
                            + " - Venda triangular:" + string(lVendaTriangular,"SIM/NAO")
                            + " - Tipo Atividade:" + cdescTipoAtiv
                            + " - Estado:" + emitente.estado  
                            ).*/
          RUN criarMensagem('erro','N∆o foi encontrado Regra. Favor informar ao setor fiscal o arquivo:' + cArquivoLog + 
                                      ' para detalhamento dos dados necess†rios para criaá∆o da regra para atender o pedido corrente ').
       END.
       /*ASSIGN cHtml = cHtml + "</table>".*/
   END.
END.
ELSE DO:
  RUN incluirlog IN hlog02('','cliente n∆o encontrado').
   RUN criarMensagem('Erro','Emitente n∆o encontrado').
END.
OUTPUT TO value(cArquivoLog).
PUT cHtml SKIP.

OUTPUT CLOSE.

END PROCEDURE.

PROCEDURE criarMensagem:
    DEFINE INPUT  PARAMETER pTipo AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pMensagem AS CHARACTER   NO-UNDO FORMAT 'x(1500)'.
    CREATE ttMensagem.
    ASSIGN ttMensagem.ordem = i + 1
           ttMensagem.tipo  = pTipo
           ttMensagem.descricao = pMensagem .
END PROCEDURE.
PROCEDURE retornarErros:
    DEFINE OUTPUT PARAMETER cErros AS CHARACTER   NO-UNDO FORMAT 'x(4000)'.
    DEFINE VARIABLE cComum AS CHARACTER   NO-UNDO FORMAT 'x(4000)'.
    FOR EACH ttMensagem
        WHERE ttMensagem.tipo = 'erro' BY ttMensagem.ordem:
        ASSIGN cComum = ttMensagem.descricao.
        IF cErros = '' THEN
           ASSIGN cErros =  cComum .
        ELSE 
           ASSIGN cErros = cErros + CHR(10) + CHR(13) + cComum .
    END.
END PROCEDURE.

PROCEDURE limparErros:
    EMPTY TEMP-TABLE ttMensagem.
END.
PROCEDURE verificarRelUFCli:
    DEFINE INPUT  PARAMETER pEstab AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pUF    AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER iRelUf AS INTEGER     NO-UNDO.
/*     IF c-seg-usuario = 'super' THEN           */
/*        MESSAGE pUF SKIP                       */
/*                pEstab SKIP                    */
/*            VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    FIND FIRST estabelec
        WHERE estabelec.cod-estab = pEstab NO-LOCK NO-ERROR.
    IF AVAIL estabelec THEN DO:
       IF estabelec.estado = pUf THEN
          ASSIGN iRelUf = 1.
       ELSE 
         ASSIGN irelUf = 2.
    END.
    ELSE DO:
       RUN criarMensagem('Erro','Estabelecimento n∆o encontrado').
       ASSIGN iRelUf = 3.
    END.
/*     IF c-seg-usuario = 'super' THEN           */
/*         MESSAGE pUF SKIP                      */
/*                pEstab SKIP                    */
/*                  irelUf '(1-dentro 2- fora)'  */
/*            VIEW-AS ALERT-BOX INFO BUTTONS OK. */
END PROCEDURE.

PROCEDURE buscarDadosClienteTriang:
    DEFINE INPUT  PARAMETER pNomeAbrev           LIKE ped-venda.nome-abrev-tri     NO-UNDO.
    DEFINE INPUT  PARAMETER pCodEstab            AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER pContribuinteIcms    AS LOGICAL     NO-UNDO INIT NO.
    DEFINE OUTPUT PARAMETER pIndClienteUF        AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER pClienteSuframa      AS LOGICAL     NO-UNDO INIT NO.
    DEFINE OUTPUT PARAMETER pEstado              AS CHARACTER   NO-UNDO.
    
    DEFINE VARIABLE iRelUf AS INTEGER     NO-UNDO.
    FIND FIRST bfemitente
        WHERE bfemitente.nome-abrev = pNomeAbrev NO-LOCK NO-ERROR.
    IF AVAIL bfemitente THEN DO:
       RUN verificarRelUFCli(pCodEstab,
                             bfemitente.estado,
                             OUTPUT pIndClienteUF).
       ASSIGN pContribuinteIcms = bfemitente.contrib-icms
              pClienteSuframa   = bfemitente.cod-suframa <> '' .
       ASSIGN pEstado = bfemitente.estado.
    END.      
END PROCEDURE.

PROCEDURE gravarParamNaturPedVenda:
    DEFINE INPUT  PARAMETER pCodParamNatOperacao AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pNrPedido AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pCodEstab AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER lOk       AS LOGICAL     NO-UNDO.

    FIND FIRST ped-venda-ext 
        WHERE ped-venda-ext.nr-pedido = pNrPedido
        AND   ped-venda-ext.cod-estab = pCodEstab
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL ped-venda-ext THEN DO:
       ASSIGN ped-venda-ext.cod_param_nat_operacao = pCodParamNatOperacao
              lOk = YES.
    END.
    ELSE DO:
       ASSIGN lOk = NO.
    END.      
    RELEASE ped-venda-ext.
END PROCEDURE.

PROCEDURE buscarDescFinalidadeVenda:
    DEFINE INPUT  PARAMETER pFinalidadeVenda AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER cDescricao       LIKE finalidades_venda.desc_finalidade_venda   NO-UNDO INIT ''.
    FIND FIRST finalidades_venda
        WHERE finalidades_venda.cod_finalidade_venda = pFinalidadeVenda
        NO-LOCK NO-ERROR.
    IF AVAIL finalidades_venda THEN
       ASSIGN cDescricao =  finalidades_venda.desc_finalidade_venda. 
    ELSE
       ASSIGN cDescricao = "Finalidade:" + string(pFinalidadeVenda) + " n∆o encontrada".
END PROCEDURE.

PROCEDURE buscarDescTipoAtividade:
    DEFINE INPUT  PARAMETER pTipoAtividade AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER cDescricao     AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
    CASE pTipoAtividade:
        WHEN 1 THEN
           ASSIGN cDescricao = 'Varejo'.
        WHEN 2 THEN
           ASSIGN cDescricao = 'Atacado'.
        WHEN 3 THEN
           ASSIGN cDescricao = 'Industria'.
        WHEN 4 THEN
           ASSIGN cDescricao = 'Serviáo'.
    END CASE.

END PROCEDURE.

