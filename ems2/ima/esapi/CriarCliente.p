
{esapi/clienteConvJson.i}
{esbo/errosBo.i}
{esp/util.i}

DEFINE TEMP-TABLE tt-emitente LIKE Emitente
FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE tt-emitente-aux LIKE Emitente
FIELD r-rowid AS ROWID.

DEFINE INPUT PARAMETER TABLE FOR tt-emitente.
DEFINE INPUT PARAMETER TABLE FOR ttEmitenteExt.
DEFINE INPUT PARAMETER TABLE FOR ttAtividades.
DEFINE OUTPUT PARAMETER TABLE FOR rowErrors.


//DEFINE OUTPUT PARAMETER cErro AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAtiv   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE obs     AS CHARACTER   NO-UNDO.

DEFINE VARIABLE hBOEmitente    AS HANDLE NO-UNDO. 
DEFINE VARIABLE codigoEmitente LIKE emitente.cod-emitente
NO-UNDO. 
DEFINE VARIABLE nomeEmitente   LIKE emitente.nome-emit NO-UNDO. 
DEFINE VARIABLE nomeAbrev LIKE emitente.nome-abrev 
NO-UNDO.

FUNCTION limparCNAE RETURNS CHAR( cnae AS CHAR):

    ASSIGN cnae = REPLACE(cnae,"-","")
           cnae = REPLACE(cnae,".","").
    IF cnae = ? THEN
       ASSIGN cnae = '?'.
    RETURN cnae.

END FUNCTION.

RUN adbo/boad098na.p PERSISTENT SET hBOEmitente.

RUN openQueryStatic IN hBOEmitente (INPUT "Main":U). 
RUN newRecord IN hBOEmitente. 
EMPTY TEMP-TABLE tt-emitente-aux.
RUN getRecord IN hBOEmitente (OUTPUT TABLE tt-emitente-aux). 
FIND FIRST tt-Emitente NO-ERROR.
FIND FIRST tt-emitente-aux NO-ERROR.
IF AVAIL tt-emitente-aux AND AVAIL tt-emitente THEN DO:
   BUFFER-COPY tt-Emitente TO tt-emitente-aux.
   ASSIGN tt-emitente-aux.nome-matriz          = tt-emitente-aux.nome-abrev
          tt-emitente-aux.end-cobranca         = tt-emitente.cod-emitente
          tt-emitente-aux.pais-cob             = tt-emitente.pais
          tt-emitente-aux.estado-cob           = tt-emitente.estado
          tt-emitente-aux.cidade-cob           = tt-emitente.cidade
          tt-emitente-aux.endereco-cob         = tt-emitente.endereco
          tt-emitente-aux.bairro-cob           = tt-emitente.bairro
          tt-emitente-aux.cep-cob              = tt-emitente.cep 
          tt-emitente-aux.cgc-cob              = tt-emitente.cgc
          tt-emitente-aux.ins-est-cob          = tt-emitente.ins-estadual
          obs                                  = tt-emitente.observacoes
          tt-emitente-aux.observacoes          = ''
          .
   IF obs <> '' THEN DO:
     /*  MESSAGE obs
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
      RUN criarHisEmit(tt-emitente-aux.cod-emitente,obs).
   END.
END.
   
//FIND FIRST tt-emitente-aux NO-ERROR.
/*MESSAGE tt-emitente.cod-emitente
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/

/* posicionar no primeiro emitenteÿ */ 
RUN setRecord IN hBOEmitente ( INPUT TABLE tt-Emitente-aux ).
 /* apagar qualquer erro na DBO */ 
RUN emptyRowErrors IN hBOEmitente. 
 /* criar o registro na DBO */ 
RUN createRecord   IN hBOEmitente.
IF RETURN-VALUE = 'NOK' THEN DO: 
   RUN getRowErrors IN hBOEmitente ( OUTPUT TABLE Rowerrors ).
END.
ELSE DO:
    FIND FIRST tt-emitente-aux NO-ERROR.
    IF AVAIL tt-emitente-aux THEN DO:
       /* MESSAGE 'cod emit para ext: ' tt-emitente-aux.cod-emitente
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
      FIND FIRST ttEmitenteExt 
        WHERE ttEmitenteExt.cod-emitente = int(tt-emitente-aux.cod-emitente) NO-ERROR.
      IF AVAIL ttEmitenteExt  THEN DO:
         FIND FIRST ext-emitente
             WHERE ext-emitente.cod-emitente = tt-emitente-aux.cod-emitente
             NO-ERROR.

         ASSIGN ttEmitenteExt.cod-emitente = tt-emitente-aux.cod-emitente.
         /*MESSAGE 'entrei no ext'
                  'j  existe ext?' AVAIL ttEmitenteExt
                 ttemitenteExt.e-mail-fisc SKIP
                 ttemitenteext.e-mail-comerc SKIP
                 ttEmitenteExt.e-mail-financ SKIP
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
         IF NOT AVAIL ext-emitente THEN
            CREATE ext-emitente.

         BUFFER-COPY ttEmitenteExt EXCEPT nome_comprador cpf_comprador celular_waths TO ext-emitente.
         RUN criarContEmit(tt-emitente-aux.cod-emitente,
                     tt-emitente-aux.telefone[1] ,
                     ttEmitenteExt.e-mail-fisc,
                     ttEmitenteExt.nome_comprador,
                     ttEmitenteExt.cpf_comprador,
                     ttEmitenteExt.celular_waths,
                     'FISCAL',
                     tt-emitente-aux.identific,
                     ttEmitenteExt.aplicacao,  
                     ttEmitenteExt.cnpj_contabilidade
                      ). 
         RUN criarContEmit(tt-emitente-aux.cod-emitente ,
                     tt-emitente-aux.telefone[1] ,
                     ttEmitenteExt.e-mail-comerc ,
                     ttEmitenteExt.nome_comprador,
                     ttEmitenteExt.cpf_comprador,
                     ttEmitenteExt.celular_waths,
                     'COMERCIAL',
                      tt-emitente-aux.identific,
                      1, // nenhuma aplica‡Æo
                      ''
                       ). 
         
         RUN criarContEmit(tt-emitente-aux.cod-emitente,
                     tt-emitente-aux.telefone[1] ,
                     ttEmitenteExt.e-mail-financ,
                     ttEmitenteExt.nome_comprador,
                     ttEmitenteExt.cpf_comprador,
                     ttEmitenteExt.celular_waths,
                     'FINANCEIRO',
                     tt-emitente-aux.identific,
                     1, // nenhuma aplica‡Æo,
                     ''             
                     ). 



      END.                                            

      FIND FIRST ttAtividades
          NO-ERROR.
      IF AVAIL ttAtividades THEN DO:
         /*MESSAGE 'cod.emitente ativ.' ttAtividades.cod_emitente
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
      END.
      ELSE DO:
         /*MESSAGE 'nao existe registro de atividade'
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
      END.
      FOR EACH ttAtividades
          WHERE ttAtividades.cod_emitente       = INT(tt-emitente-aux.cod-emitente) :
          ASSIGN cAtiv = trim(limparCNAE(ttAtividades.cod_ativ)).

          FIND FIRST cnaes
              WHERE  cnaes.cod_cnae = trim(cAtiv)
              NO-LOCK NO-ERROR.
          IF NOT AVAIL cnaes THEN DO:
              /*MESSAGE "atividadee nÆo cadastrada:" cAtiv
                  VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
              RUN inserirErroManual(100001,"Atividade " +  cAtiv   + " nÆo cadastrada",cAtiv,"OTHER",'Cadastre a Atividade no cadastro de CNAES',"WARNING").
          END.
          //ASSIGN ttAtividades.cod_emitente      = tt-emitente-aux.cod-emitente.
          CREATE emitente_cnae.
          ASSIGN emitente_cnae.cod_emitente     = ttAtividades.cod_emitente
                 emitente_cnae.cod_cnae         = cAtiv
                 emitente_cnae.cod_tipo_cnae    = IF ttAtividades.ativ_princ THEN 1 ELSE 0 .
      END.


    END.  

END.


  
  
PROCEDURE criarContEmit:

    DEFINE INPUT  PARAMETER pCodEmit    AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pTelefone   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pEmail      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pNomeCompr  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCpfCompr   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCelWaths   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pArea       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pIdentific  AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pAplicacao  AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pCNPJ       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iSeq AS INTEGER     NO-UNDO.
    DEFINE BUFFER bf FOR cont-emit.
    FIND LAST cont-emit
        WHERE cont-emit.cod-emitente = pCodemit
        NO-LOCK NO-ERROR.
    IF AVAIL cont-emit THEN
       ASSIGN iSeq = cont-emit.sequencia + 10.
    ELSE
       ASSIGN iSeq = 10.


    CREATE cont-emit.
    ASSIGN cont-emit.cod-emitente   = pCodEmit
           cont-emit.sequencia      = iSeq
           cont-emit.nome           = pNomeCompr
           cont-emit.area           = pArea
           cont-emit.cargo          = pCpfCompr
           cont-emit.CHAR-2         = pCNPJ
           cont-emit.telefone       = pTelefone 
           cont-emit.e-mail         = pEmail
           cont-emit.identific      = pIdentific
           cont-emit.int-1          = pAplicacao
           .
   IF pCelWaths <> '' THEN DO:
       CREATE ext-cont-emit.
       ASSIGN ext-cont-emit.cod-emitente   = cont-emit.cod-emitente
              ext-cont-emit.sequencia      = cont-emit.sequencia
              ext-cont-emit.celular1       = pCelWaths
              ext-cont-emit.messenger1     = YES
              ext-cont-emit.aplicativo1    = 'WhatsAPP'
              .
   END.
  

END PROCEDURE.


PROCEDURE criarHisEmit:

    DEFINE INPUT  PARAMETER iEmitente AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER cHist     AS CHARACTER   NO-UNDO.

    FIND FIRST his-emit
        WHERE his-emit.cod-emitente = iEmitente
        NO-LOCK NO-ERROR.
    IF NOT AVAIL his-emit THEN DO:
       CREATE his-emit .
       ASSIGN his-emit.dt-his-emit  = TODAY
              his-emit.horario      = STRING(TIME,"HH:MM") 
              his-emit.cod-emitente = iemitente 
              his-emit.log-2        = YES
              his-emit.historico    = cHist.   
    END.
    ELSE DO:
        ASSIGN his-emit.historico    = cHist
               his-emit.horario      =  STRING(TIME,"HH:MM") 
               his-emit.dt-his-emit  = TODAY
               his-emit.log-2        = YES.
    END.

    

           
    //horario historico dt-his-emit cod-emitente
END PROCEDURE.

/*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod-emitente                     inte        im
   20 sequencia                        inte        im
   30 nome                             char        im
   40 cargo                            char        m
   50 area                             char        m
   60 telefone                         char        m
   70 ramal                            char        m
   80 telefax                          char        m
   90 ramal-fax                        char        m
  100 e-mail                           char
  110 observacao                       char
  250 identific                        inte
  260 char-1                           char
  270 char-2                           char
  280 dec-1                            deci-8
  290 dec-2                            deci-8
  300 int-1                            inte
  310 int-2                            inte
  320 log-1                            logi
  330 log-2                            logi
  340 data-1                           date
  350 data-2                           date
  360 check-sum                        char
*/
