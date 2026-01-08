/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/

{include/i-prgvrs.i FT0527RP 2.00.00.078 } /*** "010078" 22/08/2024 15:24:58 ***/

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ft0527rp MFT}
&ENDIF

/*****************************************************************************
**       Programa: FT0527rp.p
**       Data....: 14/03/07
**       Autor...: DATASUL S.A.
**       Objetivo: Emissor DANFE - NF-e
**       Vers∆o..: 1.00.000 - super
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/

/*define variable c-prog-gerado as character no-undo initial "FT0527rp".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.*/

/****************** Definiá∆o de Tabelas Tempor†rias do Relat¢rio **********************/
 {esp/util.i}
DEFINE TEMP-TABLE tt-raw-digita
    FIELD raw-digita AS RAW.

DEFINE TEMP-TABLE tt-param-aux NO-UNDO
    FIELD destino              AS INTEGER
    FIELD destino-bloq         AS INTEGER
    FIELD arquivo              AS CHAR
    FIELD arquivo-bloq         AS CHAR
    FIELD usuario              AS CHAR
    FIELD data-exec            AS DATE
    FIELD hora-exec            AS INTEGER
    FIELD parametro            AS LOGICAL
    FIELD formato              AS INTEGER
    FIELD cod-layout           AS CHARACTER
    FIELD des-layout           AS CHARACTER
    FIELD log-impr-dados       AS LOGICAL
    FIELD v_num_tip_aces_usuar AS INTEGER
&IF "{&mguni_version}" >= "2.071" &THEN
    FIELD ep-codigo            LIKE mgcad.empresa.ep-codigo
&ELSE
    FIELD ep-codigo            AS INTEGER
&ENDIF
    FIELD c-cod-estabel        LIKE nota-fiscal.cod-estabel
    FIELD c-serie              LIKE nota-fiscal.serie
    FIELD c-nr-nota-fis-ini    LIKE nota-fiscal.nr-nota-fis
    FIELD c-nr-nota-fis-fim    LIKE nota-fiscal.nr-nota-fis
    FIELD de-cdd-embarque-ini  LIKE nota-fiscal.cdd-embarq
    FIELD de-cdd-embarque-fim  LIKE nota-fiscal.cdd-embarq
    FIELD da-dt-saida          LIKE nota-fiscal.dt-saida
    FIELD c-hr-saida           LIKE nota-fiscal.hr-confirma
    FIELD banco                AS INTEGER
    FIELD cod-febraban         AS INTEGER
    FIELD cod-portador         AS INTEGER
    FIELD prox-bloq            AS CHAR
    FIELD c-instrucao          AS CHAR EXTENT 5
    FIELD imprime-bloq         AS LOGICAL
    FIELD imprime-bloq-danfe   AS LOGICAL
    FIELD rs-imprime           AS INTEGER
    FIELD impressora-so        AS CHAR
    FIELD impressora-so-bloq   AS CHAR
    FIELD nr-copias            AS INTEGER
    FIELD l-gera-danfe-xml     AS LOGICAL
    FIELD c-dir-hist-xml       AS CHARACTER.
   
DEFINE TEMP-TABLE tt-log-danfe-xml NO-UNDO
    FIELD seq           AS INTEGER
    FIELD c-nr-nota-xml AS CHARACTER
    FIELD c-chave-xml   AS CHARACTER.

DEFINE TEMP-TABLE tt-boletos NO-UNDO
    FIELD id          AS CHARACTER
    FIELD cod_arq     AS CHARACTER
    FIELD cod_arq_aux AS CHARACTER.

/****************** Parametros **********************/

define temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9":U
    field exemplo          as character format "x(30)":U
    index id ordem.

DEF INPUT PARAM raw-param AS RAW NO-UNDO.
DEF INPUT PARAM TABLE FOR tt-raw-digita.

for each tt-raw-digita:
    create tt-digita.
    raw-transfer tt-raw-digita.raw-digita to tt-digita.  
end. 

DEF TEMP-TABLE ttArquivo NO-UNDO
      FIELD sequencia   AS INT
      FIELD nomeArquivo AS CHAR
      FIELD modeloDanfe AS CHAR
      INDEX idx1 sequencia.

DEF NEW GLOBAL SHARED TEMP-TABLE tt-notas-impressas
   FIELD r-nota AS ROWID .

{cdp/cdapi090def.i}

{bcp/bcapi004.i}
{cdp/cd0666.i}
{cdp/cdcfgdis.i}
{include/i-epc200.i ft0527rp}

DEFINE BUFFER b-nota-fiscal FOR nota-fiscal.
DEFINE BUFFER bf-ttArquivo  FOR ttArquivo.

{ftp/ft2010.i1} /* Definicao da temp-table tt-notas-geradas */ 

{ftp/ft0527rp.i1 "NEW"} /* DefiniÁ„o temp-table ttCaracteres como NEW SHARED */
{ftp/ft0527rp.i2}       /* CriaÁ„o registros temp-table ttCaracteres e ttColunasDANFE */

/*Integraá∆o Mais Neg¢cios - Verificaá∆o de funá∆o ativa*/
{dibo/bodi01076.i1}  /*v_log_integr_mng*/
DEFINE VARIABLE lBoleto                         AS LONGCHAR    NO-UNDO.
DEFINE VARIABLE l-bol-mais-negoc                AS LOGICAL     NO-UNDO.
DEFINE VARIABLE h-bodi01076                     AS HANDLE      NO-UNDO.
DEFINE VARIABLE decdmptr                        AS MEMPTR      NO-UNDO.
DEFINE VARIABLE c-nom-arq-boleto-mais-negoc     AS CHARACTER   NO-UNDO.


/****************** INCLUDE COM VARI¡VEIS GLOBAIS *********************/

{utp/ut-glob.i}

/****  Variaveis Compartilhadas  ****/
DEFINE NEW SHARED VAR r-nota     AS ROWID.
DEFINE NEW SHARED VAR c-hr-saida AS CHAR    FORMAT "xx:xx:xx" INIT "000000".
DEFINE NEW SHARED VAR l-dt       AS LOGICAL FORMAT "Sim/Nao"  INIT NO.
/*Definiá∆o da vari†veis para busca no xml*/
DEFINE NEW SHARED VARIABLE c-nr-nota-xml        AS character.
DEFINE NEW SHARED VARIABLE c-chave-xml          AS character.   
DEFINE NEW SHARED VARIABLE l-gera-danfe-xml     AS logical.   
DEFINE NEW SHARED VARIABLE c-cod-dir-histor-xml AS character. 
                       

/***************** Definiáao de Vari†veis de Processamento do Relat¢rio *********************/

DEF VAR h-acomp              AS HANDLE NO-UNDO.
DEF VAR v-cod-destino-impres AS CHAR   NO-UNDO.
DEF VAR v-num-reg-lidos      AS INT    NO-UNDO.
DEF VAR v-num-point          AS INT    NO-UNDO.
DEF VAR v-num-set            AS INT    NO-UNDO.
DEF VAR v-num-linha          AS INT    NO-UNDO.
DEF VAR v-cont-registro      AS INT    NO-UNDO.
DEF VAR v-des-retorno        AS CHAR   NO-UNDO.
DEF VAR v-des-local-layout   AS CHAR   NO-UNDO.
DEF VAR c-arquivo-continua   AS CHAR   NO-UNDO.
DEF VAR lSemWord             AS LOG    NO-UNDO.
DEF VAR c-dir-tmp            AS CHAR   NO-UNDO.

DEF VAR c-dir-spool          AS CHAR   NO-UNDO.

DEF VAR da-dt-saida          AS DATE   NO-UNDO.
DEF VAR c-cod-layout         AS CHAR   NO-UNDO.
DEF VAR l-mais-itens         AS LOG    NO-UNDO INIT NO.

&IF "{&mguni_version}" >= "2.071" &THEN
DEF VAR c-cod-estabel LIKE nota-fiscal.cod-estabel FORMAT "x(05)" INITIAL "" NO-UNDO.
&ELSE
DEF VAR c-cod-estabel LIKE nota-fiscal.cod-estabel FORMAT "X(3)"  INITIAL "" NO-UNDO.
&ENDIF

DEF VAR c-serie       LIKE nota-fiscal.serie       FORMAT "x(5)"  INITIAL "" NO-UNDO.

DEF VAR r-ped-venda     AS ROWID.
DEF VAR r-pre-fat       AS ROWID.
DEF VAR r-emitente      AS ROWID.
DEF VAR r-estabel       AS ROWID.
DEF VAR r-docum-est     AS ROWID.
DEF VAR r-ser-estab     AS ROWID.
DEF VAR r-natur-oper    AS ROWID.
DEF VAR l-tipo-nota     AS LOGICAL FORMAT "Entrada/Saida" NO-UNDO.

DEF VAR i-sit-nota-ini    AS INTEGER NO-UNDO.
DEF VAR i-sit-nota-fim    AS INTEGER NO-UNDO.

DEF VAR lDados            AS LOGICAL INITIAL NO NO-UNDO.
DEF VAR cont              AS INTEGER            NO-UNDO.

DEF VAR c-msg             AS CHAR               NO-UNDO.
DEF VAR c-help            AS CHAR               NO-UNDO.

DEF VAR h-printacrord32   AS HANDLE             NO-UNDO.

DEF VAR c-liter-impressora AS CHAR NO-UNDO.
{utp/ut-liter.i "Enviando_arquivo_para_a_impressora" *}
ASSIGN c-liter-impressora = RETURN-VALUE.

{cdp/cd0590.i} /*tt-comunica */
DEF VAR tp-integ AS CHAR NO-UNDO.

DEFINE STREAM arq-erro.
DEFINE VAR    c-arquivo     AS CHAR FORMAT "X(40)".
DEFINE VARIABLE iSitNotaOri AS INT   NO-UNDO.
CREATE tt-param-aux.
RAW-TRANSFER raw-param TO tt-param-aux.

ASSIGN l-gera-danfe-xml     = tt-param-aux.l-gera-danfe-xml
       c-cod-dir-histor-xml = tt-param-aux.c-dir-hist-xml.

{include/i-rpvar.i}

ASSIGN c-programa     = "FT0527rp":U
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Emissor DANFE - NF-e"
       c-sistema      = "mft"
       c-dir-tmp      = SESSION:TEMP-DIRECTORY.

{varinc/var00002.i}

/*{esp/exportarTabelaCsv3.i tt-param-aux " " " " "ttAUX"}
FIND FIRST tt-param-aux NO-ERROR.*/

FIND FIRST mguni.empresa NO-LOCK
    WHERE mguni.empresa.ep-codigo = i-ep-codigo-usuario NO-ERROR.

IF AVAIL mguni.empresa THEN
   ASSIGN c-empresa  = mguni.empresa.razao-social.
ELSE
   ASSIGN c-empresa = "".
   
/* ========================== Chamada EPC ============================= */
IF c-nom-prog-upc-mg97 <> '':U THEN DO:
   FOR EACH tt-epc WHERE tt-epc.cod-event = "Destino":U :
      DELETE tt-epc.
   END.
   CREATE tt-epc.
   ASSIGN tt-epc.cod-event     = "Destino":U
          tt-epc.cod-parameter = "tt-param-aux.destino":U
          tt-epc.val-parameter = STRING(tt-param-aux.destino).

   {include/i-epc201.i "Destino"}

   FOR FIRST tt-epc WHERE tt-epc.cod-event     = "Destino"
                      AND tt-epc.cod-parameter = "tt-param-aux.destino":
      IF tt-epc.val-parameter <> "" AND
         tt-epc.val-parameter <> ? THEN
         ASSIGN tt-param-aux.destino = INTEGER(tt-epc.val-parameter).
   END.
END.
/* ==================================================================== */



/**** EXECUCAO RELATORIO GRAFICO ****/
CASE tt-param-aux.destino:
    WHEN 1 THEN ASSIGN v-cod-destino-impres = "Impressora".
    WHEN 2 THEN ASSIGN v-cod-destino-impres = "Arquivo".
    OTHERWISE   ASSIGN v-cod-destino-impres = "Terminal".
END CASE.

IF (OPSYS = "WIN32" AND SEARCH("c:/windows/fonts/dc-code128.ttf") = ?)
OR (OPSYS = "UNIX"  AND SEARCH("/usr/share/fonts/dc-code128.ttf") = ?) THEN DO:

    {utp/ut-liter.i "N∆o_foi_poss°vel_realizar_a_impress∆o."}
    ASSIGN c-msg = TRIM(RETURN-VALUE).
    {utp/ut-liter.i "N∆o_foram_encontradas_as_Fontes_True_Type_necess†rias_para_impress∆o_do_DANFE._Gentileza_instalar_todas_as_FTT_contidas_na_pasta_interfac/bcodefont."}
    ASSIGN c-help = TRIM(RETURN-VALUE).

    RUN utp/ut-msgs.p (INPUT "show":U,
                       INPUT 17242,
                       INPUT c-msg + "~~" + c-help).

    RETURN ERROR.
END.

/*run printForm in h-FunctionLibrary.*/

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

RUN pi-inicializar IN h-acomp(INPUT "Acompanhamento Relat¢rio").

ASSIGN v-num-reg-lidos = 0.

FOR EACH tt-notas-impressas:
   DELETE tt-notas-impressas.
END.

IF tt-param-aux.data-exec <> ? THEN
   ASSIGN l-dt = yes.
ELSE 
   ASSIGN l-dt = no.

ASSIGN c-cod-estabel     = tt-param-aux.c-cod-estabel
       c-serie           = tt-param-aux.c-serie
       da-dt-saida       = tt-param-aux.da-dt-saida
       c-hr-saida        = STRING(tt-param-aux.c-hr-saida,"99:99:99")
       c-cod-layout      = tt-param-aux.cod-layout
       l-gera-danfe-xml  = tt-param-aux.l-gera-danfe-xml
       c-nr-nota-fis-ini = c-nr-nota-fis-ini
       c-nr-nota-fis-fim = c-nr-nota-fis-fim.

IF c-nr-nota-fis-fim = "" THEN
    ASSIGN c-nr-nota-fis-fim = "ZZZZZZZZZZZZZZZZ". 

IF tt-param-aux.rs-imprime = 1 THEN
   ASSIGN i-sit-nota-ini = 1
          i-sit-nota-fim = 1.
ELSE
   ASSIGN i-sit-nota-ini = 2
          i-sit-nota-fim = 7.

/* Seguranáa por estabelecimento */
&scoped-define TTONLY YES    
{include/i-estab-security.i}

IF tt-param-aux.imprime-bloq THEN DO:
    CREATE tt-digita.
    ASSIGN tt-digita.ordem = 4.
    
    CREATE tt-digita.
    ASSIGN tt-digita.ordem  = 2
           tt-digita.exemplo = "2".
END.

FIND FIRST tt-digita NO-LOCK
     WHERE tt-digita.ordem = 1 NO-ERROR.
 
IF l-estab-security-active = YES THEN DO:   
   FOR EACH {&ESTAB-SEC-TT} NO-LOCK,
       EACH nota-fiscal NO-LOCK
      WHERE nota-fiscal.cod-estabel   = c-cod-estabel
        AND nota-fiscal.cod-estabel   = {&ESTAB-SEC-TT-FIELD}
        AND nota-fiscal.serie         = c-serie
        AND nota-fiscal.nr-nota-fis  >= c-nr-nota-fis-ini
        AND nota-fiscal.nr-nota-fis  <= c-nr-nota-fis-fim
        AND nota-fiscal.cdd-embarq   >= de-cdd-embarque-ini
        AND nota-fiscal.cdd-embarq   <= de-cdd-embarque-fim
        AND nota-fiscal.ind-sit-nota >= i-sit-nota-ini
        AND nota-fiscal.ind-sit-nota <= i-sit-nota-fim
        BREAK BY nota-fiscal.cod-estabel
              BY nota-fiscal.serie
              BY nota-fiscal.nr-nota-fis:
    
    IF nota-fiscal.idi-sit-nf-eletro = 0 THEN NEXT.

    IF  tt-digita.exemplo <> "" 
    AND nota-fiscal.user-calc <> tt-digita.exemplo THEN NEXT.

      &IF DEFINED (bf_dis_nfe) &THEN
         &IF "{&bf_dis_versao_ems}" >= "2.07" &THEN
            IF ((nota-fiscal.idi-forma-emis-nf-eletro  = 1       /* Tipo de Emiss∆o   = Normal                  */
            OR   nota-fiscal.idi-forma-emis-nf-eletro  = 6       /* Tipo de Emiss∆o   = SVC-AN                  */
            OR   nota-fiscal.idi-forma-emis-nf-eletro  = 7 )     /* Tipo de Emiss∆o   = SVC-RS                  */          
            AND  nota-fiscal.idi-sit-nf-eletro        <> 3 )     /* SituaÁ„o da nota <> Uso Autorizado          */
             
            OR (nota-fiscal.idi-forma-emis-nf-eletro  = 4        /* Tipo de Emiss„o   = Contingencia EPEC       */
            AND nota-fiscal.idi-sit-nf-eletro        <> 15       /* SituaÁ„o da nota <> EPEC recebido pelo SCE  */
            AND nota-fiscal.idi-sit-nf-eletro        <> 3 )      /* SituaÁ„o da nota <> Uso Autorizado          */

            OR (nota-fiscal.idi-forma-emis-nf-eletro <> 1        /* Tipo de Emiss„o  <> Normal                  */
            AND nota-fiscal.idi-forma-emis-nf-eletro <> 4        /* Tipo de Emiss„o  <> Contingencia EPEC       */
            AND nota-fiscal.idi-sit-nf-eletro         = 5 )      /* Situaá∆o da nota  = Documento Cancelado     */

            /*OR (nota-fiscal.idi-forma-emis-nf-eletro <> 2        /* Tipo de Emiss∆o  <> Contingencia FS         */
            AND nota-fiscal.idi-forma-emis-nf-eletro <> 5        /* Tipo de Emiss∆o  <> Contingencia FS-DA      */
            AND nota-fiscal.idi-sit-nf-eletro        <> 3 )*/ THEN /* Situaá∆o da nota <> Uso Autorizado          */
               NEXT.
         &ELSE
            IF SUBSTR(nota-fiscal.char-2,65,2) = '' THEN 
               NEXT.
            IF SUBSTR(nota-fiscal.char-2,65,2) = '1' THEN DO: /*Tp Emis 1 = Normal*/
               FIND FIRST sit-nf-eletro NO-LOCK
                    WHERE sit-nf-eletro.cod-estabel   = nota-fiscal.cod-estabel
                      AND sit-nf-eletro.cod-serie     = nota-fiscal.serie      
                      AND sit-nf-eletro.cod-nota-fisc = nota-fiscal.nr-nota-fis NO-ERROR.
               IF NOT AVAIL sit-nf-eletro OR sit-nf-eletro.idi-sit-nf-eletro <> 3 THEN /*Sit 3 = Uso Autorizado*/
                  NEXT.
            END.
            IF SUBSTR(nota-fiscal.char-2,65,2) = '4' THEN DO: /*Tp Emis 4 = Contingància EPEC*/
               FIND FIRST sit-nf-eletro NO-LOCK
                    WHERE sit-nf-eletro.cod-estabel   = nota-fiscal.cod-estabel
                      AND sit-nf-eletro.cod-serie     = nota-fiscal.serie      
                      AND sit-nf-eletro.cod-nota-fisc = nota-fiscal.nr-nota-fis NO-ERROR.
               IF NOT AVAIL sit-nf-eletro OR (sit-nf-eletro.idi-sit-nf-eletro <> 15 AND sit-nf-eletro.idi-sit-nf-eletro <> 3) THEN /*Sit 15 = EPEC recebido pelo SCE*/
                  NEXT.
            END.
         &ENDIF
      &ENDIF
      
      IF c-nom-prog-upc-mg97 <> '':U THEN DO:
         FOR EACH tt-epc WHERE tt-epc.cod-event = "IGNORAR-NOTA-FISCAL":U :
            DELETE tt-epc.
         END.
         CREATE tt-epc.
         ASSIGN tt-epc.cod-event     = "IGNORAR-NOTA-FISCAL":U
                tt-epc.cod-parameter = "Rowid_NotaFiscal":U
                tt-epc.val-parameter = STRING(ROWID(nota-fiscal)).
      
         {include/i-epc201.i "IGNORAR-NOTA-FISCAL"}
      
         if RETURN-VALUE = "NOK":U then
             NEXT.
      END.
   
      
      /* SE ESTIVER MARCADO PARA GERAR DANFE PELO XML, MONTA O NOME DOS ARQUIVOS COM BASE: 
          1-c-nr-nota-xml - Estabelecimento, SÇrie e Nro. da nota 
          2-c-chave-xml   - Chave de Acesso */
      IF l-gera-danfe-xml THEN  DO: 

          RUN cdp/cd0360b.p (INPUT nota-fiscal.cod-estabel,
                             INPUT "NF-e",
                             OUTPUT tp-integ).
       
          if  tp-integ = "TC2" OR tp-integ = "TPF" then do:
              
              bloco_leitura:
              FOR EACH integr-totvs-colab WHERE
                      integr-totvs-colab.cod-edi   = "170" AND
                      integr-totvs-colab.cod-docto = nota-fiscal.cod-chave-aces-nf-eletro AND
                      integr-totvs-colab.log-lido  = yes NO-LOCK:
              
                  EMPTY TEMP-TABLE tt-comunica.
                  {cdp/cd0590.i2 "tt-comunica" "integr-totvs-colab"}
                  
                  if  tt-comunica.cStat = "100" then  do:
                       assign c-nr-nota-xml = trim(ENTRY(2, integr-totvs-colab.cod-msg, "|")).
                       leave bloco_leitura.
                  end.
              end.
          end.
          else do:

              ASSIGN c-nr-nota-xml = TRIM(nota-fiscal.cod-estabel) +                              
                                     SUBSTR(nota-fiscal.cod-chave-aces-nf-eletro,23,3) + 
                                     TRIM(STRING(INTEGER(SUBSTR(nota-fiscal.cod-chave-aces-nf-eletro,26,9)),">>9999999")) + ".XML"
                                 
                     c-chave-xml = TRIM(nota-fiscal.cod-chave-aces-nf-eletro) + ".XML". 
          END.
                   
         /* SE N«O ENCONTRAR O ARQUIVO .XML NO DIRET‡RIO VAI PARA O PR‡XIMO REGISTRO */    
         IF((SEARCH(c-cod-dir-histor-xml + "/" + c-nr-nota-xml) = ? AND
             SEARCH(c-cod-dir-histor-xml + "/" + c-chave-xml) = ?)) THEN DO:   
             
             /*CRIA TEMP-TABLE COM INFORMAÄÂES DAS NOTAS EM 
               QUE O XML N«O FOI ENCONTRADO NO DIRET‡RIO*/
             ASSIGN cont = cont + 1.  
             CREATE tt-log-danfe-xml.
             ASSIGN tt-log-danfe-xml.seq           = cont
                    tt-log-danfe-xml.c-nr-nota-xml = TRIM(nota-fiscal.nr-nota-fis)
                    tt-log-danfe-xml.c-chave-xml   = nota-fiscal.cod-chave-aces-nf-eletro.           
   
             NEXT.
         END.
      END.
   
      ASSIGN lDados = YES.
      /* Inicio -- Projeto Internacional */
      {utp/ut-liter.i "Gerando_DANFE_para_nota" *}
      RUN pi-acompanhar IN h-acomp(RETURN-VALUE + " " + nota-fiscal.cod-estabel + "/" + nota-fiscal.serie + "/" + nota-fiscal.nr-nota-fis).
   
      ASSIGN v-num-reg-lidos = v-num-reg-lidos + 1.
      
      IF FIRST-OF(nota-fiscal.nr-nota-fis) THEN DO:
         ASSIGN v-cont-registro = 0.
      END.
   
      IF FIRST-OF(nota-fiscal.nr-nota-fis) THEN DO:
          RUN pi-imprime-nota.
      END.
   END.
END.
ELSE DO:    
   FOR EACH {&ESTAB-SEC-TT} NO-LOCK,
       EACH nota-fiscal NO-LOCK
      WHERE nota-fiscal.cod-estabel   = c-cod-estabel
        AND nota-fiscal.serie         = c-serie
        AND nota-fiscal.nr-nota-fis  >= c-nr-nota-fis-ini
        AND nota-fiscal.nr-nota-fis  <= c-nr-nota-fis-fim
        AND nota-fiscal.cdd-embarq   >= de-cdd-embarque-ini
        AND nota-fiscal.cdd-embarq   <= de-cdd-embarque-fim
        AND nota-fiscal.ind-sit-nota >= i-sit-nota-ini
        AND nota-fiscal.ind-sit-nota <= i-sit-nota-fim
        BREAK BY nota-fiscal.cod-estabel
              BY nota-fiscal.serie
              BY nota-fiscal.nr-nota-fis:
    ASSIGN iSitNotaOri = nota-fiscal.ind-sit-nota.     
    IF nota-fiscal.idi-sit-nf-eletro = 0 THEN NEXT.

    IF  tt-digita.exemplo     <> "" 
    AND nota-fiscal.user-calc <> tt-digita.exemplo THEN NEXT.
    
     

    &IF DEFINED (bf_dis_nfe) &THEN
        &IF "{&bf_dis_versao_ems}" >= "2.07" &THEN
            IF ((nota-fiscal.idi-forma-emis-nf-eletro  = 1       /* Tipo de Emiss∆o   = Normal                  */
            OR   nota-fiscal.idi-forma-emis-nf-eletro  = 6       /* Tipo de Emiss∆o   = SVC-AN                  */
            OR   nota-fiscal.idi-forma-emis-nf-eletro  = 7 )     /* Tipo de Emiss∆o   = SVC-RS                  */          
            AND  nota-fiscal.idi-sit-nf-eletro        <> 3 )     /* SituaÁ„o da nota <> Uso Autorizado          */

            OR (nota-fiscal.idi-forma-emis-nf-eletro  = 4        /* Tipo de Emiss„o   = Contingencia EPEC       */
            AND nota-fiscal.idi-sit-nf-eletro        <> 15       /* SituaÁ„o da nota <> EPEC recebido pelo SCE  */
            AND nota-fiscal.idi-sit-nf-eletro        <> 3 )      /* SituaÁ„o da nota <> Uso Autorizado          */

            OR (nota-fiscal.idi-forma-emis-nf-eletro <> 1        /* Tipo de Emiss„o  <> Normal                  */
            AND nota-fiscal.idi-forma-emis-nf-eletro <> 4        /* Tipo de Emiss„o  <> Contingencia EPEC       */
            AND nota-fiscal.idi-sit-nf-eletro         = 5 )      /* Situaá∆o da nota  = Documento Cancelado     */                          
            /*OR (nota-fiscal.idi-forma-emis-nf-eletro <> 2        /* Tipo de Emiss∆o  <> Contingencia FS         */
            AND nota-fiscal.idi-forma-emis-nf-eletro <> 5        /* Tipo de Emiss∆o  <> Contingencia FS-DA      */
            AND nota-fiscal.idi-sit-nf-eletro        <> 3 )*/ THEN /* Situaá∆o da nota <> Uso Autorizado          */
               NEXT.
        &ELSE            
            IF SUBSTR(nota-fiscal.char-2,65,2) = '' THEN 
               NEXT.
            IF SUBSTR(nota-fiscal.char-2,65,2) = '1' THEN DO: /*Tp Emis 1 = Normal*/
               FIND FIRST sit-nf-eletro NO-LOCK
                    WHERE sit-nf-eletro.cod-estabel   = nota-fiscal.cod-estabel
                      AND sit-nf-eletro.cod-serie     = nota-fiscal.serie      
                      AND sit-nf-eletro.cod-nota-fisc = nota-fiscal.nr-nota-fis NO-ERROR.
               IF NOT AVAIL sit-nf-eletro OR sit-nf-eletro.idi-sit-nf-eletro <> 3 THEN /*Sit 3 = Uso Autorizado*/
                  NEXT.
            END.
            IF SUBSTR(nota-fiscal.char-2,65,2) = '4' THEN DO: /*Tp Emis 4 = Contingància EPEC*/
               FIND FIRST sit-nf-eletro NO-LOCK
                    WHERE sit-nf-eletro.cod-estabel   = nota-fiscal.cod-estabel
                      AND sit-nf-eletro.cod-serie     = nota-fiscal.serie      
                      AND sit-nf-eletro.cod-nota-fisc = nota-fiscal.nr-nota-fis NO-ERROR.
               IF NOT AVAIL sit-nf-eletro OR (sit-nf-eletro.idi-sit-nf-eletro <> 15 AND sit-nf-eletro.idi-sit-nf-eletro <> 3) THEN /*Sit 15 = EPEC recebido pelo SCE*/
                  NEXT.
            END.
         &ENDIF
      &ENDIF
      
     
      
      IF c-nom-prog-upc-mg97 <> '':U THEN DO:
         FOR EACH tt-epc WHERE tt-epc.cod-event = "IGNORAR-NOTA-FISCAL":U :
            DELETE tt-epc.
         END.
         CREATE tt-epc.
         ASSIGN tt-epc.cod-event     = "IGNORAR-NOTA-FISCAL":U
                tt-epc.cod-parameter = "Rowid_NotaFiscal":U
                tt-epc.val-parameter = STRING(ROWID(nota-fiscal)).
      
         {include/i-epc201.i "IGNORAR-NOTA-FISCAL"}
      
         if RETURN-VALUE = "NOK":U then
             NEXT.
      END.
   
      /* SE ESTIVER MARCADO PARA GERAR DANFE PELO XML, MONTA O NOME DOS ARQUIVOS COM BASE: 
          1-c-nr-nota-xml - Estabelecimento, SÇrie e Nro. da nota 
          2-c-chave-xml   - Chave de Acesso */
      IF l-gera-danfe-xml THEN  DO: 
          RUN cdp/cd0360b.p (INPUT nota-fiscal.cod-estabel,
                             INPUT "NF-e",
                             OUTPUT tp-integ).
       
          if  tp-integ = "TC2" OR tp-integ = "TPF" then do:
              
              bloco_leitura:
              FOR EACH integr-totvs-colab WHERE
                      integr-totvs-colab.cod-edi   = "170" AND
                      integr-totvs-colab.cod-docto = nota-fiscal.cod-chave-aces-nf-eletro AND
                      integr-totvs-colab.log-lido  = yes NO-LOCK:
              
                  EMPTY TEMP-TABLE tt-comunica.
                  {cdp/cd0590.i2 "tt-comunica" "integr-totvs-colab"}
                  
                  if  tt-comunica.cStat = "100" then  do:
                       assign c-nr-nota-xml = trim(ENTRY(2, integr-totvs-colab.cod-msg, "|")).
                       leave bloco_leitura.
                  end.
              end.
          end.
          else do:

              ASSIGN c-nr-nota-xml = TRIM(nota-fiscal.cod-estabel) +                              
                                     SUBSTR(nota-fiscal.cod-chave-aces-nf-eletro,23,3) + 
                                     TRIM(STRING(INTEGER(SUBSTR(nota-fiscal.cod-chave-aces-nf-eletro,26,9)),">>9999999")) + ".XML"
                                 
                     c-chave-xml = TRIM(nota-fiscal.cod-chave-aces-nf-eletro) + ".XML". 
          END.
                   
         /* SE N«O ENCONTRAR O ARQUIVO .XML NO DIRET‡RIO VAI PARA O PR‡XIMO REGISTRO */    
         IF((SEARCH(c-cod-dir-histor-xml + "/" + c-nr-nota-xml) = ? AND
             SEARCH(c-cod-dir-histor-xml + "/" + c-chave-xml) = ?)) THEN DO:   
             
             /*CRIA TEMP-TABLE COM INFORMAÄÂES DAS NOTAS EM 
               QUE O XML N«O FOI ENCONTRADO NO DIRET‡RIO*/
             ASSIGN cont = cont + 1.  
             CREATE tt-log-danfe-xml.
             ASSIGN tt-log-danfe-xml.seq           = cont
                    tt-log-danfe-xml.c-nr-nota-xml = TRIM(nota-fiscal.nr-nota-fis)
                    tt-log-danfe-xml.c-chave-xml   = nota-fiscal.cod-chave-aces-nf-eletro.           
   
             NEXT.
         END.
      END.
   
   
      ASSIGN lDados = YES.
      /* Inicio -- Projeto Internacional */
      {utp/ut-liter.i "Gerando_DANFE_para_nota" *}
      RUN pi-acompanhar IN h-acomp(RETURN-VALUE + " " + nota-fiscal.cod-estabel + "/" + nota-fiscal.serie + "/" + nota-fiscal.nr-nota-fis).
   
      ASSIGN v-num-reg-lidos = v-num-reg-lidos + 1.
      
      IF FIRST-OF(nota-fiscal.nr-nota-fis) THEN DO:
         ASSIGN v-cont-registro = 0.
      END.
   
      IF FIRST-OF(nota-fiscal.nr-nota-fis) THEN DO:         
         RUN pi-imprime-nota.
      END.
   END.
END.
 


 
/* Inicio -- Projeto Internacional */
{utp/ut-liter.i "Gerando_Arquivo_Final" *}
RUN pi-acompanhar in h-acomp(RETURN-VALUE).

IF CAN-FIND (FIRST ser-estab
             WHERE ser-estab.cod-estabel = c-cod-estabel
               AND ser-estab.serie       = c-serie
               AND &IF "{&bf_dis_versao_ems}":U >= "2.08":U &THEN
                       ser-estab.log-word-danfe
                   &ElSE
                       substring(ser-estab.char-1,70,1) = "S":U
                   &ENDIF) THEN
    ASSIGN lSemWord = YES.

IF lDados = YES THEN DO:
    RUN piJuntaArquivos.
    RUN piJuntaArquivosDanfeSimplificado.
END.
ELSE DO:
    {utp/ut-liter.i "N∆o_foi_poss°vel_realizar_a_impress∆o."}
    ASSIGN c-msg = TRIM(RETURN-VALUE).
    {utp/ut-liter.i "N∆o_h†_dados_para_imprimir."}
    ASSIGN c-help = TRIM(RETURN-VALUE).

    RUN utp/ut-msgs.p (INPUT "show":U,
                       INPUT 53082,
                       INPUT c-msg + "~~" + c-help).
END.
/* Inicio -- Projeto Internacional */
{utp/ut-liter.i "Abrindo_Documento_DANFE" *}
RUN pi-acompanhar in h-acomp(RETURN-VALUE).

IF VALID-HANDLE(h-acomp) THEN /*gr9030g*/
    RUN pi-finalizar IN h-acomp NO-ERROR.

/*Cria Log de mlx n∆o encontrados*/
IF CAN-FIND(FIRST tt-log-danfe-xml) THEN DO:                       
    RUN  ftp/ft0527g.p(INPUT raw-param, 
                       INPUT TABLE tt-raw-digita, 
                       INPUT TABLE tt-log-danfe-xml).
    
    IF i-num-ped-exec-rpw = 0 THEN
       run utp/ut-msgs.p (INPUT "show":U,
                          INPUT 53366,
                          INPUT SESSION:TEMP-DIRECTORY + "FT0527_log.txt").
END.

IF c-nom-prog-upc-mg97 <> '':U THEN DO:
   FOR EACH tt-epc WHERE tt-epc.cod-event = "FIM-IMPRESSAO":U :
      DELETE tt-epc.
   END.
   CREATE tt-epc.
   ASSIGN tt-epc.cod-event     = "FIM-IMPRESSAO":U
          tt-epc.cod-parameter = "handle_ttParam":U
          tt-epc.val-parameter = STRING(TEMP-TABLE tt-param-aux:HANDLE).

   {include/i-epc201.i "FIM-IMPRESSAO"}
END.

RETURN 'OK'.

/* Procedure para impressao da nota fiscal */
PROCEDURE pi-imprime-nota:
   /* data de saida da nota fiscal */
   ASSIGN r-nota = rowid(nota-fiscal).

   {cdp/cdapi090a.i SCI CREATE nota-fiscal TP8}

   IF l-dt = YES AND string(da-dt-saida) <> "" AND da-dt-saida <> ? THEN DO:
      FIND FIRST b-nota-fiscal
           WHERE rowid(b-nota-fiscal) = rowid(nota-fiscal) EXCLUSIVE-LOCK no-error.

      IF b-nota-fiscal.dt-saida = ? THEN
          ASSIGN b-nota-fiscal.dt-saida = da-dt-saida.
    
      {cdp/cdapi090a.i SCI CREATE b-nota-fiscal TP9}

      RELEASE b-nota-fiscal.
    END.

   FIND FIRST ped-venda
        WHERE ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
          AND ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
          NO-LOCK NO-ERROR.

   FIND FIRST estabelec
        WHERE estabelec.cod-estabel = nota-fiscal.cod-estabel
        NO-LOCK NO-ERROR.

   FIND FIRST emitente
        WHERE emitente.nome-abrev = nota-fiscal.nome-ab-cli
        NO-LOCK NO-ERROR.

   FIND FIRST pre-fatur 
        WHERE pre-fatur.cdd-embarq   = nota-fiscal.cdd-embarq
        AND   pre-fatur.nome-abrev   = nota-fiscal.nome-ab-cli
        AND   pre-fatur.nr-pedcli    = nota-fiscal.nr-pedcli
        AND   pre-fatur.nr-resumo    = nota-fiscal.nr-resumo
        NO-LOCK NO-ERROR.

   FIND FIRST natur-oper
        WHERE natur-oper.nat-operacao = nota-fiscal.nat-operacao
        NO-LOCK NO-ERROR.

   FIND FIRST ser-estab NO-LOCK
        WHERE ser-estab.cod-estabel = c-cod-estabel
          AND ser-estab.serie       = c-serie NO-ERROR.

   ASSIGN r-estabel    = rowid(estabelec)
          r-ped-venda  = rowid(ped-venda)
          r-emitente   = rowid(emitente)
          r-natur-oper = rowid(natur-oper)
          r-ser-estab  = ROWID(ser-estab)
          r-pre-fat    = IF AVAIL pre-fatur THEN
                             rowid(pre-fatur)
                         ELSE ?
          l-tipo-nota  = no.

   IF (&IF "{&bf_dis_versao_ems}" >= "2.071" &THEN
            TRIM(estabelec.des-vers-layout)
        &ELSE
            TRIM(SUBSTRING(estabelec.char-1,173,10)
        &ENDIF >= "4.00") THEN DO:
        /*MESSAGE 'ft0527f4'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
        RUN ftp/ft0527f4.p (INPUT-OUTPUT TABLE ttArquivo).        
            
    END.
    ELSE DO:
    /*MESSAGE 'ft0527f3'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
        RUN ftp/ft0527f3.p (INPUT-OUTPUT TABLE ttArquivo).
    END.

   /* muda o status da nota-fiscal */
   IF nota-fiscal.ind-sit-nota <> 2 THEN DO:
    /*  MESSAGE 'sit.nota:' SKIP
           nota-fiscal.ind-sit-nota
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
      RUN ftp/ft0503a.p.
   END.
      
   /*FIND FIRST ttArquivo NO-ERROR.
   MESSAGE 'ponto 1:' ttArquivo.nomeArquivo SKIP
   VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.  */
   
            
   CREATE tt-notas-impressas.
   ASSIGN tt-notas-impressas.r-nota = rowid(nota-fiscal).

   /* GERA ETIQUETAS PARA O MODULO DE COLETA DE DADOS */

   IF  AVAIL param-global
   AND param-global.modulo-cl
   AND ( nota-fiscal.ind-tip-nota = 2) /* tipo de nota-fiscal Manual */
   THEN DO:     
      CREATE tt-prog-bc.
      ASSIGN tt-prog-bc.cod-prog-dtsul        = "ft0513"
             tt-prog-bc.cod-versao-integracao = 1
             tt-prog-bc.usuario               = tt-param-aux.usuario
             tt-prog-bc.opcao                 = 1.

      RUN bcp/bcapi004.p (INPUT-OUTPUT TABLE tt-prog-bc,
                          INPUT-OUTPUT TABLE tt-erro).

      FIND FIRST tt-prog-bc NO-ERROR.

      ASSIGN c-arquivo = tt-prog-bc.nome-dir-etiq + "/" + c-arquivo.

      IF return-value = "OK" THEN DO:
         {utp/ut-liter.i Gerando_Etiquetas  MRE R}
         RUN pi-acompanhar in h-acomp (INPUT RETURN-VALUE).

         erro:
         DO ON stop     UNDO erro,LEAVE erro
            ON quit     UNDO erro,LEAVE erro
            ON error    UNDO erro,LEAVE erro
            ON endkey   UNDO erro,LEAVE erro:

            RUN value(tt-prog-bc.prog-criacao)(INPUT tt-prog-bc.cd-trans,
                                               INPUT rowid(nota-fiscal),
                                               INPUT-OUTPUT TABLE tt-erro) NO-ERROR.

            IF ERROR-STATUS:ERROR 
            OR (    ERROR-STATUS:GET-NUMBER(1) <> 138
                AND ERROR-STATUS:NUM-MESSAGES  <> 0)
            THEN DO:
               OUTPUT STREAM arq-erro TO VALUE(c-arquivo) APPEND.

               {utp/ut-liter.i Ocorreu_na_Geraá∆o_de_Etiquetas_-_Progress MRE R}
               PUT STREAM arq-erro "***" RETURN-VALUE SKIP.
               {utp/ut-liter.i Programa * R}
               PUT STREAM arq-erro ERROR-STATUS:GET-MESSAGE(1) SKIP.
               PUT STREAM arq-erro RETURN-VALUE ": " tt-prog-bc.prog-criacao SKIP.
               PUT STREAM arq-erro nota-fiscal.serie                           AT 1.
               PUT STREAM arq-erro nota-fiscal.nr-nota-fis                     AT 7.
               PUT STREAM arq-erro nota-fiscal.cod-estabel                     AT 24.

               OUTPUT STREAM arq-erro CLOSE.
            END.

            IF RETURN-VALUE = "NOK" THEN DO:
               FIND FIRST tt-erro NO-ERROR.
               IF AVAIL tt-erro
               THEN DO:
                  OUTPUT STREAM arq-erro TO VALUE(c-arquivo) APPEND.

                  {utp/ut-liter.i Ocorreu_na_Geraá∆o_de_Etiquetas MRE R}
                  PUT STREAM arq-erro "***" RETURN-VALUE SKIP.
                  FOR EACH tt-erro:
                     PUT STREAM arq-erro SKIP tt-erro.cd-erro " - " tt-erro.mensagem.
                  END.
                  PUT STREAM arq-erro SKIP.
                  OUTPUT STREAM arq-erro CLOSE.
               END.
            END.
         END.
      END.
      ELSE DO:
         /**** caso tenha integraá∆o com o coleta e ocorreu erros ***/
         FIND FIRST tt-erro NO-ERROR.
         IF AVAIL tt-erro THEN DO:
            OUTPUT STREAM arq-erro TO VALUE(c-arquivo) APPEND.

            {utp/ut-liter.i Ocorreu_na_Geraá∆o_de_Etiquetas MRE R}
            PUT STREAM arq-erro "***" RETURN-VALUE SKIP.
            FOR EACH tt-erro:
                PUT stream arq-erro skip tt-erro.cd-erro " - " tt-erro.mensagem.
            END.
            PUT STREAM arq-erro SKIP.
            OUTPUT STREAM arq-erro CLOSE.
         END.
      END.
   END.
   
   
   FIND FIRST ttArquivo
   WHERE ttArquivo.sequencia = int(nota-fiscal.nr-nota-fis) -  INT(c-nr-nota-fis-ini) + 1 NO-ERROR.
   IF AVAIL ttArquivo THEN DO:
      RUN esapi/sincrArqDanfeNF.p(ROWID(nota-fiscal),ttArquivo.nomeArquivo).
      
   END.
   /*MESSAGE iSitNotaOri
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
   IF iSitNotaOri = 1 THEN
   DO:
      /*MESSAGE 'vou mandar o email'
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
      RUN esapi/enviarEmailNF.p(ROWID(nota-fiscal),'') .
   END.
/* ========================== Chamada EPC ============================= */
         IF c-nom-prog-dpc-mg97  <> ""
         OR c-nom-prog-appc-mg97 <> "" 
         OR c-nom-prog-upc-mg97  <> ""  then do:
            FOR EACH tt-epc 
                WHERE tt-epc.cod-event = "Pacote_boleto":
                DELETE tt-epc.
            END.
            {include/i-epc201.i "Pacote_boleto  "}
             /* Sem tratamento de retorno */
         END.
/* ==================================================================== */
 
    ASSIGN l-bol-mais-negoc = NO.
    FOR FIRST nota-fisc-mais-negoc NO-LOCK
        WHERE nota-fisc-mais-negoc.cod-estabel   = nota-fiscal.cod-estabel
          AND nota-fisc-mais-negoc.cod-serie     = nota-fiscal.serie
          AND nota-fisc-mais-negoc.cod-nota-fisc = nota-fiscal.nr-nota-fis:
    END.
    IF  tt-param-aux.imprime-bloq THEN DO:
        /*Nota Fiscal Mais Neg¢cios*/
        IF  TRIM(SUBSTRING(nota-fiscal.char-2,257,1)) = "S":U
        AND AVAIL nota-fisc-mais-negoc
        AND NOT nota-fisc-mais-negoc.log-livre-1 THEN DO:
            ASSIGN l-bol-mais-negoc = YES.
            IF  nota-fisc-mais-negoc.idi-status-autoriz = 2 THEN DO:                
                /* Nota Fiscal com ConfirmaÁ„o de entrega n„o aprovada na Supplier */
                IF  nota-fisc-mais-negoc.log-livre-4
                AND nota-fisc-mais-negoc.num-livre-2 < 4  THEN
                     IF LOG-MANAGER:LOGGING-LEVEL > 2 THEN DO:
                        LOG-MANAGER:WRITE-MESSAGE(">>>> Impress∆o do Boleto Mais Neg¢cios n∆o realizada.":U).
                        LOG-MANAGER:WRITE-MESSAGE(">>>> Confirmaá∆o de Entrega n∆o est† 'Aprovada' na Plataforma Mais Neg¢cios":U).
                        LOG-MANAGER:WRITE-MESSAGE(">>>> Nota Fiscal Mais Neg¢cios - Estab: ":U + nota-fiscal.cod-estabel + " SÇrie: ":U + nota-fiscal.serie + " N£mero NF: " + nota-fiscal.nr-nota-fis).
                    END.   
                    
                /*o conte£do do boleto j† encontra-se em PDF porÇm criptografado*/                
                IF  nota-fisc-mais-negoc.blb-contdo-boleto-seg-via <> ? THEN
                    COPY-LOB nota-fisc-mais-negoc.blb-contdo-boleto-seg-via TO lBoleto NO-CONVERT NO-ERROR.
                ELSE 
                    IF nota-fisc-mais-negoc.blb-contdo-boleto <> ? THEN
                        COPY-LOB nota-fisc-mais-negoc.blb-contdo-boleto TO lBoleto NO-CONVERT NO-ERROR.                    
                                                 
                IF  lBoleto = ""  THEN
                    IF LOG-MANAGER:LOGGING-LEVEL > 2 THEN DO:
                        LOG-MANAGER:WRITE-MESSAGE(">>>> Impress∆o do Boleto Mais Neg¢cios n∆o realizada.":U).
                        LOG-MANAGER:WRITE-MESSAGE(">>>> O Boleto gerado na plataforma Mais Neg¢cios n∆o encontra-se atualizado para a Nota Fiscal ":U).
                    END.                         
            END.

            IF  nota-fisc-mais-negoc.idi-status-autoriz = 1 THEN /*Pendente Aprovaá∆o na Plataforma Mais Neg¢cios*/
                IF LOG-MANAGER:LOGGING-LEVEL > 2 THEN DO:
                    LOG-MANAGER:WRITE-MESSAGE(">>>> Impress∆o do Boleto Mais Neg¢cios n∆o realizada.":U).
                    LOG-MANAGER:WRITE-MESSAGE(">>>> Nota Fiscal Mais Neg¢cios encontra-se com situaá∆o 'Pendente' na Plataforma Mais Neg¢cios":U).
                    LOG-MANAGER:WRITE-MESSAGE(">>>> Nota Fiscal Mais Neg¢cios - Estab: ":U + nota-fiscal.cod-estabel + " SÇrie: ":U + nota-fiscal.serie + " N£mero NF: " + nota-fiscal.nr-nota-fis).
                END.
            /* Nota Fiscal com ConfirmaÁ„o de entrega cancelada na Supplier e ativa no Datasul, imprime boleto normal */    
            IF  nota-fisc-mais-negoc.log-livre-4
            AND nota-fisc-mais-negoc.num-livre-2 > 4 THEN 
                ASSIGN l-bol-mais-negoc = NO
                       lBoleto          = "".            
        END.                
        
        IF  lBoleto = "" 
        AND NOT l-bol-mais-negoc THEN DO:
            RUN ftp/ft0518h.p (INPUT-OUTPUT TABLE tt-param-aux,
                               INPUT              r-nota, 
                               INPUT-OUTPUT TABLE ttArquivo, 
                               INPUT        TABLE tt-digita).
            
            IF NOT AVAIL tt-param-aux THEN
                FIND FIRST tt-param-aux NO-ERROR.        
        END.
        ELSE            
            IF lBoleto <> "" THEN DO:
               decdmptr = BASE64-DECODE(lBoleto).

                FOR LAST bf-ttArquivo: END.
                CREATE ttArquivo.
                ASSIGN ttArquivo.sequencia   = IF AVAIL bf-ttArquivo THEN bf-ttArquivo.sequencia + 1 ELSE 1.
                ASSIGN ttArquivo.nomeArquivo = TRIM(nota-fiscal.cod-estabel) + "-"        + 
                                               TRIM(nota-fiscal.serie)       + "-"        + 
                                               TRIM(nota-fiscal.nr-nota-fis) + "-boleto-01" + 
                                               ".pdf".
                COPY-LOB FROM decdmptr TO FILE SESSION:TEMP-DIRECTORY + ttArquivo.nomeArquivo.
                IF NOT AVAIL tt-param-aux THEN
                    FIND FIRST tt-param-aux NO-ERROR.
                ASSIGN lBoleto = "".    
            END.
    END. 
    /*FIND FIRST ttArquivo NO-ERROR.
    MESSAGE 'ponto 2.1:' ttArquivo.nomeArquivo SKIP
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
END PROCEDURE.

PROCEDURE OpenDocument:
   DEF INPUT PARAM c-doc  AS CHAR NO-UNDO.

   DEFINE VARIABLE c-exec AS CHAR NO-UNDO.
   DEFINE VARIABLE h-Inst AS INT  NO-UNDO.

   ASSIGN c-exec = FILL("x",255).
   RUN FindExecutableA (INPUT c-doc,
                        INPUT "",
                        INPUT-OUTPUT c-exec,
                        OUTPUT h-inst).

   IF h-inst >= 0 AND h-inst <= 32 THEN
      RUN ShellExecuteA (INPUT 0,
                         INPUT "open",
                         INPUT "rundll32.exe",
                         INPUT "shell32.dll,OpenAs_RunDLL " + c-doc,
                         INPUT "",
                         INPUT 1,
                         OUTPUT h-inst).

   RUN ShellExecuteA (INPUT 0,
                      INPUT "open",
                      INPUT c-doc,
                      INPUT "",
                      INPUT "",
                      INPUT 1,
                      OUTPUT h-inst).

   IF h-inst < 0 OR h-inst > 32 THEN RETURN "OK".
   ELSE RETURN "NOK".
END PROCEDURE.

PROCEDURE FindExecutableA EXTERNAL "Shell32.dll" persistent:
   DEFINE INPUT        PARAMETER lpFile      AS CHAR NO-UNDO.
   DEFINE INPUT        PARAMETER lpDirectory AS CHAR NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER lpResult    AS CHAR NO-UNDO.
   DEFINE RETURN       PARAMETER hInstance   AS LONG.
END.

PROCEDURE ShellExecuteA EXTERNAL "Shell32.dll" persistent:
   DEFINE INPUT  PARAMETER hwnd         AS LONG.
   DEFINE INPUT  PARAMETER lpOperation  AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER lpFile       AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER lpParameters AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER lpDirectory  AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER nShowCmd     AS LONG.
   DEFINE RETURN PARAMETER hInstance    AS LONG.
END PROCEDURE.

PROCEDURE piJuntaArquivos:
    DEFINE VARIABLE dt-data        	       AS DATE      NO-UNDO.
    DEFINE VARIABLE c-hora         	       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE c-arq          	       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v_cod_arq      	       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v_cod_arq_aux  	       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE c-fullpath     	       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE i-numero-copia 	       AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE c-command-java         AS CHARACTER          NO-UNDO.
    DEFINE VARIABLE l-possui-java          AS LOGICAL INITIAL NO NO-UNDO.
    DEFINE VARIABLE i-total-notas          AS INTEGER            NO-UNDO.

    IF CAN-FIND(FIRST ttArquivo WHERE ttArquivo.modeloDanfe <> "5") THEN
        ASSIGN dt-data                  = TODAY
               c-hora                   = STRING(TIME,"HH:MM:SS")
               c-arq                    = "FT0527" + REPLACE(STRING(dt-data), "/", "") + REPLACE(c-hora, ":", "") + ".pdf"
               i-total-notas            = 0.
    ELSE 
        RETURN.

    /* Salva na variavel a lista de Danfes que nao sao formato Simplificado */
    FOR EACH ttArquivo WHERE ttArquivo.modeloDanfe <> "5":
        ASSIGN i-total-notas = i-total-notas + 1.            
        DO i-numero-copia = 1 TO tt-param-aux.nr-copias:
            ASSIGN v_cod_arq = v_cod_arq + c-dir-tmp + ttArquivo.nomeArquivo + " ".
        END.
    END.
    
    /* Busca o nome do primeiro Danfe gerado */
    FOR FIRST ttArquivo WHERE ttArquivo.modeloDanfe <> "5":
        ASSIGN v_cod_arq_aux = c-dir-tmp + ttArquivo.nomeArquivo.
    END.
    
    ASSIGN v_cod_arq_aux  = REPLACE(v_cod_arq_aux, ".pdf", "-m.pdf")
           c-fullpath     = SEARCH("ftp/Multivalent20060102.jar":U)
           c-command-java = "java -version 2> " + c-dir-tmp + "command-java-temp.txt".

    RUN pi-verifica-instalacao-java (INPUT c-command-java, OUTPUT l-possui-java).

    IF tt-param-aux.destino = 1 THEN /* impressora */
        RUN ftp/printacrord32.p PERSISTENT SET h-printacrord32.

    /* Se possui JAVA */
    IF l-possui-java AND c-fullpath <> ? THEN DO:
    
        RUN pi-executa-junta-arquivos (INPUT c-fullpath, INPUT v_cod_arq, v_cod_arq_aux, INPUT c-arq).
        
        RUN pi-envia-portal (INPUT FALSE).

        /* Apaga Danfes individuais */ 
        FOR EACH ttArquivo WHERE ttArquivo.modeloDanfe <> "5":
           OS-DELETE VALUE(c-dir-tmp + ttArquivo.nomeArquivo) NO-ERROR.
        END.
        
        RUN pi-trata-arquivo-conforme-destino(INPUT c-arq).
    END.      
    ELSE /* se Nao tem JAVA - Se nao possui java instalado e Se nao for encontrado o .Jar, nao executa a juncao dos arquivos e abre todos os Danfes gerados */
        RUN pi-trata-arquivos-separados (INPUT FALSE, i-total-notas).

    IF VALID-HANDLE(h-printacrord32) THEN DO:
        DELETE PROCEDURE h-printacrord32.
        ASSIGN h-printacrord32 = ?.
    END.

END PROCEDURE.

PROCEDURE piJuntaArquivosDanfeSimplificado:
    DEFINE VARIABLE dt-data        	AS DATE               NO-UNDO.
    DEFINE VARIABLE c-hora         	AS CHARACTER          NO-UNDO.
    DEFINE VARIABLE c-fullpath     	AS CHARACTER          NO-UNDO.
    DEFINE VARIABLE i-numero-copia 	AS INTEGER            NO-UNDO.
    DEFINE VARIABLE c-command-java  AS CHARACTER          NO-UNDO.
    DEFINE VARIABLE l-possui-java   AS LOGICAL INITIAL NO NO-UNDO.
    DEFINE VARIABLE i-total-notas   AS INTEGER            NO-UNDO.
    
    DEFINE VARIABLE c-arq-danfe-simpl            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE c-arq-danfe-simpl-boleto     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v_cod_arq_danfe_simpl        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v_cod_arq_danfe_simpl_boleto AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v_cod_arq_danfe_aux  	       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v_cod_arq_boleto_aux  	     AS CHARACTER NO-UNDO.
    
    IF CAN-FIND(FIRST ttArquivo WHERE ttArquivo.modeloDanfe = "5") THEN    
        ASSIGN dt-data                  = TODAY
               c-hora                   = STRING(TIME,"HH:MM:SS")
               i-total-notas            = 0
               c-arq-danfe-simpl        = "FT0527" + REPLACE(STRING(dt-data), "/", "") + "-DANFE-Simplificado-"        + REPLACE(c-hora, ":", "") + ".pdf"
               c-arq-danfe-simpl-boleto = "FT0527" + REPLACE(STRING(dt-data), "/", "") + "-DANFE-Simplificado-Boleto-" + REPLACE(c-hora, ":", "") + ".pdf".
    ELSE 
        RETURN.

    /* Salva nas variaveis a lista de Danfes emitidos no formato Simplificado, com seus Boletos */
    FOR EACH ttArquivo WHERE ttArquivo.modeloDanfe = "5":
        ASSIGN i-total-notas = i-total-notas + 1.

        DO i-numero-copia = 1 TO tt-param-aux.nr-copias:

            IF ttArquivo.nomeArquivo MATCHES("*boleto*") THEN
                ASSIGN v_cod_arq_danfe_simpl_boleto = v_cod_arq_danfe_simpl_boleto + c-dir-tmp + ttArquivo.nomeArquivo + " ".
            ELSE
                ASSIGN v_cod_arq_danfe_simpl = v_cod_arq_danfe_simpl + c-dir-tmp + ttArquivo.nomeArquivo + " ".
        END.
    END.
    
    /* Busca o nome do primeiro Danfe Simplificado gerado */
    FOR FIRST ttArquivo WHERE ttArquivo.modeloDanfe = "5" AND NOT ttArquivo.nomeArquivo MATCHES("*boleto*"):
        ASSIGN v_cod_arq_danfe_aux = c-dir-tmp + ttArquivo.nomeArquivo.
    END.

    /* Busca o nome do primeiro boleto de Danfe Simplificado */
    FOR FIRST ttArquivo WHERE ttArquivo.modeloDanfe = "5" AND ttArquivo.nomeArquivo MATCHES("*boleto*"):
        ASSIGN v_cod_arq_boleto_aux = c-dir-tmp + ttArquivo.nomeArquivo.
    END.
    
    ASSIGN v_cod_arq_danfe_aux  = REPLACE(v_cod_arq_danfe_aux, ".pdf", "-m.pdf")
           v_cod_arq_boleto_aux = REPLACE(v_cod_arq_boleto_aux, ".pdf", "-m.pdf")
           c-fullpath           = SEARCH("ftp/Multivalent20060102.jar":U)
           c-command-java       = "java -version 2> " + c-dir-tmp + "command-java-temp.txt".

    RUN pi-verifica-instalacao-java (INPUT c-command-java, OUTPUT l-possui-java).

    IF tt-param-aux.destino = 1 THEN /* impressora */
        RUN ftp/printacrord32.p PERSISTENT SET h-printacrord32.

    /* Se possui JAVA */
    IF l-possui-java AND c-fullpath <> ? THEN DO:
    
        /* quando Danfe for simplificado, boletos devem ser gerados em arquivos separados */
        RUN pi-executa-junta-arquivos (INPUT c-fullpath, INPUT v_cod_arq_danfe_simpl       , v_cod_arq_danfe_aux , INPUT c-arq-danfe-simpl).
        RUN pi-executa-junta-arquivos (INPUT c-fullpath, INPUT v_cod_arq_danfe_simpl_boleto, v_cod_arq_boleto_aux, INPUT c-arq-danfe-simpl-boleto).
        
        RUN pi-envia-portal (INPUT TRUE). /* l-arquivos-danfe-simpl */

        /* Apaga Danfes individuais */ 
        FOR EACH ttArquivo WHERE ttArquivo.modeloDanfe = "5":
           OS-DELETE VALUE(c-dir-tmp + ttArquivo.nomeArquivo) NO-ERROR.
        END.
        
        RUN pi-trata-arquivo-conforme-destino(INPUT c-arq-danfe-simpl).
        RUN pi-trata-arquivo-conforme-destino(INPUT c-arq-danfe-simpl-boleto).
    END.      
    ELSE /* se Nao tem JAVA - Se nao possui java instalado e Se nao for encontrado o .Jar, nao executa a juncao dos arquivos e abre todos os Danfes gerados */
        RUN pi-trata-arquivos-separados (INPUT TRUE, i-total-notas). /* l-arquivos-danfe-simpl */

    IF VALID-HANDLE(h-printacrord32) THEN DO:
        DELETE PROCEDURE h-printacrord32.
        ASSIGN h-printacrord32 = ?.
    END.

END PROCEDURE.

PROCEDURE piEnviaArquivoImpressora:

    DEF INPUT PARAM c-arquivo AS CHAR NO-UNDO.

    DEFINE VARIABLE c-impres AS CHARACTER NO-UNDO.

    RUN pi-acompanhar in h-acomp(c-liter-impressora). /* Enviando arquivo para a impressora */

    ASSIGN tt-param-aux.impressora-so = SUBSTR(tt-param-aux.impressora-so,1,INDEX(tt-param-aux.impressora-so,":") - 1).
            
    FIND FIRST imprsor_usuar 
         WHERE imprsor_usuar.nom_impressora = TRIM(tt-param-aux.impressora-so)
           AND imprsor_usuar.cod_usuario    = tt-param-aux.usuario NO-LOCK NO-ERROR.

    IF OPSYS = "WIN32" THEN DO:
        ASSIGN c-impres = 'print /d:' + imprsor_usuar.nom_disposit_so + ' ' + c-arquivo.
        DOS SILENT VALUE(c-impres).
    END.
    ELSE DO:
        IF INDEX (imprsor_usuar.nom_disposit_so," in session") > 0 THEN
            RUN printAcroRd32 IN h-printacrord32 (INPUT c-arquivo, INPUT imprsor_usuar.nom_disposit_so + " on " + SESSION:PRINTER-PORT).
        ELSE 
            RUN printAcroRd32 IN h-printacrord32 (INPUT c-arquivo, INPUT imprsor_usuar.nom_disposit_so).
    END.

END PROCEDURE.

PROCEDURE pi-verifica-instalacao-java:
    DEFINE INPUT  PARAM c-command-java AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAM l-possui-java  AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE     c-result-java  AS CHARACTER NO-UNDO.

    /* verificar se o usuario tem o java instalado */
    OUTPUT TO VALUE(c-dir-tmp + "command-java-temp.bat").
       PUT UNFORMATTED c-command-java.
    OUTPUT CLOSE.
    
    /* Executa o .bat criado */
    OS-COMMAND SILENT VALUE(c-dir-tmp + "command-java-temp.bat").
    
    INPUT FROM VALUE(c-dir-tmp + "command-java-temp.txt").
    REPEAT:
        IMPORT UNFORMATTED c-result-java.
        IF c-result-java MATCHES('*version*') THEN
            ASSIGN l-possui-java = YES.
    END.
    INPUT CLOSE.
    
    OS-DELETE VALUE(c-dir-tmp + "command-java-temp.bat") NO-ERROR.
    OS-DELETE VALUE(c-dir-tmp + "command-java-temp.txt") NO-ERROR.
    
END PROCEDURE.

PROCEDURE pi-executa-junta-arquivos:
    DEFINE INPUT PARAM c-fullpath             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAM c-arquivos-para-juntar AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAM c-nome-arquivo-tmp     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAM c-nome-arquivo-final   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE c-command-line AS CHARACTER NO-UNDO.

    ASSIGN c-command-line = "java  -classpath ":U  + c-fullpath + " tool.pdf.Merge -verbose " + c-arquivos-para-juntar.

    IF LENGTH(c-command-line) >= 2000 THEN DO:
        /* Para evitar o erro "** OS escap COMMAND too long. (379)" */
        OUTPUT TO VALUE(c-dir-tmp + "command-line-temp.bat").
            PUT UNFORMATTED c-command-line.
        OUTPUT CLOSE.

        /* Executa o .bat criado */
        OS-COMMAND SILENT VALUE(c-dir-tmp + "command-line-temp.bat").

        /* Elimina o .bat */
        OS-DELETE VALUE(c-dir-tmp + "command-line-temp.bat") NO-ERROR.
    END.
    ELSE DO:
        /* Executa .Jar respons†vel pela juná∆o dos Danfes */
        OS-COMMAND SILENT VALUE(c-command-line).
    END.

    /* Renomeia o arquivo gerado com todos os Danfes para o padr∆o */
    OS-RENAME VALUE(c-nome-arquivo-tmp) VALUE(c-dir-tmp + c-nome-arquivo-final).

END PROCEDURE.

PROCEDURE pi-trata-arquivos-separados:
    DEFINE INPUT PARAM l-arquivos-danfe-simpl AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAM i-total-notas          AS INTEGER NO-UNDO.
    
    FOR EACH ttArquivo 
       WHERE (IF l-arquivos-danfe-simpl
              THEN ttArquivo.modeloDanfe  = "5"
              ELSE ttArquivo.modeloDanfe <> "5") EXCLUSIVE-LOCK:              

        IF tt-param-aux.destino = 3 THEN DO: /* destino terminal */
            RUN OpenDocument(c-dir-tmp + ttArquivo.nomeArquivo). 
        END.
        ELSE IF tt-param-aux.destino = 2 THEN DO: /* destino arquivo */
            IF i-total-notas > 1 THEN
                /* se for uma faixa eu sou obrigada a colocar a sequencia no nome do arquivo 
                   pois como n∆o tem o java para juntar os arquivos em 1 s¢
                   eu n∆o teria como salvar v†rios arquivos com o mesmo nome no diret¢rio */
                OS-COPY VALUE (c-dir-tmp + ttArquivo.nomeArquivo) VALUE (SUBSTRING(tt-param-aux.arquivo, 1, R-INDEX(tt-param-aux.arquivo,"/")) + STRING(ttArquivo.sequencia) + "-" + SUBSTRING(tt-param-aux.arquivo, R-INDEX(tt-param-aux.arquivo,"/") + 1, LENGTH(tt-param-aux.arquivo))).
            ELSE
                /* se n∆o Ç uma faixa eu simplesmente pego o que o usu†rio
                   informou na tela na opá∆o arquivo */
                OS-COPY VALUE (c-dir-tmp + ttArquivo.nomeArquivo) VALUE (tt-param-aux.arquivo).

            OS-DELETE VALUE (c-dir-tmp + ttArquivo.nomeArquivo).
        END.
        ELSE /* destino impressora */
            RUN piEnviaArquivoImpressora (INPUT c-dir-tmp + ttArquivo.nomeArquivo).

    END.
END PROCEDURE.

PROCEDURE pi-envia-portal:
    DEFINE INPUT PARAM l-arquivos-danfe-simpl AS LOGICAL NO-UNDO.

    DEFINE VARIABLE c-cod-estabel LIKE nota-fiscal.cod-estabel NO-UNDO.
    DEFINE VARIABLE c-serie       LIKE nota-fiscal.serie       NO-UNDO.
    DEFINE VARIABLE c-nr-nota-fis LIKE nota-fiscal.nr-nota-fis NO-UNDO.	 

    FIND FIRST portal-param
         WHERE portal-param.cod-param = "portal-pasta-arquivos-danfe-nfe" NO-LOCK NO-ERROR.
    
    IF AVAIL portal-param AND portal-param.cod-val-param <> "" THEN DO:
   
      RUN juntaBoletosPortal.
       
      FOR EACH ttArquivo 
         WHERE ENTRY(4, ttArquivo.nomeArquivo, "-") <> 'boleto'
	   AND (IF l-arquivos-danfe-simpl 
	   	THEN ttArquivo.modeloDanfe  = "5"
		ELSE ttArquivo.modeloDanfe <> "5"):

          ASSIGN c-cod-estabel = ENTRY(1, ttArquivo.nomeArquivo, "-")
                 c-serie       = ENTRY(2, ttArquivo.nomeArquivo, "-")
                 c-nr-nota-fis = ENTRY(3, ttArquivo.nomeArquivo, "-").

          OS-COPY VALUE (c-dir-tmp + ttArquivo.nomeArquivo) VALUE (portal-param.cod-val-param).
          OS-COPY VALUE(portal-param.cod-val-param + ttArquivo.nomeArquivo) VALUE(portal-param.cod-val-param + c-cod-estabel + c-serie + c-nr-nota-fis + ".pdf").
          OS-DELETE VALUE(portal-param.cod-val-param + ttArquivo.nomeArquivo) NO-ERROR.
      END.
    END.

END PROCEDURE.

PROCEDURE pi-trata-arquivo-conforme-destino:
    DEFINE INPUT PARAM c-nome-arquivo-final   AS CHARACTER NO-UNDO.

    IF  tt-param-aux.destino = 3 THEN DO: /* Abre em tela se a opcao for destino terminal */
        RUN OpenDocument(c-dir-tmp + c-nome-arquivo-final).
    END.
    ELSE IF tt-param-aux.destino = 2 THEN DO: /* Muda para o diret¢rio informado se for destino arquivo */
        IF i-num-ped-exec-rpw <> 0 THEN DO:

            FOR FIRST ped_exec NO-LOCK
                WHERE ped_exec.num_ped_exec = i-num-ped-exec-rpw:
              FOR FIRST servid_exec NO-LOCK
                  WHERE servid_exec.cod_servid_exec = ped_exec.cod_servid_exec:
                    ASSIGN c-dir-spool = servid_exec.nom_dir_spool.
              END.
            END.

            IF c-dir-spool = "" THEN
                ASSIGN c-dir-spool = SESSION:TEMP-DIRECTORY.

            IF SUBSTRING(c-dir-spool,LENGTH(c-dir-spool),1) = "/" OR SUBSTRING(c-dir-spool,LENGTH(c-dir-spool),1) = "\" THEN
                ASSIGN OVERLAY(c-dir-spool,LENGTH(c-dir-spool),1) = "".
               
            FOR FIRST usuar_mestre NO-LOCK
                WHERE usuar_mestre.cod_usuario = ped_exec.cod_usuario:
                IF usuar_mestre.nom_subdir_spool_rpw <> "" THEN
                    ASSIGN c-dir-spool = c-dir-spool + '/' + usuar_mestre.nom_subdir_spool_rpw.
            END.

            IF SUBSTRING(c-dir-spool,LENGTH(c-dir-spool),1) = "/" OR SUBSTRING(c-dir-spool,LENGTH(c-dir-spool),1) = "\" THEN
                ASSIGN OVERLAY(c-dir-spool,LENGTH(c-dir-spool),1) = "".

            ASSIGN tt-param-aux.arquivo = c-dir-spool + '/' + tt-param-aux.arquivo.
        END.
        
        OS-COPY   VALUE (c-dir-tmp + c-nome-arquivo-final) VALUE (tt-param-aux.arquivo).
        OS-DELETE VALUE (c-dir-tmp + c-nome-arquivo-final).
    END.
    ELSE DO: /* destino impressora */
        OS-RENAME VALUE(c-dir-tmp + c-nome-arquivo-final) VALUE(tt-param-aux.arquivo).

        RUN piEnviaArquivoImpressora (INPUT tt-param-aux.arquivo).
    END.

END PROCEDURE.

PROCEDURE juntaBoletosPortal:

    DEFINE VARIABLE c-dir-tmp       AS CHARACTER            NO-UNDO.
    DEFINE VARIABLE c-fullpath      AS CHARACTER            NO-UNDO.
    DEFINE VARIABLE c-command-line  AS CHARACTER            NO-UNDO.
    DEFINE VARIABLE idBoleto        AS CHARACTER            NO-UNDO.

    EMPTY TEMP-TABLE tt-boletos.
    ASSIGN c-dir-tmp  = SESSION:TEMP-DIRECTORY
           c-fullpath = SEARCH("ftp/Multivalent20060102.jar":U).

    /* Salva temp-table uma lista de Boletos */
    FOR EACH ttArquivo
        WHERE ENTRY(4, ttArquivo.nomeArquivo, "-") = 'boleto':

        ASSIGN idBoleto = ENTRY(1, ttArquivo.nomeArquivo, "-") + "-" + ENTRY(2, ttArquivo.nomeArquivo, "-") + "-" + ENTRY(3, ttArquivo.nomeArquivo, "-").

        FOR FIRST tt-boletos
            WHERE tt-boletos.id = idBoleto:
            ASSIGN tt-boletos.cod_arq = tt-boletos.cod_arq + c-dir-tmp + ttArquivo.nomeArquivo + " ".            
        END.

        IF NOT AVAIL tt-boletos THEN DO:
            CREATE tt-boletos.
            ASSIGN tt-boletos.id            = idBoleto
                   tt-boletos.cod_arq     = c-dir-tmp + ttArquivo.nomeArquivo + " "
                   tt-boletos.cod_arq_aux = c-dir-tmp + ttArquivo.nomeArquivo.
        END.
    END.

    FOR EACH tt-boletos:
    
        ASSIGN tt-boletos.cod_arq_aux = REPLACE(tt-boletos.cod_arq_aux, ".pdf", "-m.pdf")
               c-command-line           = "java  -classpath ":U  + c-fullpath + " tool.pdf.Merge -verbose " + tt-boletos.cod_arq.  
            
        IF LENGTH(c-command-line) >= 2000 THEN DO:
            /* Para evitar o erro "** OS escap COMMAND too long. (379)" */
            OUTPUT TO VALUE(c-dir-tmp + "command-line-temp.bat").
                PUT UNFORMATTED c-command-line.
            OUTPUT CLOSE.
            
            /* Executa o .bat criado */
            OS-COMMAND SILENT VALUE(c-dir-tmp + "command-line-temp.bat").  
        
            /* Elimina o .bat */
            OS-DELETE VALUE(c-dir-tmp + "command-line-temp.bat") NO-ERROR.
        END.
        ELSE DO:
            /* Executa .Jar respons†vel pela juná∆o dos Danfes */   
            OS-COMMAND SILENT VALUE(c-command-line).
        END.
    
        // Deleta se j† existir um arquivo com o mesmo nome
        OS-DELETE VALUE(c-dir-tmp + 'boleto-portal-' + tt-boletos.id + '.pdf') NO-ERROR.
    
        /* Renomeia o arquivo gerado com todos os Danfes para o padr∆o */
        OS-RENAME VALUE(tt-boletos.cod_arq_aux) VALUE(c-dir-tmp + 'boleto-portal-' + tt-boletos.id + '.pdf').
        OS-COPY VALUE (c-dir-tmp + 'boleto-portal-' + tt-boletos.id + '.pdf') VALUE (portal-param.cod-val-param).
    END.

END PROCEDURE.

/* fim do programa */
