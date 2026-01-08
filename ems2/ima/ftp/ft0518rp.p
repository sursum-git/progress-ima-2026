/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i FT0518RP 2.00.00.000}  /*** 010000 ***/
{esinc/i-propath.i}

&IF "{&EMSFND_VERSION}" >= "1.00" &THEN
    {include/i-license-manager.i ft0518rp MFT}
&ENDIF

DEF BUFFER empresa FOR mgcad.empresa.

/* TON 21/12
DEF TEMP-TABLE tt-gati-nfe NO-UNDO LIKE gati-nfe.*/

/*****************************************************************************
**       Programa: FT0518rp.p
**       Data....: 14/03/07
**       Autor...: DATASUL S.A.
**       Objetivo: Emissor DANFE - NF-e
**       Vers∆o..: 1.00.000 - super
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/

/*define variable c-prog-gerado as character no-undo initial "FT0518rp".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.*/

/****************** Definiá∆o de Tabelas Tempor†rias do Relat¢rio **********************/
DEF TEMP-TABLE tt-notas-fiscais NO-UNDO LIKE nota-fiscal
    FIELD r-rowid AS ROWID 
    FIELD c-arq-xml AS CHAR.


define temp-table tt-raw-digita
    field raw-digita as raw.

define temp-table tt-param-aux
    field destino              as integer
    field destino-bloq         as integer
    field arquivo              as char
    field arquivo-bloq         as char
    field usuario              as char
    field data-exec            as date
    field hora-exec            as integer
    field parametro            as logical
    field formato              as integer
    field cod-layout           as character
    field des-layout           as character
    field log-impr-dados       as logical  
    field v_num_tip_aces_usuar as integer
    field ep-codigo            LIKE empresa.ep-codigo
    field c-cod-estabel        like nota-fiscal.cod-estabel
    field c-serie              like nota-fiscal.serie
    field c-nr-nota-fis-ini    like nota-fiscal.nr-nota-fis
    field c-nr-nota-fis-fim    like nota-fiscal.nr-nota-fis
    field de-cdd-embarque-ini  like nota-fiscal.cdd-embarq
    field de-cdd-embarque-fim  like nota-fiscal.cdd-embarq
    field da-dt-saida          like nota-fiscal.dt-saida
    field c-hr-saida           like nota-fiscal.hr-confirma
    field banco                as integer
    field cod-febraban         as integer      
    field cod-portador         as integer      
    field prox-bloq            as char         
    FIELD c-instrucao          AS CHAR EXTENT 5
    field imprime-bloq         as logical
    field imprime-bloq-danfe   as logical
    field rs-imprime           as INTEGER
    FIELD impressora-so        AS CHAR
    FIELD impressora-so-bloq   AS CHAR
    FIELD nr-copias            AS INTEGER
    FIELD l-gera-danfe-xml     AS LOG
    FIELD c-dir-hist-xml       AS CHAR.

define temp-table tt-log-danfe-xml NO-UNDO
    field seq           as int
    field c-nr-nota-xml as character
    field c-chave-xml   as character.

DEF TEMP-TABLE tt-doc 
    FIELD caminho-doc AS CHAR
    FIELD caminho-pdf AS CHAR
    FIELD nr-nota-fis AS CHAR
    FIELD serie       AS CHAR
    FIELD cod-estabel AS CHAR.

/****************** Parametros **********************/

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

DEF TEMP-TABLE ttArquivo NO-UNDO
      FIELD sequencia AS INT
      FIELD nomeArquivo AS CHAR
      INDEX idx1 sequencia.

/* n„o coloque NO-UNDO nessa shared temp-table */
define new shared temp-table tt-notas-impressas field r-nota as rowid. 


DEFINE VARIABLE ch-pdfx            AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE ch-work            AS COM-HANDLE  NO-UNDO.
DEFINE VARIABLE AppWord            AS COM-HANDLE  NO-UNDO.


/* TON 21/12
{bcp/bcapi004.i}
{cdp/cd0666.i}
{cdp/cdcfgdis.i}
*/

/*************************
*   definicao de buffer
*************************/
def buffer b-nota-fiscal for nota-fiscal.
                                                              
{ftp/ft2010.i1} /* Definicao da temp-table tt-notas-geradas */ 

{ftp/ft0518rp.i1 "NEW"} /* Definiá∆o temp-table ttCaracteres como NEW SHARED */
{ftp/ft0518rp.i2}       /* Criaá∆o registros temp-table ttCaracteres e ttColunasDANFE */

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

{utp/ut-glob.i}

/****  Variaveis Compartilhadas  ****/
DEFINE NEW SHARED VAR r-nota       AS ROWID.
DEFINE NEW SHARED VAR c-hr-saida   AS CHAR    FORMAT "xx:xx:xx" INIT "000000".
DEFINE NEW SHARED VAR l-dt         AS LOGICAL FORMAT "Sim/Nao"  INIT NO.
                       
/*Variavel definida na EPC-FT0518 */
DEF NEW GLOBAL SHARED VAR h-ed-justif AS HANDLE NO-UNDO.

/*Definiá∆o da vari†veis para busca no xml*/
DEFINE NEW SHARED VARIABLE c-nr-nota-xml        AS character.
DEFINE NEW SHARED VARIABLE c-chave-xml          AS character.   
DEFINE NEW SHARED VARIABLE l-gera-danfe-xml     AS logical.   
DEFINE NEW SHARED VARIABLE c-cod-dir-histor-xml AS character. 
                       
// Vari†veis para Envio de E-mail, criadas dinamicamente na EPC do ft0518
DEF NEW GLOBAL SHARED VAR h-cb-envia-email AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-ed-dest-email  AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-ed-copia-email AS HANDLE NO-UNDO.


/***************** Definiáao de Vari†veis de Processamento do Relat¢rio *********************/

def var h-acomp              as handle no-undo.
def var v-cod-destino-impres as char   no-undo.
def var v-num-reg-lidos      as int    no-undo.
def var v-num-point          as int    no-undo.
def var v-num-set            as int    no-undo.
def var v-num-linha          as int    no-undo.
def var v-cont-registro      as int    no-undo.
def var v-des-retorno        as char   no-undo.
def var v-des-local-layout   as char   no-undo.
def var c-arquivo-continua   as char   no-undo.
DEF VAR lSemWord             AS LOG    NO-UNDO.

DEF VAR da-dt-saida          AS DATE   NO-UNDO.
DEF VAR c-cod-layout         AS CHAR   NO-UNDO.
DEF VAR l-mais-itens         AS LOG    NO-UNDO INIT NO.
def var c-cod-estabel        like nota-fiscal.cod-estabel format "x(05)"       initial "" no-undo.
def var c-serie              like nota-fiscal.serie       format "x(5)"       initial "" no-undo.
def var r-ped-venda          as rowid.
def var r-pre-fat            as rowid.
def var r-emitente           as rowid.
def var r-estabel            as rowid.
def var r-docum-est          as rowid.
DEF VAR r-ser-estab          AS ROWID.
def var r-natur-oper         as rowid.
def var l-tipo-nota          as logical format "Entrada/Saida" no-undo.
/*def var de-conv        as decimal format ">>>>9.99".
def var de-conv-pis    as decimal format ">>>>9.99".
def var de-conv-cofins as decimal format ">>>>9.99".
def var de-conv-total  as decimal format ">>>>9.99".*/
def var i-sit-nota-ini    as integer.
def var i-sit-nota-fim    as integer.

DEFINE VARIABLE l-enviar-email     AS LOGICAL     NO-UNDO.

define stream arq-erro.
def var c-arquivo as char format "X(40)".

create tt-param-aux.
raw-transfer raw-param to tt-param-aux.

FIND param-nf-estab WHERE
     param-nf-estab.cod-estabel = tt-param-aux.c-cod-estabel NO-LOCK NO-ERROR.

ASSIGN l-gera-danfe-xml = tt-param-aux.l-gera-danfe-xml
       c-cod-dir-histor-xml = param-nf-estab.cod-caminho-xml.

IF VALID-HANDLE(h-cb-envia-email) THEN
   ASSIGN l-enviar-email = h-cb-envia-email:SCREEN-VALUE <> 'Nenhum'.

{include/i-rpvar.i}

assign c-programa     = "FT0518rp":U
       c-versao       = "2.00"
       c-revisao      = ".00.000"
       c-titulo-relat = "Emissor DANFE - NF-e"
       c-sistema      = "mft".

{varinc/var00002.i}

run utp/ut-acomp.p persistent set h-acomp.

find first mguni.empresa no-lock
    where mguni.empresa.ep-codigo = i-ep-codigo-usuario no-error.
if  avail mguni.empresa
then
    assign c-empresa  = mguni.empresa.razao-social.
else
    assign c-empresa = "".

/* run setConstant in h-FunctionLibrary ("cCompany", c-empresa).                */
/* run setConstant in h-FunctionLibrary ("cReportTitle", c-titulo-relat).       */
/* run setConstant in h-FunctionLibrary ("cSystem", c-sistema).                 */
/* run setConstant in h-FunctionLibrary ("cProgram", c-programa).               */
/* run setConstant in h-FunctionLibrary ("cVersion", c-versao).                 */
/* run setConstant in h-FunctionLibrary ("cRevision", c-revisao).               */
/* run setConstant in h-FunctionLibrary ("cCurrentUser", tt-param-aux.usuario). */
/* run setConstant in h-FunctionLibrary ("cActualDate", STRING(TODAY)).         */
/* run setConstant in h-FunctionLibrary ("cActualHour", STRING(TIME)).          */

/**** EXECUCAO RELATORIO GRAFICO ****/
case tt-param-aux.destino:
    when 1 then assign v-cod-destino-impres = "Impressora".
    when 2 then assign v-cod-destino-impres = "Arquivo".
    otherwise   assign v-cod-destino-impres = "Terminal".
end case.

/*run printForm in h-FunctionLibrary.*/

RUN utp/ut-acomp.p persistent set h-acomp.

RUN pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

ASSIGN v-num-reg-lidos = 0.

FOR EACH tt-notas-impressas:
    DELETE tt-notas-impressas.
END.

IF tt-param-aux.data-exec <> ? THEN 
   ASSIGN l-dt = YES.
ELSE 
   ASSIGN l-dt = NO.

ASSIGN c-cod-estabel = tt-param-aux.c-cod-estabel
       c-serie       = tt-param-aux.c-serie
       da-dt-saida   = tt-param-aux.da-dt-saida
       c-hr-saida    = STRING(tt-param-aux.c-hr-saida,"99:99:99")
       c-cod-layout  = tt-param-aux.cod-layout.

IF tt-param-aux.rs-imprime = 1 THEN
   ASSIGN i-sit-nota-ini = 1
          i-sit-nota-fim = 1.
ELSE 
   ASSIGN i-sit-nota-ini = 2
          i-sit-nota-fim = 7.


IF tt-param-aux.rs-imprime = 2 AND 
   VALID-HANDLE(h-ed-justif) THEN DO.
    IF LENGTH(h-ed-justif:SCREEN-VALUE) <= 20 THEN DO.
       RUN pi-finalizar IN h-acomp NO-ERROR.
       MESSAGE 'Justificativa deve conter pelo menos 20 Caracteres' SKIP
               'Nota Fiscal n∆o poder∆o ser ReImpressa...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       RETURN 'ADM-ERROR'.
    END.
END.

for each nota-fiscal no-lock
   where nota-fiscal.cod-estabel   = c-cod-estabel      
     and nota-fiscal.serie         = c-serie
     and nota-fiscal.nr-nota-fis  >= c-nr-nota-fis-ini
     and nota-fiscal.nr-nota-fis  <= c-nr-nota-fis-fim
     and nota-fiscal.cdd-embarq   >= de-cdd-embarque-ini 
     and nota-fiscal.cdd-embarq   <= de-cdd-embarque-fim 
     and nota-fiscal.ind-sit-nota >= i-sit-nota-ini
     and nota-fiscal.ind-sit-nota <= i-sit-nota-fim  
     break by nota-fiscal.cod-estabel
           by nota-fiscal.serie
           by nota-fiscal.nr-nota-fis:


    IF nota-fiscal.cod-protoc = '' THEN NEXT.
    IF nota-fiscal.dt-cancela <> ? THEN NEXT.

    IF tt-param-aux.rs-imprime = 2 THEN DO.
       CREATE ext-his-nota-fiscal.
       ASSIGN ext-his-nota-fiscal.cod-estabel = nota-fiscal.cod-estabel 
              ext-his-nota-fiscal.serie = nota-fiscal.serie 
              ext-his-nota-fiscal.nr-nota-fis = nota-fiscal.nr-nota-fis
              ext-his-nota-fiscal.data = TODAY
              ext-his-nota-fiscal.hora = TIME
              ext-his-nota-fiscal.usuario = c-seg-usuario.
       IF VALID-HANDLE(h-ed-justif) THEN DO.
          ASSIGN ext-his-nota-fiscal.narrativa = "Nota Fiscal Reimpressa com a Justificativa: " +
                                                  h-ed-justif:SCREEN-VALUE.

          ASSIGN h-ed-justif:SCREEN-VALUE = "".
       END.
    END.

    /*
    IF (nota-fiscal.idi-forma-emis-nf-eletro  = 1        /* Tipo de Emiss∆o   = Normal                  */
   AND  nota-fiscal.idi-sit-nf-eletro        <> 3 )      /* Situaá∆o da nota <> Uso Autorizado          */ 
    OR (nota-fiscal.idi-forma-emis-nf-eletro  = 4        /* Tipo de Emiss∆o   = Contingencia DPEC       */
   AND  nota-fiscal.idi-sit-nf-eletro        <> 15       /* Situaá∆o da nota <> DPEC recebido pelo SCE  */
   AND  nota-fiscal.idi-sit-nf-eletro        <> 3 )      /* Situaá∆o da nota <> Uso Autorizado          */ 
    OR (nota-fiscal.idi-forma-emis-nf-eletro <> 1        /* Tipo de Emiss∆o  <> Normal                  */
   AND  nota-fiscal.idi-forma-emis-nf-eletro <> 4        /* Tipo de Emiss∆o  <> Contingencia DPEC       */
   AND  nota-fiscal.idi-sit-nf-eletro         = 5 ) THEN /* Situaá∆o da nota  = Documento Rejeitado */
       NEXT.
    */
    
    &IF DEFINED (bf_dis_nfe) &THEN                                                                       
        &IF "{&bf_dis_versao_ems}" >= "2.07" &THEN                                                       
            IF (nota-fiscal.idi-forma-emis-nf-eletro = 1 AND nota-fiscal.idi-sit-nf-eletro <> 3  )                                                /*Tp Emis 1 = Normal            | Sit 3  = Uso Autorizado         */ 
            OR (nota-fiscal.idi-forma-emis-nf-eletro = 4 AND nota-fiscal.idi-sit-nf-eletro <> 15 AND nota-fiscal.idi-sit-nf-eletro <> 3) THEN     /*Tp Emis 4 = Contingància DPEC | Sit 15 = DPEC recebido pelo SCE */ 
               NEXT.
        &ELSE
            IF  SUBSTR(nota-fiscal.char-2,65,2) = '' THEN 
                NEXT.
            IF SUBSTR(nota-fiscal.char-2,65,2) = '1' THEN DO: /*Tp Emis 1 = Normal*/
            
                FIND FIRST sit-nf-eletro NO-LOCK
                     WHERE sit-nf-eletro.cod-estabel   = nota-fiscal.cod-estabel
                       AND sit-nf-eletro.cod-serie     = nota-fiscal.serie      
                       AND sit-nf-eletro.cod-nota-fisc = nota-fiscal.nr-nota-fis NO-ERROR.
                IF  NOT AVAIL sit-nf-eletro OR sit-nf-eletro.idi-sit-nf-eletro <> 3 THEN /*Sit 3 = Uso Autorizado*/
                    NEXT.
            END.
            IF SUBSTR(nota-fiscal.char-2,65,2) = '4' THEN DO: /*Tp Emis 4 = Contingància DPEC*/
            
                FIND FIRST sit-nf-eletro NO-LOCK
                     WHERE sit-nf-eletro.cod-estabel   = nota-fiscal.cod-estabel
                       AND sit-nf-eletro.cod-serie     = nota-fiscal.serie      
                       AND sit-nf-eletro.cod-nota-fisc = nota-fiscal.nr-nota-fis NO-ERROR.
                IF  NOT AVAIL sit-nf-eletro OR (sit-nf-eletro.idi-sit-nf-eletro <> 15 AND sit-nf-eletro.idi-sit-nf-eletro <> 3) THEN /*Sit 15 = DPEC recebido pelo SCE*/
                    NEXT.
            END.
        &ENDIF
    &ENDIF
    

    run pi-acompanhar in h-acomp("Gerando DANFE para nota " + nota-fiscal.cod-estabel + "/" + nota-fiscal.serie + "/" + nota-fiscal.nr-nota-fis).

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    
    if  first-of(nota-fiscal.nr-nota-fis) then do:
        assign v-cont-registro = 0.
    end.

    EMPTY TEMP-TABLE tt-notas-impressas.
    EMPTY TEMP-TABLE tt-notas-fiscais.
    EMPTY TEMP-TABLE ttArquivo.
    PAUSE 2 NO-MESSAGE.

    run pi-imprime-nota.

    run pi-acompanhar in h-acomp("DANFE gerado " + nota-fiscal.cod-estabel + "/" + nota-fiscal.serie + "/" + nota-fiscal.nr-nota-fis).

    IF l-enviar-email THEN DO:
       ASSIGN c-nr-nota-xml = TRIM(nota-fiscal.cod-estabel) +                              
                              SUBSTR(nota-fiscal.cod-chave-aces-nf-eletro,23,3) + 
                              TRIM(STRING(INTEGER(SUBSTR(nota-fiscal.cod-chave-aces-nf-eletro,26,9)),">>9999999")) + ".XML".

       FIND FIRST tt-notas-fiscais NO-LOCK
             WHERE tt-notas-fiscais.cod-estabel = nota-fiscal.cod-estabel 
               AND tt-notas-fiscais.serie       = nota-fiscal.serie       
               AND tt-notas-fiscais.nr-nota-fis = nota-fiscal.nr-nota-fis NO-ERROR.
        IF NOT AVAIL tt-notas-fiscais THEN DO:
           CREATE tt-notas-fiscais.
           BUFFER-COPY nota-fiscal TO tt-notas-fiscais.
           ASSIGN tt-notas-fiscais.r-rowid = ROWID(nota-fiscal)
                  tt-notas-fiscais.c-arq-xml = SEARCH(c-cod-dir-histor-xml + "/" + c-nr-nota-xml).
           IF  tt-notas-fiscais.c-arq-xml = ? THEN DO:
               OUTPUT TO value('c:\temp\erro_XML_nao_encontrado_' + STRING(TIME) + '.txt').
                  PUT "O arquivo abaixo n∆o foi encontrado:" SKIP
                      c-cod-dir-histor-xml + "/" + c-nr-nota-xml  FORMAT 'x(50)'. 
               OUTPUT CLOSE.
           END.
        END.
    END.

    {utp/ut-liter.i "Gerando_Arquivo_Final" *}
    RUN pi-acompanhar in h-acomp(RETURN-VALUE).
    EMPTY TEMP-TABLE tt-doc.
    
    RUN piCriaSeparado(OUTPUT TABLE tt-doc).  // Gera o Danfe em PDF

    /* Aqui vai chamar o ponto que ja tem o pdf gerado */
    FOR EACH tt-notas-fiscais:
        FIND FIRST tt-doc WHERE     
                   tt-doc.cod-estabel = tt-notas-fiscais.cod-estabel AND 
                   tt-doc.serie       = tt-notas-fiscais.serie AND 
                   tt-doc.nr-nota-fis = tt-notas-fiscais.nr-nota-fis 
                   NO-LOCK NO-ERROR.
        IF AVAIL tt-doc AND 
           VALID-HANDLE(h-cb-envia-email) THEN
           RUN pi-envia-email (INPUT tt-notas-fiscais.r-rowid).
    END.

    IF CAN-FIND (FIRST ser-estab                                 
                 WHERE ser-estab.cod-estabel = c-cod-estabel      
                   AND ser-estab.serie       = c-serie
                   AND ser-estab.log-word-danfe)  THEN
                       ASSIGN lSemWord = YES. 
                       
    IF NOT lSemWord THEN
       RUN piJuntaArquivos.
    
    run pi-acompanhar in h-acomp("Abrindo Documento DANFE").
    
    assign v-des-retorno = "OK":U.
    
    if v-des-retorno <> "OK" then do:
        if i-num-ped-exec-rpw <> 0 then
            return v-des-retorno.
        else
            message v-des-retorno view-as alert-box error buttons ok.
    end.

    IF  lSemWord THEN DO:
        IF  tt-param-aux.destino = 1 
        OR  tt-param-aux.destino = 3 THEN DO:
            FOR EACH ttArquivo:
                RUN OpenDocument(SESSION:TEMP-DIRECTORY + "/" + ttArquivo.nomeArquivo).
            END.
        END.
    END.
    ELSE DO:
        IF  tt-param-aux.destino = 3 THEN DO:
            RUN OpenDocument(tt-param-aux.arquivo).
            IF l-mais-itens THEN
                RUN OpenDocument(c-arquivo-continua).
        END.
    END.

    PAUSE 4 NO-MESSAGE.
END.

IF VALID-HANDLE(h-acomp) THEN /*gr9030g*/
    RUN pi-finalizar IN h-acomp NO-ERROR.

RETURN 'OK'.

/* Procedure para impressao da nota fiscal */
PROCEDURE pi-imprime-nota:
    /* data de saida da nota fiscal */
    assign r-nota = rowid(nota-fiscal).

    if  l-dt = YES AND string(da-dt-saida) <> "" AND da-dt-saida <> ? then do:
        find first b-nota-fiscal
             where rowid(b-nota-fiscal) = rowid(nota-fiscal) EXCLUSIVE-LOCK no-error.
        assign b-nota-fiscal.dt-saida = da-dt-saida.
    end.
    
    find first ped-venda
         where ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
           and ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
         no-lock no-error.
    
    find first estabelec
         where estabelec.cod-estabel = nota-fiscal.cod-estabel
         no-lock no-error.

    find first emitente
         where emitente.nome-abrev = nota-fiscal.nome-ab-cli
         no-lock no-error.

    find first pre-fatur use-index ch-embarque
         where pre-fatur.nr-embarque = nota-fiscal.nr-embarque
         and   pre-fatur.nome-abrev  = nota-fiscal.nome-ab-cli
         and   pre-fatur.nr-pedcli   = nota-fiscal.nr-pedcli
         and   pre-fatur.nr-resumo   = nota-fiscal.nr-resumo
         no-lock no-error.

    find first natur-oper
         where natur-oper.nat-operacao = nota-fiscal.nat-operacao
         no-lock no-error.

    FIND FIRST ser-estab NO-LOCK
        WHERE  ser-estab.cod-estabel = c-cod-estabel
          AND  ser-estab.serie       = c-serie NO-ERROR.

    assign r-estabel    = rowid(estabelec)
           r-ped-venda  = rowid(ped-venda)
           r-emitente   = rowid(emitente)
           r-natur-oper = rowid(natur-oper)
           r-ser-estab  = ROWID(ser-estab)
           r-pre-fat    = if  avail pre-fatur then
                              rowid(pre-fatur)
                          else ?
           l-tipo-nota  = no.

    /*assign de-conv        = 1
           de-conv-pis    = 1
           de-conv-cofins = 1
           de-conv-total  = 1.

    find first cidade-zf where cidade-zf.cidade = nota-fiscal.cidade
                         and   cidade-zf.estado = nota-fiscal.estado
                         no-lock no-error.
    if  avail cidade-zf 
    and dec(substr(natur-oper.char-2,66,5)) > 0 then
        assign de-conv = (100 - dec(substr(natur-oper.char-2,66,5))) / 100.
        /* valor para tratamento de ZFM */

    &IF "{&bf_dis_versao_ems}" >= "2.062" &THEN

        if  avail cidade-zf 
        and natur-oper.val-perc-desc-pis-zfm > 0 then
            assign de-conv-pis = (100 - natur-oper.val-perc-desc-pis-zfm) / 100.
    
        if  avail cidade-zf 
        and natur-oper.val-perc-desc-cofins-zfm > 0 then
            assign de-conv-cofins = (100 - natur-oper.val-perc-desc-cofins-zfm) / 100.
    
        if  avail cidade-zf 
        and (dec(substr(natur-oper.char-2,66,5)) > 0 OR 
             natur-oper.val-perc-desc-pis-zfm    > 0 OR
             natur-oper.val-perc-desc-cofins-zfm > 0) THEN
            assign de-conv-total = (100 - (dec(substr(natur-oper.char-2,66,5)) +
                                           natur-oper.val-perc-desc-pis-zfm    +
                                           natur-oper.val-perc-desc-cofins-zfm)) / 100.
    &ELSE

        if  avail cidade-zf 
        and dec(substr(natur-oper.char-1,110,8)) > 0 then
            assign de-conv-pis = (100 - dec(substr(natur-oper.char-1,110,8))) / 100.
    
        if  avail cidade-zf 
        and dec(substr(natur-oper.char-1,118,8)) > 0 then
            assign de-conv-cofins = (100 - dec(substr(natur-oper.char-1,118,8))) / 100.
    
        if  avail cidade-zf 
        and (dec(substr(natur-oper.char-2,66,5)) > 0 OR 
             dec(substr(natur-oper.char-1,110,8)) > 0 OR
             dec(substr(natur-oper.char-1,118,8)) > 0) THEN
            assign de-conv-total = (100 - (dec(substr(natur-oper.char-2,66,5))  + 
                                           dec(substr(natur-oper.char-1,110,8)) + 
                                           dec(substr(natur-oper.char-1,118,8)))) / 100.
    &ENDIF
    */

    run ftp/ft0518f.p (INPUT-OUTPUT TABLE ttArquivo).

    /* muda o status da nota-fiscal */
    run ftp/ft0503a.p .

    create tt-notas-impressas.
    assign tt-notas-impressas.r-nota = rowid(nota-fiscal).

    /*************************************************/
END PROCEDURE.

PROCEDURE OpenDocument:

    def input param c-doc as char  no-undo.
    def var c-exec as char  no-undo.
    def var h-Inst as int  no-undo.

    assign c-exec = fill("x",255).
    run FindExecutableA (input c-doc,
                         input "",
                         input-output c-exec,
                         output h-inst).

    if h-inst >= 0 and h-inst <=32 then
      run ShellExecuteA (input 0,
                         input "open",
                         input "rundll32.exe",
                         input "shell32.dll,OpenAs_RunDLL " + c-doc,
                         input "",
                         input 1,
                         output h-inst).

    run ShellExecuteA (input 0,
                       input "open",
                       input c-doc,
                       input "",
                       input "",
                       input 1,
                       output h-inst).

    if h-inst < 0 or h-inst > 32 then return "OK".
    else return "NOK".

END PROCEDURE.

PROCEDURE FindExecutableA EXTERNAL "Shell32.dll" persistent:

    define input parameter lpFile as char  no-undo.
    define input parameter lpDirectory as char  no-undo.
    define input-output parameter lpResult as char  no-undo.
    define return parameter hInstance as long.

END.

PROCEDURE ShellExecuteA EXTERNAL "Shell32.dll" persistent:

    define input parameter hwnd as long.
    define input parameter lpOperation as char  no-undo.
    define input parameter lpFile as char  no-undo.
    define input parameter lpParameters as char  no-undo.
    define input parameter lpDirectory as char  no-undo.
    define input parameter nShowCmd as long.
    define return parameter hInstance as long.

END PROCEDURE.

PROCEDURE piJuntaArquivos:
    DEFINE VAR lEntrou        AS LOG        NO-UNDO.
    DEFINE VAR ch-app-word    AS COM-HANDLE NO-UNDO.
    DEFINE VAR i-numero-copia AS INTEGER    NO-UNDO.
    DEFINE VAR c-impressora-padrao AS CHARACTER  NO-UNDO.

    CREATE 'Word.Application':U ch-app-word.                         /* Cria uma aplicaá∆o WORD */
    ch-app-word:WindowState = 2.                                     /* O estado dois para o Word Ç minimizado */
    ch-app-word:VISIBLE = NO.                                        /* Apenas para n∆o mostrar que o word est† sendo utilizado em tela */

    FOR EACH ttArquivo:
        run pi-acompanhar in h-acomp("Imprimindo Nota Fiscal " + ENTRY(3,ttArquivo.nomeArquivo,"-") ).
        
        DO i-numero-copia = 1 TO tt-param-aux.nr-copias:
        
            IF  ttArquivo.Sequencia = 1 AND 
                i-numero-copia      = 1 THEN
                ch-app-word:Documents:ADD(SESSION:TEMP-DIRECTORY + "/" + ttArquivo.nomeArquivo).        /* Inclui arquivo */
            ELSE DO:
                ch-app-word:SELECTION:EndKey(6).                         /* Posiciona cursor no final do arquivo */
                ch-app-word:SELECTION:InsertBreak(7).                    /* Qubra pagina antes de inserir arquivo */
                ch-app-word:SELECTION:Insertfile(SESSION:TEMP-DIRECTORY + "/" + ttArquivo.nomeArquivo).  /* Insere arquivo no documento aberto */
            END.

        END.

        ASSIGN lEntrou = YES.
    END.
    
    IF NOT lEntrou THEN
       ch-app-word:Documents:ADD().                                 /* Inclui arquivo */

    ch-app-word:ActiveDocument:SaveAs(tt-param-aux.arquivo).         /* Salva o arquivo aberto no WORD com o nome final do arquivo */
    IF  tt-param-aux.destino = 1 THEN DO: /* Impressora */
        
        IF INDEX (tt-param-aux.impressora-so," in session") > 0 THEN
            tt-param-aux.impressora-so = tt-param-aux.impressora-so + " on " + SESSION:PRINTER-PORT.
        /* Guarda a impressora padr∆o do windows antes da geraá∆o do DANFE */
        ASSIGN c-impressora-padrao = SESSION:PRINTER-NAME.
        /* Seleciona a impressora para impress∆o */
        ch-app-word:ActivePrinter = tt-param-aux.impressora-so.
        /* Imprime o documento na impressora selecionada */
        ch-app-word:printout(0). /* 0 : N∆o mostra erros nem advertencias */
        /* Volta a impressora padr∆o do windows */
        ch-app-word:ActivePrinter = c-impressora-padrao.    
    END.

    ch-app-word:ActiveDocument:CLOSE.                                /* Fecha o arquivo do WORD */
    ch-app-word:QUIT().                                              /* Fechar o WORD */
    RELEASE OBJECT ch-app-word.                                      /* Elimina o endereáo utilizado para o WORD na m†quina */

    FOR EACH ttArquivo:
        OS-DELETE VALUE(SESSION:TEMP-DIRECTORY + "/" + ttArquivo.nomeArquivo) NO-ERROR. 
    END.

END.

PROCEDURE piCriaSeparado:

    DEFINE OUTPUT PARAM TABLE FOR tt-doc.

    /************************Variaveis do Word********************/
    //DEFINE VARIABLE chDoc               AS COM-HANDLE  NO-UNDO.
    //DEFINE VARIABLE ch_documento        AS COM-HANDLE  NO-UNDO.
    //DEFINE VARIABLE ch_novo             AS COM-HANDLE  NO-UNDO.
    //DEFINE VARIABLE chNovo              AS COM-HANDLE  NO-UNDO.
    DEFINE VARIABLE i-cont              AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i-cont-aux          AS INTEGER     NO-UNDO.
    DEFINE VARIABLE c-arquivo           AS CHAR        NO-UNDO.
    DEFINE VARIABLE c-caminho-aux       AS CHAR        NO-UNDO.
    DEFINE VARIABLE chave-danfe         AS CHAR        NO-UNDO.
    DEFINE VARIABLE c-impressora-padrao AS CHAR        NO-UNDO.
    DEFINE VARIABLE c-caminho-aux2      AS CHAR        NO-UNDO.

    ASSIGN c-impressora-padrao = SESSION:PRINTER-NAME.

    FOR EACH ttArquivo:
        ASSIGN i-cont = i-cont + 1.
        CREATE 'Word.Application':U appWord.                         /* Cria uma aplicaá∆o WORD */
        appWord:WindowState = 2.                                     /* O estado dois para o Word Ç minimizado */
        appWord:VISIBLE = NO.                                        /* Apenas para n∆o mostrar que o word est† sendo utilizado em tela */
        ASSIGN ttArquivo.nomeArquivo = ttArquivo.nomeArquivo.
        AppWord:Documents:OPEN(SESSION:TEMP-DIRECTORY + "/" + ttArquivo.nomeArquivo).
        

/*         appWord:documents:ADD().                      */
/*                                                       */
/*         appWord:SELECTION:PageSetup:ORIENTATION  = 1. */
/*         appWord:SELECTION:PageSetup:topMargin    = 24.5. */
/*         appWord:SELECTION:PageSetup:BottomMargin = 24.5. */
/*         appWord:SELECTION:PageSetup:LeftMargin   = 24.5. */
/*         appWord:SELECTION:PageSetup:RightMargin  = 24.5. */

        /*Verifica o tipo de saida do arquivo 1- impressora, 2- arquivo e 3- terminal */
        //IF tt-param-aux.destino = 2 OR tt-param-aux.destino = 3 THEN DO: 
        ASSIGN c-arquivo     = (tt-param-aux.arquivo + chave-danfe)
               c-arquivo     = REPLACE(c-arquivo,".doc":U, "":U).
               //c-arquivo     = REPLACE(c-arquivo + "-" + string(i-cont),".doc":U, "":U).

        ASSIGN c-caminho-aux  = ENTRY(NUM-ENTRIES(c-arquivo,"\"),c-arquivo,"\")
               //c-caminho-aux  = c-caminho-aux + "-" + STRING(i-cont)
            .
        ASSIGN c-caminho-aux2 = REPLACE(c-arquivo,c-caminho-aux,"").

        OS-DELETE VALUE(c-arquivo + ".pdf") NO-ERROR.
        OS-DELETE VALUE(c-caminho-aux + ".doc") NO-ERROR.
        /*IF c-seg-usuario = 'adriano' THEN
           MESSAGE 'cam. aux.2' c-caminho-aux2 SKIP
                   'cam. aux.'  c-caminho-aux SKIP
                   'nome arquivo' ttArquivo.nomeArquivo SKIP
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

        AppWord:ActiveDocument:SaveAs(c-caminho-aux + ".doc").
        AppWord:ActiveDocument:SaveAs2(c-arquivo + ".pdf", 17).

            /* MESSAGE 'geraá∆o arquivo:' ttArquivo.nomeArquivo SKIP
                      'arquivo aux2' c-caminho-aux2 SKIP    
                     'arquivo aux' c-caminho-aux SKIP
            VIEW-AS ALERT-BOX INFO BUTTONS OK.       */



             CREATE tt-doc.
             ASSIGN tt-doc.cod-estabel     = STRING(ENTRY(1,ttArquivo.nomeArquivo,"-"))
                    tt-doc.serie           = STRING(ENTRY(2,ttArquivo.nomeArquivo,"-"))
                    tt-doc.nr-nota-fis     = STRING(ENTRY(3,ttArquivo.nomeArquivo,"-"))
                    tt-doc.caminho-pdf     = c-caminho-aux2 + c-caminho-aux  +  ".pdf"
                    tt-doc.caminho-doc     = c-caminho-aux2 + c-caminho-aux  +  ".doc".
        //END. 
        
        appWord:activeDocument:CLOSE.
        appWord:QUIT().
        RELEASE OBJECT AppWord.
    END.

    ASSIGN SESSION:PRINTER-NAME = c-impressora-padrao.
    
END PROCEDURE.

PROCEDURE pi-release:

    if valid-handle(ch-work) then
       release object ch-work.

    if valid-handle(ch-pdfx) then
       release object ch-pdfx.


END PROCEDURE.

PROCEDURE pi-envia-email.
    DEF INPUT PARAMETER p-row-nota AS ROWID.
    
    DEF VAR c-mensagem      AS CHAR.
    DEF VAR c-remetente     AS CHAR.
    DEF VAR c-destinatario  AS CHAR.
    DEF VAR c-anexos        AS CHAR.
    DEFINE VARIABLE hBoMem AS HANDLE      NO-UNDO.

    FIND nota-fiscal WHERE
         ROWID(nota-fiscal) = p-row-nota NO-LOCK NO-ERROR.

    FIND emitente WHERE
         emitente.cod-emit = nota-fiscal.cod-emit NO-LOCK NO-ERROR.

    IF h-cb-envia-email:SCREEN-VALUE MATCHES '*Danfe*' THEN
       ASSIGN c-anexos = tt-doc.caminho-pdf.

    IF h-cb-envia-email:SCREEN-VALUE MATCHES '*XML*' THEN
       ASSIGN c-anexos = IF c-anexos = '' 
                         THEN tt-notas-fiscais.c-arq-xml
                         ELSE c-anexos + "," + tt-notas-fiscais.c-arq-xml.

    RUN esbo/boMemorandos.p PERSIST SET hBoMem .
    RUN iniciar IN hBoMem.                      
    RUN setProp IN hBoMem('codEstab',0,nota-fiscal.cod-estabel).
    RUN setProp IN hBoMem('tipoMemorandoId',0,'1').
    RUN exec IN hBoMem.
    RUN getTextoMemorando IN hBoMem(OUTPUT c-mensagem).
    RUN finalizar IN hBoMem.
    ASSIGN c-mensagem = REPLACE(c-mensagem,'$$serie$$',nota-fiscal.serie)
           c-mensagem = REPLACE(c-mensagem,'$$nrnotafis$$',nota-fiscal.nr-nota-fis)
           c-mensagem = REPLACE(c-mensagem,'$$nomeEmit$$',emitente.nome-emit)
           c-mensagem = REPLACE(c-mensagem,'$$CNPJ$$',emitente.cgc)
           c-mensagem = REPLACE(c-mensagem,'$$ChaveAcesso$$',nota-fiscal.cod-chave-aces-nf-eletro)
           . 
    
    /*

    ASSIGN c-mensagem = 'Esta mensagem refere-se a Nota Fiscal Eletrìnica Nacional ' +
                        'de serie/n£mero [' + nota-fiscal.serie + '/' + nota-fiscal.nr-nota-fis + '] emitida para:' + CHR(10) + 
                        'Raz∆o Social: [' + emitente.nome-emit + ']' + CHR(10) + 
                        'CNPJ: [' + emitente.cgc + ']' + CHR(10) + CHR(10) + 
                        'Para verificar a autorizaá∆o da SEFAZ referente Ö nota acima mencionada, ' +
                        'acesse o endereáo http://www.nfe.fazenda.gov.br/portal' + CHR(10) + CHR(10) +
                        'Chave de Acesso [' + nota-fiscal.cod-chave-aces-nf-eletro + ']' + CHR(10) + 
                        'Protocolo [' + nota-fiscal.cod-chave-aces-nf-eletro + ']' + CHR(10) + CHR(10).
                        

    IF nota-fiscal.cod-estabel = '1' THEN DO.
       ASSIGN c-mensagem = c-mensagem + 
                           'Este e-mail foi enviado automaticamente pelo Sistema de Nota Fiscal Eletrìnica (NF-e) da IMA TECIDOS DA MODA LTDA'.
       ASSIGN c-remetente = 'tss@imatextil.com.br'.
    END.
    IF nota-fiscal.cod-estabel = '5' THEN DO.
       ASSIGN c-mensagem = c-mensagem + 
                          'Este e-mail foi enviado automaticamente pelo Sistema de Nota Fiscal Eletrìnica (NF-e) da MEDTEXTIL IMP. e EXP. LTDA'.
       ASSIGN c-remetente = 'tss@imatextil.com.br'.
    END.
    */
    /*IF c-seg-usuario = 'super' THEN
       MESSAGE c-anexos
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    IF h-ed-dest-email:SCREEN-VALUE BEGINS "ENVIO DE E-MAIL EM LOTE" THEN DO.
       //ASSIGN c-destinatario = 'eduardo.magno@imatextil.com.br'.
       
       FIND emitente WHERE
            emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR.
       ASSIGN c-destinatario = ''.
       FOR FIRST cont-emit OF emitente WHERE 
                 cont-emit.area = 'Fiscal' NO-LOCK.
           ASSIGN c-destinatario = cont-emit.e-mail.
       END.
       //IF c-destinatario = '' THEN RETURN 'ADM-ERROR'.
    END.
    ELSE
       ASSIGN c-destinatario = REPLACE(REPLACE(REPLACE(h-ed-dest-email:SCREEN-VALUE,CHR(10),""),CHR(13),"")," ","").

    ASSIGN c-destinatario = c-destinatario + ",log@imatextil.com.br".
    
    IF c-destinatario <> '' THEN DO.
     /*  MESSAGE 'vou enviar o e-mail:' c-destinatario
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
       RUN esapi/esapi002.p (/*c-remetente  */ INPUT c-remetente,                                 
                          /*c-destino    */ INPUT c-destinatario, 
                      //  /*c-copia      */ INPUT REPLACE(REPLACE(REPLACE(h-ed-copia-email:SCREEN-VALUE,CHR(10),""),CHR(13),"")," ",""), 
                          /*c-assunto    */ INPUT 'EMISS«O DE NF', 
                          /*c-mensagem   */ INPUT c-mensagem,
                          /*c-arq-anexo  */ INPUT c-anexos,
                          /*l-mostra-erro*/ INPUT NO).
    END.
    ELSE  DO:
        OUTPUT TO value('c:\temp\log-mail-nf' + STRING(TIME) + '.txt').
        PUT 'nota fiscal:' nota-fiscal.nr-nota-fis ' n∆o enviada, pois o destinat†rio estava em branco' SKIP.

        OUTPUT CLOSE.
    END.

END PROCEDURE.


/* fim do programa */
