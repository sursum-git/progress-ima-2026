/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i FT051627 2.00.00.009}  /*** 010009 ***/
/*****************************************************************************
**       Programa: FT051627.p
**       Data....: 14/03/07
**       Autor...: DATASUL S.A.
**       Objetivo: Emissor DANFE - NF-e
**       Vers∆o..: 1.00.000 - super
**       OBS.....: Este fonte foi gerado pelo Data Viewer 3.00
*******************************************************************************/

disable triggers for load of gati-nfe.
disable triggers for dump of gati-nfe.

/* Definiá‰es Especificas */

{utp/utapi019.i}

def buffer bf-sit-nf-eletro for sit-nf-eletro.

/* Fim Definiá‰es Especificas */

define variable c-prog-gerado as character no-undo initial "FT051627".

def new global shared var c-arquivo-log    as char  format "x(60)"no-undo.

/****************** DefiniÁ„o de Tabelas Tempor·rias do RelatÛrio **********************/

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
    field ep-codigo            as integer
    field c-cod-estabel        like nota-fiscal.cod-estabel
    field c-serie              like nota-fiscal.serie
    field c-nr-nota-fis-ini    like nota-fiscal.nr-nota-fis
    field c-nr-nota-fis-fim    like nota-fiscal.nr-nota-fis
    field i-nr-embarque-ini    like nota-fiscal.nr-embarque
    field i-nr-embarque-fim    like nota-fiscal.nr-embarque
    field da-dt-saida          like nota-fiscal.dt-saida
    field c-hr-saida           like nota-fiscal.hr-confirma
    field banco                as integer
    field cod-febraban         as integer      
    field cod-portador         as integer      
    field prox-bloq            as char         
    field c-instrucao          as char extent 5
    field imprime-bloq         as logical
    field rs-imprime           as INTEGER
    FIELD impressora-so        AS CHAR
    FIELD impressora-so-bloq   AS CHAR.

DEF TEMP-TABLE ttArquivo NO-UNDO
      FIELD sequencia AS INT
      FIELD nomeArquivo AS CHAR
      INDEX idx1 sequencia.

define shared temp-table tt-notas-impressas
    field r-nota as rowid.

DEF TEMP-TABLE tt-notas-fiscais NO-UNDO LIKE nota-fiscal
     FIELD r-rowid AS ROWID .

DEF TEMP-TABLE tt-doc 
    FIELD caminho-doc AS CHAR
    FIELD caminho-pdf AS CHAR
    FIELD nr-nota-fis AS CHAR
    FIELD serie       AS CHAR
    FIELD cod-estabel AS CHAR.

DEF TEMP-TABLE tt-gati-nfe NO-UNDO LIKE gati-nfe.

{bcp/bcapi004.i}
{cdp/cd0666.i}
{cdp/cdcfgdis.i}

/*************************
*   definicao de buffer
*************************/
def buffer b-nota-fiscal for nota-fiscal.
                                                              
{ftp/ft2010.i1} /* Definicao da temp-table tt-notas-geradas */ 

/****************** INCLUDE COM VARIµVEIS GLOBAIS *********************/

{utp/ut-glob.i}

DEF VAR  ch-pdfx AS COM-HANDLE NO-UNDO.

DEFINE VARIABLE ch-work            AS COM-HANDLE  NO-UNDO.
DEFINE VARIABLE AppWord            AS COM-HANDLE  NO-UNDO. 

def new shared var r-nota         as rowid.
def new shared var r-natur-oper   as rowid.
def new shared var i-nr-nota      like nota-fiscal.nr-nota-fis.
def new shared var c-ser          like nota-fiscal.serie       init "".
def new shared var c-est          like nota-fiscal.cod-estabel init "ZZZ".
def new shared var i-nr-embarque  like nota-fiscal.nr-embarque.
def new shared var l-dt           as logical format "Sim/Nao"  init no.
def new shared var dt-saida       as date    format "99/99/9999" init ?.
def new shared var hr-saida       as char    format "xx:xx:xx" init "".
def new shared var de-conv        as decimal format ">>>>9.99".
def new shared var de-conv-pis    as decimal format ">>>>9.99".
def new shared var de-conv-cofins as decimal format ">>>>9.99".
def new shared var de-conv-total  as decimal format ">>>>9.99".

def new shared var l-aliq-nat      as logical                        no-undo.
def new shared var l-tipo-nota     as logical format "Entrada/Saida" no-undo.
def new shared var de-aliquota-icm like natur-oper.aliquota-icm.
def new shared var r-ped-venda     as rowid.
def new shared var r-pre-fat       as rowid.
def new shared var r-emitente      as rowid.
def new shared var r-estabel       as rowid.
def new shared var r-docum-est     as rowid.
DEF NEW SHARED VAR r-ser-estab     AS ROWID.

def new global shared var c-dir-spool-servid-exec as CHAR no-undo.
/****************** Definiáao de ParÉmetros do Relat¢rio *********************/ 

/****************** Definiáao de Vari†veis de Seleá∆o do Relat¢rio *********************/ 
def new shared var c-cod-estabel      like nota-fiscal.cod-estabel format "X(3)"       initial "" no-undo.
def new shared var c-serie            like nota-fiscal.serie       format "x(5)"       initial "" no-undo.
def new shared var c-nr-nota-fis-ini  like nota-fiscal.nr-nota-fis format "x(16)"      initial "" no-undo.
def new shared var c-nr-nota-fis-fim  like nota-fiscal.nr-nota-fis format "x(16)"      initial "ZZZZZZZZZZZZZZZZ" no-undo.
def new shared var i-nr-embarque-ini  like nota-fiscal.nr-embarque format ">>>>,>>9"   initial 0 no-undo.
def new shared var i-nr-embarque-fim  like nota-fiscal.nr-embarque format ">>>>,>>9"   initial 9999999 no-undo.
def new shared var da-dt-saida        like nota-fiscal.dt-saida    format "99/99/9999" initial "01/01/1800" no-undo.
def new shared var c-hr-saida         like nota-fiscal.hr-confirma format "99:99"      initial "0000" no-undo.
def new shared var c-telefone         like empresa.telefone[1] no-undo.
def new shared var l-mais-itens       AS LOGICAL INIT NO NO-UNDO.
def new shared var c-cod-layout       as character no-undo.

/****************** Definiáao de Vari†veis dos Calculos do Relat¢rio *************************/ 

def input param raw-param as raw no-undo.
def input param table for tt-raw-digita.

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

def var i-sit-nota-ini    as integer.
def var i-sit-nota-fim    as integer.

define stream arq-erro.
def var c-arquivo as char format "X(40)".

create tt-param-aux.
raw-transfer raw-param to tt-param-aux.

/* case tt-param-aux.cod-layout:                                       */
/*    when "DANFE-Mod.1":U then                                        */
/*        assign v-des-local-layout = "ftp/FT051627-DANFE-Mod.1.xml".  */
/*    when "DANFE-Mod.2":U then                                        */
/*        assign v-des-local-layout = "ftp/FT051627-DANFE-Mod.2.xml".  */
/* end case.                                                           */
/*                                                                 */
/* run loadLayout in h-FunctionLibrary (input v-des-local-layout). */
/*                                                                 */
/* IF RETURN-VALUE <> ? and RETURN-VALUE <> "OK":U THEN DO:        */
/*    IF valid-handle(h-FunctionLibrary) THEN                      */
/*       delete procedure h-FunctionLibrary.                       */
/*    IF i-num-ped-exec-rpw <> 0 THEN                              */
/*       RETURN RETURN-VALUE.                                      */
/*    ELSE                                                         */
/*       MESSAGE RETURN-VALUE VIEW-AS ALERT-BOX INFO BUTTONS OK.   */
/*    return 'NOK'.                                                */
/* END.                                                            */


{include/i-rpvar.i}

assign c-programa     = "FT0516rp"
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
    assign c-empresa  = mguni.empresa.razao-social
           c-telefone = mguni.empresa.telefone[1].
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

assign i-ep-codigo-usuario = tt-param-aux.ep-codigo
       v_cdn_empres_usuar  = i-ep-codigo-usuario
       c-cod-estabel       = tt-param-aux.c-cod-estabel
       c-serie             = tt-param-aux.c-serie
       c-nr-nota-fis-ini   = tt-param-aux.c-nr-nota-fis-ini
       c-nr-nota-fis-fim   = tt-param-aux.c-nr-nota-fis-fim
       i-nr-embarque-ini   = tt-param-aux.i-nr-embarque-ini
       i-nr-embarque-fim   = tt-param-aux.i-nr-embarque-fim
       da-dt-saida         = tt-param-aux.da-dt-saida
       c-hr-saida          = tt-param-aux.c-hr-saida.

/**** EXECUCAO RELATORIO GRAFICO ****/
case tt-param-aux.destino:
    when 1 then assign v-cod-destino-impres = "Impressora".
    when 2 then assign v-cod-destino-impres = "Arquivo".
    otherwise   assign v-cod-destino-impres = "Terminal".
end case.

/*run printForm in h-FunctionLibrary.*/

run utp/ut-acomp.p persistent set h-acomp.

run pi-inicializar in h-acomp(input "Acompanhamento Relat¢rio").

assign v-num-reg-lidos = 0.

for each tt-notas-impressas:
    delete tt-notas-impressas.
end.

if tt-param-aux.data-exec <> ? then 
   assign l-dt = yes.
else 
   assign l-dt = no.

assign dt-saida     = tt-param-aux.da-dt-saida
       hr-saida     = string(tt-param-aux.c-hr-saida,"99:99:99")
       c-cod-layout = tt-param-aux.cod-layout.

if   tt-param-aux.rs-imprime = 1 then
     assign i-sit-nota-ini = 1
            i-sit-nota-fim = 1.
else assign i-sit-nota-ini = 2
            i-sit-nota-fim = 7.

for each nota-fiscal no-lock
   where nota-fiscal.cod-estabel   = c-cod-estabel      
     and nota-fiscal.serie         = c-serie
     and nota-fiscal.nr-nota-fis  >= c-nr-nota-fis-ini
     and nota-fiscal.nr-nota-fis  <= c-nr-nota-fis-fim
     and nota-fiscal.nr-embarque  >= i-nr-embarque-ini 
     and nota-fiscal.nr-embarque  <= i-nr-embarque-fim 
     break by nota-fiscal.cod-estabel
           by nota-fiscal.serie
           by nota-fiscal.nr-nota-fis:
                      
    if   tt-param-aux.rs-imprime = 1 THEN DO:
        FIND FIRST ser-estab
              where ser-estab.serie              = nota-fiscal.serie
                and ser-estab.cod-estabel        = nota-fiscal.cod-estabel
                AND SUBSTR(ser-estab.char-1,1,3) = 'yes'
             NO-LOCK NO-ERROR.
         IF  AVAIL ser-estab THEN DO:
             FIND serie
                 WHERE serie.serie = nota-fiscal.serie
                 NO-LOCK NO-ERROR.
             IF  NOT AVAIL serie THEN NEXT.
             IF  serie.forma-emis = 2 THEN DO:
                 FIND FIRST sit-nf-eletro NO-LOCK
                     WHERE sit-nf-eletro.cod-estabel   = nota-fiscal.cod-estabel
                       AND sit-nf-eletro.cod-serie     = nota-fiscal.serie      
                       AND sit-nf-eletro.cod-nota-fisc = nota-fiscal.nr-nota-fis NO-ERROR.
                 IF  NOT AVAIL sit-nf-eletro OR sit-nf-eletro.idi-sit-nf-eletro = 3 THEN
                    NEXT.
             END.
             ELSE
                 IF  nota-fiscal.ind-sit-nota <> i-sit-nota-ini THEN NEXT.

         END.
        run gtp/gtnf002rp.p (input nota-fiscal.cod-estabel,
                             input nota-fiscal.serie,
                             input string(int(nota-fiscal.nr-nota-fis))).
    END.
    ELSE
        IF  nota-fiscal.ind-sit-nota < i-sit-nota-ini
        OR  nota-fiscal.ind-sit-nota > i-sit-nota-fim THEN  NEXT.

            /* - Comentado em 17/12/2009 - Renersson - GATI */
            
            IF  SUBSTR(nota-fiscal.char-2,65,2) = '' THEN 
                NEXT.
            IF SUBSTR(nota-fiscal.char-2,65,2) = '1' THEN DO: /* somente se nao for contingencia */
            
            
                FIND FIRST sit-nf-eletro NO-LOCK
                     WHERE sit-nf-eletro.cod-estabel   = nota-fiscal.cod-estabel
                       AND sit-nf-eletro.cod-serie     = nota-fiscal.serie      
                       AND sit-nf-eletro.cod-nota-fisc = nota-fiscal.nr-nota-fis NO-ERROR.
                IF  NOT AVAIL sit-nf-eletro OR sit-nf-eletro.idi-sit-nf-eletro <> 3 THEN
                    NEXT.
                    
            END.
        /*&ENDIF
    &ENDIF*/
    
    run pi-acompanhar in h-acomp("Gerando DANFE para nota " + nota-fiscal.cod-estabel + "/" + nota-fiscal.serie + "/" + nota-fiscal.nr-nota-fis).

    assign v-num-reg-lidos = v-num-reg-lidos + 1.
    
    if  first-of(nota-fiscal.nr-nota-fis) then do:
        assign v-cont-registro = 0.
    end.

    if  first-of(nota-fiscal.nr-nota-fis) then do:
        run pi-imprime-nota.
    end.


/*    IF l-mais-itens THEN
        run printForm in h-FunctionLibrary2.
    
    if  last-of(nota-fiscal.nr-nota-fis) then do:
        run writePage   in h-FunctionLibrary (input yes).
        IF l-mais-itens THEN
            run writePage   in h-FunctionLibrary2 (input yes).
    end.
    else do:
        if  v-cont-registro = -1 then do: 
            assign v-cont-registro = 0.
            run writePage in h-FunctionLibrary (input no).
            IF l-mais-itens THEN
                run writePage   in h-FunctionLibrary2 (input NO).
        end.
    end.*/

    IF tt-param-aux.rs-imprime = 1 THEN DO:
       FIND FIRST tt-notas-fiscais NO-LOCK
             WHERE tt-notas-fiscais.cod-estabel = nota-fiscal.cod-estabel 
               AND tt-notas-fiscais.serie       = nota-fiscal.serie       
               AND tt-notas-fiscais.nr-nota-fis = nota-fiscal.nr-nota-fis NO-ERROR.
        IF NOT AVAIL tt-notas-fiscais THEN DO:
            CREATE tt-notas-fiscais.
            BUFFER-COPY nota-fiscal TO tt-notas-fiscais.
            ASSIGN tt-notas-fiscais.r-rowid = ROWID(nota-fiscal).
        END.
    
        RUN pi-cria-tt-gati-nfe(INPUT nota-fiscal.cod-estabel,
                                INPUT nota-fiscal.serie,
                                INPUT nota-fiscal.nr-nota-fis).
    END.
     
    /*Alterado o ponto de chamada para o envio de email para o cliente TEAR */
    IF tt-param-aux.rs-imprime = 1 THEN
       RUN gtp/gtnf005.p (INPUT ROWID(nota-fiscal)).
end.

run pi-acompanhar in h-acomp("Gerando Arquivo Final").

/*if   tt-param-aux.rs-imprime = 1 THEN DO:
    EMPTY TEMP-TABLE tt-doc.
    
    RUN piCriaSeparado(OUTPUT TABLE tt-doc).
    
    FOR EACH tt-notas-fiscais:
        
        /*Aqui vai chamar o ponto que ja tem o pdf gerado */
        FIND FIRST tt-doc
             WHERE tt-doc.cod-estabel = tt-notas-fiscais.cod-estabel
               AND tt-doc.serie       = tt-notas-fiscais.serie
               AND tt-doc.nr-nota-fis = tt-notas-fiscais.nr-nota-fis NO-LOCK NO-ERROR.
        IF AVAIL tt-doc THEN DO:
            RUN gtp/gtnf005.p (INPUT tt-notas-fiscais.r-rowid).
                               /*INPUT tt-doc.caminho-pdf)*/ /*Alterado para o cliente TEAR */    
        END.
    END.
END. Comentado para a TEAR */

RUN piJuntaArquivos.

run pi-acompanhar in h-acomp("Abrindo Documento DANFE").

/*IF tt-param-aux.destino = 1 OR tt-param-aux.destino = 4 THEN DO:
    RUN setPrintable IN h-FunctionLibrary.
    IF l-mais-itens THEN
        RUN setPrintable IN h-FunctionLibrary2.
END.                                      

run saveXML in h-FunctionLibrary(input tt-param-aux.arquivo).
ASSIGN c-arquivo-continua = session:temp-directory + "FT0516aa-cont.pdf".
IF l-mais-itens THEN
    run saveXML in h-FunctionLibrary2(input c-arquivo-continua).
*/
assign v-des-retorno = "OK":U.

if v-des-retorno <> "OK" then do:
    if i-num-ped-exec-rpw <> 0 then
        return v-des-retorno.
    else
        message v-des-retorno view-as alert-box error buttons ok.
end.

IF VALID-HANDLE(h-acomp) THEN /*gr9030g*/
    RUN pi-finalizar IN h-acomp NO-ERROR.

IF tt-param-aux.destino = 3 THEN DO:
    RUN OpenDocument(tt-param-aux.arquivo).

    IF l-mais-itens THEN
        RUN OpenDocument(c-arquivo-continua).
END.

return 'OK'.

/* Procedure para impressao da nota fiscal */
procedure pi-imprime-nota:
    /* data de saida da nota fiscal */
    assign r-nota = rowid(nota-fiscal).

    /*
    if  l-dt = yes then do:
        find first b-nota-fiscal
             where rowid(b-nota-fiscal) = rowid(nota-fiscal) EXCLUSIVE-LOCK no-error.
        assign b-nota-fiscal.dt-saida = dt-saida.
    end.
    */

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

    assign de-conv        = 1
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
    
    run ftp/ft0516f.p (input rowid(cidade-zf),
                       INPUT-OUTPUT TABLE ttArquivo).

    /* muda o status da nota-fiscal */
    run ftp/ft0503a.p .
    

    create tt-notas-impressas.
    assign tt-notas-impressas.r-nota = rowid(nota-fiscal).

    /* GERA ETIQUETAS PARA O MODULO DE COLETA DE DADOS */

    if  avail param-global
    and param-global.modulo-cl
    and ( nota-fiscal.ind-tip-nota = 2) /* tipo de nota-fiscal Manual */
    then do:
        create tt-prog-bc.
        assign tt-prog-bc.cod-prog-dtsul        = "ft0513"
               tt-prog-bc.cod-versao-integracao = 1
               tt-prog-bc.usuario               = tt-param-aux.usuario
               tt-prog-bc.opcao                 = 1.

        run bcp/bcapi004.p (input-output table tt-prog-bc,
                            input-output table tt-erro).

        find first tt-prog-bc no-error.

        assign  c-arquivo = tt-prog-bc.nome-dir-etiq + "/" + c-arquivo.

        if  return-value = "OK" then do:

            {utp/ut-liter.i Gerando_Etiquetas  MRE R}
            run pi-acompanhar in h-acomp (input return-value).

            erro:
            do  on stop     undo erro,leave erro
                on quit     undo erro,leave erro
                on error    undo erro,leave erro
                on endkey   undo erro,leave erro:

                run value(tt-prog-bc.prog-criacao)(input tt-prog-bc.cd-trans,
                                                   input rowid(nota-fiscal),
                                                   input-output table tt-erro) no-error.

                if  ERROR-STATUS:ERROR 
                or  (    error-status:get-number(1) <> 138
                     and error-status:num-messages  <> 0)
                then do:
                    output stream arq-erro to value(c-arquivo) append.

                    {utp/ut-liter.i Ocorreu_na_Geraá∆o_de_Etiquetas_-_Progress MRE R}
                    put stream arq-erro "***" return-value skip.
                    {utp/ut-liter.i Programa * R}
                    put stream arq-erro error-status:get-message(1) skip.
                    put stream arq-erro return-value ": " tt-prog-bc.prog-criacao skip.
                    put stream arq-erro nota-fiscal.serie                           at 1.
                    put stream arq-erro nota-fiscal.nr-nota-fis                     at 7.
                    put stream arq-erro nota-fiscal.cod-estabel                     at 24.

                    output stream arq-erro close.
                end.

                if  return-value = "NOK" then do:
                    find first tt-erro no-error.
                    if  avail tt-erro
                    then do:
                        output stream arq-erro to value(c-arquivo) append.

                        {utp/ut-liter.i Ocorreu_na_Geraá∆o_de_Etiquetas MRE R}
                        put stream arq-erro "***" return-value skip.
                        for each tt-erro:
                            put stream arq-erro skip tt-erro.cd-erro " - " tt-erro.mensagem.
                        end.
                        put stream arq-erro skip.
                        output stream arq-erro close.
                    end.
                end.
            end.
        end.
        else do:
            /**** caso tenha integraá∆o com o coleta e ocorreu erros ***/
            find first tt-erro no-error.
            if  avail tt-erro then do:
                output stream arq-erro to value(c-arquivo) append.

                {utp/ut-liter.i Ocorreu_na_Geraá∆o_de_Etiquetas MRE R}
                put stream arq-erro "***" return-value skip.
                for each tt-erro:
                    put  stream arq-erro skip tt-erro.cd-erro " - " tt-erro.mensagem.
                end.
                put stream arq-erro skip.
                output stream arq-erro close.
            end.
        end.


    end.

    
end procedure.

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
    DEF VAR lEntrou     AS LOG NO-UNDO.
    DEF VAR ch-app-word AS COM-HANDLE NO-UNDO.
    CREATE 'Word.Application':U ch-app-word.                         /* Cria uma aplicaá∆o WORD */
    ch-app-word:WindowState = 2.                                     /* O estado dois para o Word Ç minimizado */
    ch-app-word:VISIBLE = NO.                                        /* Apenas para n∆o mostrar que o word est† sendo utilizado em tela */


    FOR EACH ttArquivo:

        run pi-acompanhar in h-acomp("Imprimindo Nota Fiscal " + ENTRY(3,ttArquivo.nomeArquivo,"-") ).
        IF  ttArquivo.Sequencia = 1 THEN do:
            ch-app-word:Documents:ADD(SESSION:TEMP-DIRECTORY + "/" + ttArquivo.nomeArquivo).        /* Inclui arquivo */
        end.
        ELSE DO:
            ch-app-word:SELECTION:EndKey(6).                         /* Posiciona cursor no final do arquivo */
            ch-app-word:SELECTION:InsertBreak(7).                    /* Qubra pagina antes de inserir arquivo */
            ch-app-word:SELECTION:Insertfile(SESSION:TEMP-DIRECTORY + "/" + ttArquivo.nomeArquivo).  /* Insere arquivo no documento aberto */
        END.
        ASSIGN lEntrou = YES.
    END.

    
    
    IF NOT lEntrou THEN
       ch-app-word:Documents:ADD().                                 /* Inclui arquivo */

    ch-app-word:ActiveDocument:SaveAs(tt-param-aux.arquivo).         /* Salva o arquivo aberto no WORD com o nome final do arquivo */
    IF  tt-param-aux.destino = 1 THEN DO: /* Impressora */
        
        IF INDEX (tt-param-aux.impressora-so," in session") > 0 THEN
            tt-param-aux.impressora-so = tt-param-aux.impressora-so + " on " + SESSION:PRINTER-PORT.
        /* Seleciona a impressora para impress∆o */
        ch-app-word:ActivePrinter = tt-param-aux.impressora-so.
        /* Imprime o documento na impressora selecionada */
        ch-app-word:printout(0). /* 0 : N∆o mostra erros nem advertencias */
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
    DEFINE VARIABLE chDoc               AS COM-HANDLE  NO-UNDO.
    DEFINE VARIABLE ch_documento        AS COM-HANDLE  NO-UNDO.
    DEFINE VARIABLE ch_novo             AS COM-HANDLE  NO-UNDO.
    
    DEFINE VARIABLE chNovo              AS COM-HANDLE  NO-UNDO.
    DEFINE VARIABLE i-cont              AS INTEGER     NO-UNDO.
    DEFINE VARIABLE i-cont-aux          AS INTEGER     NO-UNDO.
    DEFINE VARIABLE c-arquivo           AS CHAR        NO-UNDO.
    DEFINE VARIABLE c-caminho-aux       AS CHAR        NO-UNDO.
    DEFINE VARIABLE chave-danfe         AS CHAR        NO-UNDO.
    DEFINE VARIABLE c-impressora-padrao AS CHAR        NO-UNDO.
    DEFINE VARIABLE c-caminho-aux2      AS CHAR        NO-UNDO.

    ASSIGN c-impressora-padrao = SESSION:PRINTER-NAME.

    FOR EACH ttArquivo:
        
        CREATE 'Word.Application':U appWord.                         /* Cria uma aplicaá∆o WORD */
        appWord:WindowState = 2.                                     /* O estado dois para o Word Ç minimizado */
        appWord:VISIBLE = NO.                                        /* Apenas para n∆o mostrar que o word est† sendo utilizado em tela */

        appWord:documents:ADD().

        appWord:SELECTION:PageSetup:ORIENTATION  = 1.
        appWord:SELECTION:PageSetup:topMargin    = 24.5.
        appWord:SELECTION:PageSetup:BottomMargin = 24.5.
        appWord:SELECTION:PageSetup:LeftMargin   = 24.5.
        appWord:SELECTION:PageSetup:RightMargin  = 24.5.

        FIND FIRST tt-gati-nfe NO-LOCK
             WHERE tt-gati-nfe.cod-estabel = STRING(ENTRY(1,ttArquivo.nomeArquivo,"-"))
               AND tt-gati-nfe.serie       = STRING(ENTRY(2,ttArquivo.nomeArquivo,"-"))
               AND tt-gati-nfe.nr-nota-fis = STRING(ENTRY(3,ttArquivo.nomeArquivo,"-")) NO-ERROR.
        IF AVAIL tt-gati-nfe THEN
            ASSIGN chave-danfe = tt-gati-nfe.ds-chave.
        
        /*Verifica o tipo de saida do arquivo 1- impressora, 2- arquivo e 3- terminal */
        IF tt-param-aux.destino = 2 OR tt-param-aux.destino = 3 THEN DO:
             ASSIGN c-arquivo     = (tt-param-aux.arquivo + chave-danfe)
                    c-arquivo     = REPLACE(c-arquivo,".doc":U, "":U).

             ASSIGN c-caminho-aux  = ENTRY(NUM-ENTRIES(tt-param-aux.arquivo,"\"),tt-param-aux.arquivo,"\").
             ASSIGN c-caminho-aux2 = REPLACE(tt-param-aux.arquivo,c-caminho-aux,"").
             ASSIGN c-caminho-aux  = ENTRY(1,c-caminho-aux,".").
             ASSIGN c-caminho-aux  = REPLACE(c-arquivo,c-caminho-aux,"").
              
             AppWord:SELECTION:Insertfile(SESSION:TEMP-DIRECTORY + "/" + ttArquivo.nomeArquivo).
           
             AppWord:ActiveDocument:SaveAs(c-caminho-aux + ".doc").

             /*converte para pdf */
            /* RUN pi-pdf.
             RUN pi-save-pdf(INPUT c-caminho-aux2,
                             INPUT chave-danfe).  /*Comentado a chamada do pdf para nao rodar na TEAR*/ 
*/
             CREATE tt-doc.
             ASSIGN tt-doc.cod-estabel     = STRING(ENTRY(1,ttArquivo.nomeArquivo,"-"))
                    tt-doc.serie           = STRING(ENTRY(2,ttArquivo.nomeArquivo,"-"))
                    tt-doc.nr-nota-fis     = STRING(ENTRY(3,ttArquivo.nomeArquivo,"-"))
                    tt-doc.caminho-pdf     = c-caminho-aux + ".pdf"  /*Comentado a chamada do pdf para nao rodar na TEAR*/
                    tt-doc.caminho-doc     = c-caminho-aux + ".doc".

             appWord:activeDocument:CLOSE.
        END.
        
        /*appWord:QUIT().*/
    
        IF VALID-HANDLE (chDoc) THEN
           RELEASE OBJECT chDoc.
        
        IF  VALID-HANDLE(appWord) THEN
            RELEASE OBJECT AppWord.
    
    END.

    ASSIGN SESSION:PRINTER-NAME = c-impressora-padrao.
    
END PROCEDURE.

PROCEDURE pi-cria-tt-gati-nfe:

    DEFINE INPUT PARAM pCodEstabel LIKE nota-fiscal.cod-estabel.
    DEFINE INPUT PARAM pSerie      LIKE nota-fiscal.serie.
    DEFINE INPUT PARAM pNrNotaFis  LIKE nota-fiscal.nr-nota-fis.


    FIND FIRST gati-nfe NO-LOCK
         WHERE gati-nfe.cod-estabel = pCodEstabel
           AND gati-nfe.serie       = pSerie
           AND gati-nfe.nr-nota-fis = pNrNotaFis NO-ERROR.
    IF AVAIL gati-nfe THEN DO:
        FIND FIRST tt-gati-nfe NO-LOCK
             WHERE tt-gati-nfe.cod-estabel = gati-nfe.cod-estabel
               AND tt-gati-nfe.serie       = gati-nfe.serie      
               AND tt-gati-nfe.nr-nota-fis = gati-nfe.nr-nota-fis NO-ERROR.
        IF NOT AVAIL tt-gati-nfe THEN DO:
            CREATE tt-gati-nfe.
            BUFFER-COPY gati-nfe TO tt-gati-nfe.
        END.
    END.

END PROCEDURE.

PROCEDURE pi-pdf:

    CREATE "PDFCreator.clsPDFCreator" ch-pdfx NO-ERROR.

    IF NOT VALID-HANDLE(ch-pdfx) THEN DO:
        RUN utp/ut-msgs.p(INPUT "show":U,
                           INPUT 17006,
                           INPUT "Problemas na geraá∆o do PDF~~" +
                           "O PDFCreator nao esta instalado na sua maquina e/ou servidor.").
        RETURN "NOK":U.
    END.
    
END PROCEDURE.

PROCEDURE pi-save-pdf:

    DEFINE INPUT PARAM p-diretorio    AS CHAR NO-UNDO.
    DEFINE INPUT PARAM p-nome-arquivo AS CHAR NO-UNDO.

    IF NOT VALID-HANDLE(ch-pdfx) THEN
        RUN pi-pdf.

    ch-pdfx:cStart("/NoProcessignAtStartup", TRUE).

    assign ch-pdfx:cOption("UseAutosave")          = 1
           ch-pdfx:cOption("UseAutosaveDirectory") = 1
           ch-pdfx:cOption("AutosaveDirectory")    = p-diretorio
           ch-pdfx:cOption("AutosaveFilename")     = p-nome-arquivo
           ch-pdfx:cOption("AutosaveFormat")       = 0
           ch-pdfx:cdefaultprinter()               = "PDFCreator".


           ch-pdfx:cSaveOptions(1,1).

           appWord:printout(,,,,"PDFCreator",,).

           ch-pdfx:cprinterstop() = FALSE.

           IF ch-pdfx:cisconverted() = NO THEN DO
               WHILE ch-pdfx:cisconverted() <> YES:               
           END.

           ch-pdfx:cisconverted() = no.
           ch-pdfx:cclearcache().
           ch-pdfx:cprinterstop() = true.

           RUN pi-release.

END PROCEDURE.

PROCEDURE pi-release:

    if valid-handle(ch-work) then
       release object ch-work.

   if valid-handle(ch-pdfx) then
       release object ch-pdfx.


END PROCEDURE.

/* fim do programa */


