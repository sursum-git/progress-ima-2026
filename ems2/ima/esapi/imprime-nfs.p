DEF BUFFER empresa FOR mgcad.empresa.

DEF INPUT PARAMETER p-nr-nota-ini LIKE nota-fiscal.nr-nota-fis.
DEF INPUT PARAMETER p-nr-nota-fin LIKE nota-fiscal.nr-nota-fis.
DEF INPUT PARAMETER p-cod-estabel LIKE nota-fiscal.cod-estabel.
DEF INPUT PARAMETER p-serie       LIKE nota-fiscal.serie.

DEFINE NEW SHARED TEMP-TABLE tt-notas-impressas
       FIELD r-nota AS ROWID.

DEF NEW SHARED VAR r-nota  AS ROWID.

define temp-table tt-param
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

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita      AS RAW.

DEFINE VAR raw-param   AS RAW NO-UNDO.

/* Variaveis para a include i-rprun.i */
DEFINE VAR rs-execucao AS INTEGER INITIAL 1. 
DEFINE FRAME f-pg-imp rs-execucao AT ROW 5.75 COL 3 NO-LABEL.
DEFINE VAR c-programa-mg97 AS CHAR INIT "ft0518aa". 
DEFINE VAR c-versao-mg97 AS CHAR.
DEFINE VAR c-dispositivo AS CHAR.
DEFINE VAR c-arquivo AS CHAR.

DEF NEW GLOBAL SHARED VAR h-cb-envia-email AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-ed-dest-email  AS HANDLE NO-UNDO.
DEF VAR h-frame AS HANDLE.

{utp/ut-glob.i}

/* Procura por Dispositivo de Impress’o */
/*
FIND layout_impres_padr WHERE
     layout_impres_padr.cod_usuario = c-seg-usuario AND 
     layout_impres_padr.cod_proced  = "ft0516aa" 
     NO-LOCK NO-ERROR.
IF NOT AVAIL layout_impres_padr THEN
   FIND FIRST layout_impres_padr WHERE
              layout_impres_padr.cod_usuario = c-seg-usuario 
              NO-LOCK NO-ERROR.

FIND imprsor_usuar WHERE
     imprsor_usuar.nom_impressora = layout_impres_padr.nom_impressora AND
     imprsor_usuar.cod_usuario = layout_impres_padr.cod_usuario.

ASSIGN c-dispositivo = imprsor_usuar.nom_disposit_so.
*/
/* FIM DAS DEFINI°†ES DO RELATORIO ESPD0002RP.P */


CREATE FRAME h-frame.
CREATE COMBO-BOX h-cb-envia-email
        ASSIGN FRAME      = h-frame
               DATA-TYPE  = "CHARACTER"
               FORMAT     = "x(20)"
               LIST-ITEMS = 'Danfe e XML'
               ROW        = 4.1
               COLUMN     = 41
               WIDTH      = 15
               VISIBLE    = NO
               SCREEN-VALUE = 'Danfe e XML'.
CREATE EDITOR h-ed-dest-email
       ASSIGN FRAME     = h-frame
              DATA-TYPE = "CHARACTER"
              WIDTH     = 30
              HEIGHT    = 2.5
              ROW       = 6.8
              COL       = 53.7
              SCROLLBAR-VERTICAL = YES
              VISIBLE   = NO
              SCREEN-VALUE = 'ENVIO DE E-MAIL EM LOTE, PARA O CONTATO FISCAL DO CLIENTE DE CADA NOTA FISCAL'.

ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "FT0518" + p-nr-nota-ini + ".DOC".

CREATE tt-param.
ASSIGN tt-param.usuario             = c-seg-usuario
       tt-param.data-exec           = TODAY
       tt-param.hora-exec           = TIME
       tt-param.destino             = 2
       tt-param.arquivo             = c-arquivo
       tt-param.impressora-so       = c-dispositivo
       tt-param.cod-layout          = "DANFE-Mod.1"
       tt-param.ep-codigo           = '5'
       tt-param.c-cod-estabel       = p-cod-estabel
       tt-param.c-serie             = p-serie
       tt-param.c-nr-nota-fis-ini   = p-nr-nota-ini    
       tt-param.c-nr-nota-fis-fim   = p-nr-nota-fin   
       tt-param.de-cdd-embarque-ini = 0               
       tt-param.de-cdd-embarque-fim = 9999999          
       tt-param.c-hr-saida          = "000000"
       tt-param.banco               = 1
       tt-param.rs-imprime          = 1
       tt-param.nr-copias           = 1.

SESSION:SET-WAIT-STATE("general":U).

/* Imprime a Nota Fiscal */
{include/i-rprun.i ftp/ft0518rp.p} 
//{include/i-rprun.i ftp/ft051627.p}

SESSION:SET-WAIT-STATE("":U).



