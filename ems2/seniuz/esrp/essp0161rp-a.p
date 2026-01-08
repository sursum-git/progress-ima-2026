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

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita      AS RAW.

DEFINE VAR raw-param   AS RAW NO-UNDO.

/* Variaveis para a include i-rprun.i */
DEFINE VAR rs-execucao AS INTEGER INITIAL 1. 
DEFINE FRAME f-pg-imp rs-execucao AT ROW 5.75 COL 3 NO-LABEL.
DEFINE VAR c-programa-mg97 AS CHAR INIT "ft0516aa". 
DEFINE VAR c-versao-mg97 AS CHAR.
DEFINE VAR c-dispositivo AS CHAR.
DEFINE VAR c-arquivo AS CHAR.

{utp/ut-glob.i}

ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "FT0516aa" + REPLACE(STRING(TODAY),"/","") + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".DOC".

/* Procura por Dispositivo de Impress∆o */
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

/* FIM DAS DEFINIÄÂES DO RELATORIO ESPD0002RP.P */

CREATE tt-param.
ASSIGN tt-param.usuario           = c-seg-usuario
       tt-param.data-exec         = ?
       tt-param.hora-exec         = 0
       tt-param.destino           = IF AVAIL layout_impres_padr 
                                    THEN 1 ELSE 3 
       tt-param.arquivo           = c-arquivo
       tt-param.impressora-so     = c-dispositivo
       tt-param.c-cod-estabel     = p-cod-estabel
       tt-param.c-serie           = p-serie
       tt-param.c-nr-nota-fis-ini = p-nr-nota-ini    
       tt-param.c-nr-nota-fis-fim = p-nr-nota-fin   
       tt-param.i-nr-embarque-ini = 0               
       tt-param.i-nr-embarque-fim = 999999          
       tt-param.rs-imprime        = 1.

SESSION:SET-WAIT-STATE("general":U).

/* Imprime a Nota Fiscal */
{include/i-rprun.i ftp/ft051627.p}

SESSION:SET-WAIT-STATE("":U).

/* Imprimir Romaneio */
RUN esrp/essp0161rp-r.p (INPUT p-nr-nota-ini,
                         INPUT p-nr-nota-fin,
                         INPUT p-cod-estabel,
                         INPUT p-serie). 


