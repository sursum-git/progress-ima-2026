DEF INPUT PARAMETER p-nr-nota-ini LIKE nota-fiscal.nr-nota-fis.
DEF INPUT PARAMETER p-nr-nota-fin LIKE nota-fiscal.nr-nota-fis.
DEF INPUT PARAMETER p-cod-estabel LIKE nota-fiscal.cod-estabel.
DEF INPUT PARAMETER p-serie LIKE nota-fiscal.serie.

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino          AS INTEGER
       FIELD arquivo          AS CHAR FORMAT "x(35)"
       FIELD usuario          AS CHAR FORMAT "x(12)"
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       FIELD cod-estabel      AS CHAR
       FIELD serie            AS CHAR
       FIELD ini-nr-pedcli    LIKE ped-venda.nr-pedcli 
       FIELD fin-nr-pedcli    LIKE ped-venda.nr-pedcli
       FIELD sit-rolo         AS CHAR FORMAT "x"
       FIELD ini-nr-nota-fis  LIKE nota-fiscal.nr-nota-fis
       FIELD fin-nr-nota-fis  LIKE nota-fiscal.nr-nota-fis
       FIELD ini-dt-emissao   LIKE nota-fiscal.dt-emis
       FIELD fin-dt-emissao   LIKE nota-fiscal.dt-emis
       FIELD ini-nome-abrev   LIKE nota-fiscal.nome-ab-cli
       FIELD fin-nome-abrev   LIKE nota-fiscal.nome-ab-cli
       FIELD ini-cod-rep      AS INT
       FIELD fin-cod-rep      AS INT
       FIELD origem           AS CHAR
       FIELD imp-peso         AS INT.

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita      AS RAW.

DEFINE VAR raw-param   AS RAW NO-UNDO.

/* Variaveis para a include i-rprun.i */
DEFINE VAR rs-execucao AS INTEGER INITIAL 1. 
DEFINE FRAME f-pg-imp rs-execucao AT ROW 5.75 COL 3 NO-LABEL.
DEFINE VAR c-programa-mg97 AS CHAR INIT "essp0099".  /*"ft0513". */
DEFINE VAR c-versao-mg97 AS CHAR.
DEFINE VAR c-dispositivo AS CHAR.

{utp/ut-glob.i}

/* Procura por Dispositivo de Impress∆o */
FIND layout_impres_padr WHERE
     layout_impres_padr.cod_usuario = c-seg-usuario AND 
     layout_impres_padr.cod_proced  = "ESSP0099" 
     NO-LOCK NO-ERROR.

ASSIGN c-dispositivo = SESSION:TEMP-DIRECTORY + "ESSP0099" + ".tmp".
IF NOT AVAIL layout_impres_padr THEN
   FIND FIRST layout_impres_padr WHERE
              layout_impres_padr.cod_usuario = c-seg-usuario 
              NO-LOCK NO-ERROR.
IF AVAIL layout_impres_padr THEN
   ASSIGN c-dispositivo = TRIM(layout_impres_padr.nom_impressora) + ":" +
                          TRIM(layout_impres_padr.cod_layout_impres).

CREATE tt-param.
ASSIGN tt-param.usuario         = c-seg-usuario
       tt-param.data-exec       = TODAY
       tt-param.hora-exec       = TIME
       tt-param.destino         = IF AVAIL layout_impres_padr 
                                  THEN 1 ELSE 3
       tt-param.arquivo         = c-dispositivo
       tt-param.cod-estabel     = p-cod-estabel
       tt-param.serie           = p-serie
       tt-param.ini-nr-nota-fis = p-nr-nota-ini
       tt-param.fin-nr-nota-fis = p-nr-nota-fin
       tt-param.ini-dt-emissao  = 01.01.0001
       tt-param.fin-dt-emissao  = 12.31.9999
       tt-param.ini-nr-pedcli   = ""
       tt-param.fin-nr-pedcli   = "999999999999"
       tt-param.ini-nome-abrev  = ""
       tt-param.fin-nome-abrev  = "ZZZZZZZZZZZZ"
       tt-param.ini-cod-rep     = 0
       tt-param.fin-cod-rep     = 99999
       tt-param.origem          = "NF"
       tt-param.imp-peso        = 2   /* Imprime Coluna   1 - Peso     2 - Nuance */.

SESSION:SET-WAIT-STATE("general":U).

{include/i-rprun.i esrp/essp0099rp.p} 

SESSION:SET-WAIT-STATE("":U).

IF tt-param.destino = 3 THEN DO.
   RUN utp/ut-utils.p PERSISTENT SET h-prog.
   RUN EXECUTE IN h-prog(INPUT "notepad.exe", 
                         INPUT tt-param.arquivo).
   DELETE PROCEDURE h-prog.
END.

