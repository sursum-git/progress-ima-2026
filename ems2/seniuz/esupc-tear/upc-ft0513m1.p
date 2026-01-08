DEF INPUT PARAM p-wgh-frame      AS WIDGET-HANDLE NO-UNDO.

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

DEFINE VAR h-win             AS WIDGET-HANDLE NO-UNDO.
DEFINE VAR h-frame           AS WIDGET-HANDLE NO-UNDO.
DEFINE VAR h-objeto          AS WIDGET-HANDLE NO-UNDO.
DEFINE VAR h-c-arquivo-bloq  AS HANDLE.
DEFINE VAR h-rs-destino-bloq AS HANDLE.
DEFINE VAR h-prog            AS HANDLE NO-UNDO.

/* Variaveis para a include i-rprun.i */
DEFINE VAR rs-execucao            AS INTEGER INITIAL 1. 
DEFINE VAR c-programa-mg97        AS CHAR INIT "ft0513r". 
DEFINE VAR c-versao-mg97          AS CHAR.

DEFINE FRAME f-pg-imp rs-execucao AT ROW 5.75 COL 3 NO-LABEL.


/* Variaveis Globais */
DEFINE NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR wh-romaneio   AS WIDGET-HANDLE NO-UNDO.

APPLY 'choose' TO SELF.  /* Executa as Triggers Originais */

IF wh-romaneio:SCREEN-VALUE = 'NO' THEN RETURN.

ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
ASSIGN h-objeto = h-objeto:FIRST-CHILD.
DO WHILE VALID-HANDLE(h-objeto):
   IF h-objeto:NAME = "rs-destino-bloq" THEN
      ASSIGN h-rs-destino-bloq = h-objeto.

   IF h-objeto:NAME = "c-arquivo-bloq" THEN
      ASSIGN h-c-arquivo-bloq = h-objeto.

  ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
END.

CREATE tt-param.
ASSIGN tt-param.usuario        = c-seg-usuario
       tt-param.data-exec      = TODAY
       tt-param.hora-exec      = TIME
       tt-param.destino        = INT(h-rs-destino-bloq:SCREEN-VALUE)
       tt-param.arquivo        = IF tt-param.destino = 3
                                 THEN SESSION:TEMP-DIRECTORY + c-programa-mg97 + ".tmp"
                                 ELSE h-c-arquivo-bloq:SCREEN-VALUE
       tt-param.ini-dt-emissao = 01.01.0001
       tt-param.fin-dt-emissao = 12.31.9999
       tt-param.ini-nr-pedcli  = ""
       tt-param.fin-nr-pedcli  = "999999999999"
       tt-param.ini-nome-abrev = ""
       tt-param.fin-nome-abrev = "ZZZZZZZZZZZZ"
       tt-param.ini-cod-rep    = 0
       tt-param.fin-cod-rep    = 99999
       tt-param.sit-rolo       = "T" 
       tt-param.origem         = "NF".
       
ASSIGN h-win = p-wgh-frame:WINDOW.
ASSIGN h-frame = h-win:FIRST-CHILD.
DO WHILE VALID-HANDLE(h-frame):
   IF h-frame:NAME = "f-pg-sel" THEN DO.
      ASSIGN h-objeto = h-frame:FIRST-CHILD.
      ASSIGN h-objeto = h-objeto:FIRST-CHILD.
      DO WHILE VALID-HANDLE(h-objeto):
         CASE h-objeto:NAME.
             WHEN "c-cod-estabel" THEN
                ASSIGN tt-param.cod-estabel = h-objeto:SCREEN-VALUE.
             WHEN "c-serie" THEN
                ASSIGN tt-param.serie = h-objeto:SCREEN-VALUE.
             WHEN "c-ini-nota-fis" THEN
                ASSIGN tt-param.ini-nr-nota-fis = h-objeto:SCREEN-VALUE.
             WHEN "c-fim-nota-fis" THEN
                ASSIGN tt-param.fin-nr-nota-fis = h-objeto:SCREEN-VALUE.
         END CASE.
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
   END.
   ASSIGN h-frame = h-frame:NEXT-SIBLING.
END.

SESSION:SET-WAIT-STATE("general":U).
    
{include/i-rprun.i ftp/ft0513r.p} 
    
SESSION:SET-WAIT-STATE("":U).
    
IF tt-param.destino = 3 THEN DO.
   RUN utp/ut-utils.p PERSISTENT SET h-prog.
   RUN EXECUTE IN h-prog(INPUT "notepad.exe", 
            	         INPUT tt-param.arquivo).
   DELETE PROCEDURE h-prog.
END.




