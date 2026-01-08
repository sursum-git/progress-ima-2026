define temp-table tt-param  no-undo
       FIELD destino           as integer
       FIELD arquivo           as char format "x(35)"
       FIELD usuario           as char format "x(12)"
       FIELD data-exec         as date
       FIELD hora-exec         as integer
       FIELD classifica        as integer
       FIELD desc-classifica   as char format "x(40)"
       FIELD cod-estabel       like nota-fiscal.cod-estabel 
       FIELD ini-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD fin-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD ini-cod-emitente  LIKE nota-fiscal.cod-emitente
       FIELD fin-cod-emitente  LIKE nota-fiscal.cod-emitente
       FIELD ini-cod-rep       LIKE nota-fiscal.cod-rep
       FIELD fin-cod-rep       LIKE nota-fiscal.cod-rep
       FIELD enviar-e-mail     AS LOG FORMAT "Sim/NÆo"
       FIELD subject-e-mail    AS CHAR FORMAT "x(40)"
       FIELD texto-e-mail      AS CHAR FORMAT "x(2000)"
       FIELD imp-oque          AS INT
       FIELD desc-imp-oque     as char format "x(40)"
       FIELD imp-param         AS LOG.

DEFINE VAR c-arquivo LIKE tt-param.arquivo INIT "SPOOL/ESFT0005.LST".

create tt-param.
assign tt-param.usuario          = "super"
       tt-param.destino          = 2 /* Arquivo */
       tt-param.arquivo          = "SPOOL/ESFT0005.LST"
       tt-param.data-exec        = TODAY
       tt-param.hora-exec        = TIME
       tt-param.cod-estabel      = "2"
       tt-param.ini-cod-emitente = 1                 
       tt-param.fin-cod-emitente = 999999999
       tt-param.ini-cod-rep      = 2
       tt-param.fin-cod-rep      = 999999
       tt-param.enviar-e-mail    = YES
       tt-param.subject-e-mail   = "Pedidos Faturados"
       tt-param.texto-e-mail     = "Segue anexo Rela‡Æo de Pedidos faturados no per¡odo de #PER-INI a #PER-FIN." +
                                   CHR(13) + CHR(13) + "Atenciosamente," + CHR(13) + CHR(13) +
                                   "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA." + CHR(13) + 
                                   "Departamento Comercial."
       tt-param.imp-oque         = 2 
       tt-param.desc-imp-oque    = "Somente Representantes sem e-mail"
       tt-param.imp-param        = NO.  

IF WEEKDAY(TODAY) = 1 THEN /* Dom */
   ASSIGN tt-param.ini-dt-emissao = TODAY - 2
          tt-param.fin-dt-emissao = TODAY - 1.
ELSE
IF WEEKDAY(TODAY) = 2 THEN /* Seg */
    ASSIGN tt-param.ini-dt-emissao = TODAY - 3
           tt-param.fin-dt-emissao = TODAY - 2.
ELSE                      /* Ter-Sab */
    ASSIGN tt-param.ini-dt-emissao = TODAY - 1
           tt-param.fin-dt-emissao = TODAY - 1.

/* Executar do programa RP.P que ir  criar o relat¢rio */
{include/i-rpexb.i}

{include/i-rprun.i esrp/esft0005rp.p}
