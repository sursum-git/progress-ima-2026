/* Programa: ESPD002
** Modulo..: Clientes
** Objetivo: Analisa Clientes e determina a Situa‡Æo dos mesmos
** Autor...: Antonio Geraldo de Souza - JANEIRO/2019
**
*/

/* Parametros de entrada logica obrigatoria */
{esp/espd002.i}
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita                   AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.       


CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.



DEFINE VARIABLE h-prog AS HANDLE      NO-UNDO.

IF NOT CONNECTED("dbaux") THEN
   RUN esapi/connect-ima-med.p.

RUN esrp/espd002rp-a.p (INPUT raw-param,
                        INPUT TABLE tt-raw-digita).

IF CONNECTED("dbaux") THEN
   DISCONNECT dbaux.

 
RUN esbo/boAtivosInativos.p PERSISTENT SET h-prog.
RUN setIntervalRepres IN h-prog(tt-param.cod-rep-ini, tt-param.cod-rep-fim).
RUN setDtRefer IN h-prog(tt-param.dtRefer).
RUN setInterValAtuSitCli IN h-prog(tt-param.sitAtuIni, tt-param.sitAtuFim).
RUN buscarClientesRepres IN h-prog.
