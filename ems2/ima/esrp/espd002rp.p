/* Programa: ESPD002
** Modulo..: Clientes
** Objetivo: Analisa Clientes e determina a Situa‡Æo dos mesmos
** Autor...: Antonio Geraldo de Souza - JANEIRO/2019
**
*/

/* Parametros de entrada logica obrigatoria */
{esp/espd002.i}
{esp/util.i}
DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita                   AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.       
DEFINE VARIABLE cProgEnc AS CHARACTER   NO-UNDO.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

DEFINE VARIABLE h-prog AS HANDLE      NO-UNDO.
RUN verificarNomeProgsCorrente(INPUT 'pi_disparar_ped_exec',
                               OUTPUT cProgEnc).

RUN esrp/espd002rp-a.p (INPUT raw-param,
                        INPUT TABLE tt-raw-digita).

IF cProgEnc <> "" THEN DO: //rpw
   ASSIGN tt-param.dtrefer = TODAY.
END.

   
RUN esbo/boAtivosInativos.p PERSISTENT SET h-prog.
RUN setIntervalRepres IN h-prog(tt-param.cod-rep-ini, tt-param.cod-rep-fim).
RUN setDtRefer IN h-prog(tt-param.dtRefer).
RUN setInterValAtuSitCli IN h-prog(tt-param.sitAtuIni, tt-param.sitAtuFim).
RUN buscarClientesRepres IN h-prog.
