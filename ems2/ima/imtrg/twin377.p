/****************************************************************************
** Programa : TWIN377 - trigger de Write para a tabela referencia - Tadeu  **
** Data     : Julho /2019                                                  **
** Objetivo : trigger de Write para a tabela referencia                    **
** Empresa  : IMA                                                          **
** Vers∆o   : TOTVS 12.1.23                                                **
** Alterado :                                                              **
** *************************************************************************/
DEFINE PARAMETER BUFFER b-referencia-new FOR referencia.
DEFINE PARAMETER BUFFER b-referencia-old FOR referencia.
DEFINE VARIABLE iOrdem AS INTEGER     NO-UNDO.

RUN esapi/getOrdemRef.p(b-referencia-new.cod-refer,OUTPUT iOrdem).
ASSIGN b-referencia-new.int-2 = iOrdem.

