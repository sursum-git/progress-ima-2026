/****************************************************************************
** Programa : TWDI035 - trigger de Write para a tabela docum-est - TONINHO **
** Data     : Maio 2016                                                    **
** Objetivo : trigger de Write para a tabela docum-est                     **
** Empresa  : IMA                                                          **
** Vers∆o   : TOTVS 12.1.10                                                **
** Alterado :                                                              **
** *************************************************************************/
DEFINE PARAMETER BUFFER b-docum-est-new FOR docum-est.
DEFINE PARAMETER BUFFER b-docum-est-old FOR docum-est.  

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

IF NEW b-docum-est-new THEN DO:
   FIND FIRST doc-fisico WHERE
              doc-fisico.serie-docto   = b-docum-est-new.serie-docto  AND
              doc-fisico.nro-docto     = b-docum-est-new.nro-docto    AND
              doc-fisico.cod-emitente  = b-docum-est-new.cod-emitente AND
              doc-fisico.tipo-nota     = b-docum-est-new.tipo-nota   
              NO-LOCK NO-ERROR.
   IF AVAIL doc-fisico THEN
      ASSIGN b-docum-est-new.int-2 = doc-fisico.int-2.
END.

RETURN 'OK'.
