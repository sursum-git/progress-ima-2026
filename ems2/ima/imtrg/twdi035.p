/****************************************************************************
** Programa : TWDI035 - trigger de Write para a tabela devol-cli - TONINHO **
** Data     : Maio 2016                                                    **
** Objetivo : trigger de Write para a tabela devol-cli                     **
** Empresa  : IMA                                                          **
** Vers∆o   : TOTVS 12.1.10                                                **
** Alterado :                                                              **
** *************************************************************************/
DEFINE PARAMETER BUFFER b-devol-cli-new FOR devol-cli.
DEFINE PARAMETER BUFFER b-devol-cli-old FOR devol-cli.  

IF NEW b-devol-cli-new THEN DO:
   FIND docum-est OF b-devol-cli-new NO-LOCK NO-ERROR.
   IF AVAIL docum-est THEN DO.
      IF docum-est.int-2 <> 0 THEN DO.
         ASSIGN b-devol-cli-new.codigo-rejei = docum-est.int-2.
      END.
      ELSE DO.
         FIND FIRST doc-fisico WHERE
                    doc-fisico.serie-docto   = docum-est.serie-docto  AND
                    doc-fisico.nro-docto     = docum-est.nro-docto    AND
                    doc-fisico.cod-emitente  = docum-est.cod-emitente AND
                    doc-fisico.tipo-nota     = docum-est.tipo-nota   
                    NO-LOCK NO-ERROR.
         IF AVAIL doc-fisico THEN
            ASSIGN b-devol-cli-new.codigo-rejei = doc-fisico.int-2.
      END.
   END.
END.
RETURN 'OK'.
