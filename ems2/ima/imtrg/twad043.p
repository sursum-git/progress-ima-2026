/****************************************************************************
** Programa : TWAD043 - trigger de Write para a tabela cont-emit  - TONINHO
** Data     : Novmembro 2014
** Objetivo : trigger de Write para a tabela cont-emit 
** Empresa  : IMA 
** Vers∆o   : 2.04.001
** Alterado : 
** Fluxo    : Integrar Contatos do Emitente com as Demais Bases
*****************************************************************************/
DEFINE PARAMETER BUFFER b-cont-emit-new FOR cont-emit.
DEFINE PARAMETER BUFFER b-cont-emit-old FOR cont-emit.  

DEFINE VAR c-erro AS CHAR.

IF AVAIL b-cont-emit-new THEN DO:
    /*
   FIND tab-ocor WHERE
        tab-ocor.descricao = "Integrando_Emitente" AND
        tab-ocor.i-campo[1] = b-cont-emit-new.cod-emitente
         NO-LOCK NO-ERROR.
    IF NOT AVAIL tab-ocor THEN DO.
       CREATE tab-ocor.
       ASSIGN tab-ocor.descricao = "Integrando_Emitente" 
              tab-ocor.i-campo[1] = b-cont-emit-new.cod-emitente.

       SESSION:SET-WAIT-STATE("general":U).
          /*RUN esapi/integra-emitente.p (INPUT b-cont-emit-new.cod-emitente).*/
       SESSION:SET-WAIT-STATE("":U).

       ASSIGN c-erro = RETURN-VALUE.

       FIND CURRENT tab-ocor NO-ERROR.
       DELETE tab-ocor.

       IF c-erro = 'NOK' THEN DO.
          MESSAGE "ERRO na Integraá∆o das Bases" SKIP
                  "Appserver n∆o Conectado, avise CPD (IMTRG/TWAD043.P"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN ERROR.
       END.
    END.
    */
END.

RETURN 'OK'.

