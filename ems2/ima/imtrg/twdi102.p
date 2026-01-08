/****************************************************************************
** Programa : TWDI102 - trigger de Write para a tabela Local Entrega - TONINHO
** Data     : Novmembro 2015
** Objetivo : trigger de Write para a tabela loc-entr
** Empresa  : IMA 
** Vers∆o   : 2.04.001
** Alterado : 
** Fluxo    : Integrar Local de Entrega do Emitente com as Demais Bases
*****************************************************************************/
DEFINE PARAMETER BUFFER b-loc-entr-new FOR ems2cad.loc-entr.
DEFINE PARAMETER BUFFER b-loc-entr-old FOR ems2cad.loc-entr.  

DEFINE NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEFINE VAR c-erro AS CHAR.

FIND fnd_usuar_univ WHERE
     fnd_usuar_univ.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.


    
IF AVAIL b-loc-entr-new THEN DO:
   FIND tab-ocor WHERE
        tab-ocor.descricao = "Integrando_Local_Entrega" AND
        tab-ocor.c-campo[1] = STRING(ROWID(b-loc-entr-new))
        NO-LOCK NO-ERROR.

   IF NOT AVAIL tab-ocor THEN DO.
      CREATE tab-ocor.
      ASSIGN tab-ocor.descricao = "Integrando_Local_Entrega"
             tab-ocor.c-campo[1] = STRING(ROWID(b-loc-entr-new)).

      IF fnd_usuar_univ.cod_empresa = '1' THEN
         RUN esapi/conecta-dbaux.p (INPUT "MED-PRODUCAO").

      RUN esapi/integra-loc-entr.p (BUFFER b-loc-entr-new).

      FIND CURRENT tab-ocor NO-ERROR.
      DELETE tab-ocor.

      IF CONNECTED ("dbaux") THEN DISCONNECT dbaux.
   END.
END.
RETURN 'OK'.

   /*
   FIND emitente WHERE
        emitente.nome-abrev = b-loc-entr-new.nome-abrev 
        NO-LOCK NO-ERROR.

   FIND tab-ocor WHERE
        tab-ocor.descricao = "Integrando_Emitente" AND
        tab-ocor.i-campo[1] = emitente.cod-emitente
        NO-LOCK NO-ERROR.

    IF NOT AVAIL tab-ocor THEN DO.
       CREATE tab-ocor.
       ASSIGN tab-ocor.descricao = "Integrando_Emitente" 
              tab-ocor.i-campo[1] = emitente.cod-emitente.

       SESSION:SET-WAIT-STATE("general":U).
          RUN esapi/integra-loc-entr.p (INPUT emitente.cod-emitente).
       SESSION:SET-WAIT-STATE("":U).

       ASSIGN c-erro = RETURN-VALUE.

       FIND CURRENT tab-ocor NO-ERROR.
       DELETE tab-ocor.

       IF c-erro = 'NOK' THEN DO.
          MESSAGE "ERRO na Integraá∆o das Bases" SKIP
                  "Appserver n∆o Conectado, avise CPD (IMTRG/TWDI102.P"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN ERROR.
       END.
    END.
    */


 
