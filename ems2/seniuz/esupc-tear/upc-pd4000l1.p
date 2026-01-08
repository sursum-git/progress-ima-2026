/* Programa: upc-pd4000l1.p
** Objetivo: Trigger de 'Leave' para o campo fi-acond (Acondicionamento)
**           Verificar se o Acondiconamento est† relacionado com a referencia
**           do Item
**           Verificar se Acondicionamento est† cadastrado na tabela Tipo de 
**           Embalagem (tp-embala), se n∆o estiver chama o programa para efetuar
**           o cadastro
** Autor...: Prodb - Toninho  Maráo/2004
*/

DEF INPUT PARAMETER h-fPage AS HANDLE.
DEF NEW GLOBAL SHARED VAR wh-lote AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-campo AS WIDGET-HANDLE.

FIND corte-comerc WHERE
     corte-comerc.descricao = SELF:SCREEN-VALUE
     NO-LOCK NO-ERROR.

IF NOT AVAIL corte-comerc THEN DO.
   MESSAGE "Corte Comercial n∆o Cadastrado..." 
           VIEW-AS ALERT-BOX.
   APPLY 'entry' TO SELF.
   RETURN NO-APPLY.
END.

CASE corte-comerc.tp-embalag.
      WHEN 1 THEN ASSIGN wh-lote:SCREEN-VALUE = 'RP'.
      WHEN 2 THEN ASSIGN wh-lote:SCREEN-VALUE = 'PP'.
      WHEN 4 THEN ASSIGN wh-lote:SCREEN-VALUE = 'CA'.
END CASE.



FIND tp-embala WHERE 
     tp-embala.tipo = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.

IF NOT AVAIL tp-embala THEN DO.
   MESSAGE "Tipo de Embalagem n∆o Cadastrada..." SKIP(1)
           "Deseja Cadastrar ?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
            TITLE "" UPDATE l-deseja AS LOGICAL.
   IF l-deseja THEN DO.
      RUN esp/essp0089.w.
      FIND tp-embala WHERE 
           tp-embala.tipo = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
      IF NOT AVAIL tp-embala THEN DO.
         MESSAGE  "Tipo de embalagem nao cadastrada." VIEW-AS ALERT-BOX.
         APPLY 'entry' TO SELF.
         RETURN NO-APPLY.
      END.
   END.
   ELSE DO.
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
   END.
END.



