/* Programa: upc-pd4000e4.p
** Objetivo: Trigger de 'Entry' ou "Mouse-selec-click" para o bot∆o que salva o item 
**           Verifica se a digitaá∆o do acondicionamento est† correta    
**           Verifica se o tipo de embalagem est† cadastrada e chama o programa
**           que cadastra os tipos caso n∆o esteja
**           Verifica se os Lotes est∆o corretos (PP,PD,RP,RD,CA)
**           Verifica se a quantidade digitada Ç menor que a quatidade m°nima
**           do Corte comercial informado
** Autor...: Prodb - Toninho  Maráo/2004
*/

DEF INPUT PARAMETER h-fPage6 AS HANDLE.

DEF NEW GLOBAL SHARED VAR wh-acond AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-lote   AS WIDGET-HANDLE NO-UNDO.

DEF VAR h-campo AS WIDGET-HANDLE.
DEF VAR h-item AS WIDGET-HANDLE.
DEF VAR h-qt-pedida AS WIDGET-HANDLE.

ASSIGN h-campo = h-fPage6:FIRST-CHILD.
ASSIGN h-campo = h-campo:FIRST-CHILD.
DO WHILE VALID-HANDLE(h-campo):
   IF h-campo:NAME = 'it-codigo' THEN
      ASSIGN h-item = h-campo.

   IF h-campo:NAME = 'qt-pedida' THEN
      ASSIGN h-qt-pedida = h-campo.
   ASSIGN h-campo = h-campo:NEXT-SIBLING.
END.

FIND ITEM WHERE
     ITEM.it-codigo = h-item:SCREEN-VALUE NO-LOCK NO-ERROR.

IF ITEM.tipo-con-est = 4 THEN DO.
   IF wh-acond:SENSITIVE THEN DO.
      FIND corte-comerc WHERE
           corte-comerc.descricao = wh-acond:SCREEN-VALUE
           NO-LOCK NO-ERROR.
    
      IF NOT AVAIL corte-comerc THEN DO.
         MESSAGE "Corte Comercial n∆o Cadastrado..." 
                 VIEW-AS ALERT-BOX.
         APPLY 'entry' TO wh-acond.
         RETURN NO-APPLY.
      END.

      IF (corte-comerc.tp-embalag = 1 AND
          SUBSTR(wh-lote:SCREEN-VALUE,1,1) <> 'R') OR
         (corte-comerc.tp-embalag = 2 AND
          SUBSTR(wh-lote:SCREEN-VALUE,1,1) <> 'P') OR
         (corte-comerc.tp-embalag = 4 AND
          wh-lote:SCREEN-VALUE <> 'CA') THEN DO.
          MESSAGE "Lote Inv†lido para o Corte Comercial..." 
                  VIEW-AS ALERT-BOX.
          APPLY 'entry' TO wh-lote.
          RETURN NO-APPLY.
      END.
        
      FIND tp-embala WHERE 
           tp-embala.tipo = wh-acond:SCREEN-VALUE NO-LOCK NO-ERROR.
        
      IF NOT AVAIL tp-embala THEN DO.
         MESSAGE "Tipo de Embalagem n∆o Cadastrada..." SKIP(1)
                 "Deseja Cadastrar ?"
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
                  TITLE "" UPDATE l-deseja AS LOGICAL.
         IF l-deseja THEN DO.
            RUN esp/essp0089.w.
            FIND tp-embala WHERE 
                 tp-embala.tipo = wh-acond:SCREEN-VALUE NO-LOCK NO-ERROR.
            IF NOT AVAIL tp-embala THEN DO.
               MESSAGE  "Tipo de embalagem nao cadastrada." VIEW-AS ALERT-BOX.
               APPLY 'entry' TO wh-acond.
               RETURN NO-APPLY.
            END.
         END.
         ELSE DO.
            APPLY 'entry' TO wh-acond.
            RETURN NO-APPLY.
         END.
      END.
        
      IF LOOKUP(SUBSTR(wh-lote:SCREEN-VALUE,1,2),"PP,PD,RP,RD,CA") = 0 THEN DO.
         MESSAGE "Lote deve inicar com PP,PD,RP,RD,CA" VIEW-AS ALERT-BOX.
         APPLY 'entry' TO wh-lote.
         RETURN NO-APPLY. 
      END.

      IF DEC(h-qt-pedida:SCREEN-VALUE) < corte-comerc.compr-min THEN DO.
         MESSAGE "Quantidade Pedida Ç Inferior Ö Quantidade M°nima" SKIP
                 "do Corte Comercial Informado..." 
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'entry' TO wh-acond.
         RETURN NO-APPLY.
      END.
   END.
END.
APPLY 'CHOOSE' TO SELF.

