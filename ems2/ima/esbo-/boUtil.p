/*
  programa: esbo/boUtil.p
  Objetivo: Programa com v rios procedimentos genericos para otimizar
  blocos de programa‡Æo repetitivos
  autor:Tadeu Silva
  data:09/2020
*/

PROCEDURE convListaCombo:
  DEFINE INPUT-OUTPUT  PARAMETER pLista     AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER pSeparador        AS CHARACTER   NO-UNDO.
  DEFINE INPUT  PARAMETER pPosicaoInicial   AS INTEGER     NO-UNDO.
  DEFINE INPUT  PARAMETER pExibeNumero      AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE iCont                     AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cIncr                     AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cListaCB                  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE qtEntradas                AS INTEGER     NO-UNDO.
  DEFINE VARIABLE difPosIni                 AS INTEGER     NO-UNDO.
  ASSIGN qtEntradas = NUM-ENTRIES(pLista,pSeparador).

  IF pPosicaoInicial > 1 THEN DO:
     ASSIGN difPosIni  =  1 - pposicaoInicial 
            qtEntradas = qtEntradas - difPosIni.

  END.
  ELSE DO:
      ASSIGN difPosIni  = 1 - pPosicaoInicial .
      IF pPosicaoInicial < 1 THEN
         ASSIGN QtEntradas = qtEntradas - difPosIni.
  END.
  
  

  REPEAT iCont = pPosicaoInicial TO qtEntradas :
     /* MESSAGE 'posicao:' iCont SKIP
              'qt.entradas' qtEntradas SKIP
              'entrada:' Icont + difPosIni 
          VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      ASSIGN cIncr = IF pExibeNumero THEN STRING(iCont) + '-' + ENTRY(Icont + difPosIni ,PLista,pSeparador) + ',' + STRING(iCont)
                     ELSE  ENTRY(Icont + difPosIni ,PLista,pSeparador) + ',' + STRING(iCont) . 
      RUN incrValor(input-output cListaCB, INPUT cIncr,',').
  END.
  ASSIGN pLista = cListaCB.
END PROCEDURE.


PROCEDURE incrValor:
    DEFINE INPUT-OUTPUT PARAMETER pValor      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER       pIncr       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER       pSeparador  AS CHARACTER   NO-UNDO.
    IF pValor = '' THEN
       ASSIGN pValor = pIncr.
    ELSE 
      ASSIGN pValor = pValor + pSeparador + pIncr.

END PROCEDURE.


PROCEDURE getVlParametro:
    DEFINE INPUT  PARAMETER pParametro AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER cRetorno AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE tipo AS INT   NO-UNDO.
    RUN prlg/prp/busca-valor-param.p(pParametro, OUTPUT cRetorno,OUTPUT tipo).

END PROCEDURE.
