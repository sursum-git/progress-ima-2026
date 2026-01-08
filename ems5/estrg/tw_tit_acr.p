/****************************************************************************
** Programa: TW_tit_acr - trigger de write para a tabela tit_acr
** Data    : 04/2022
** Objetivo: Controlar a geraá∆o/cancelamento de boletos de emiss∆o pr¢pria
 a partir de titulos do ACR
** Empresa : IMA 
*****************************************************************************/
DEFINE PARAMETER BUFFER b-tit_acr_new FOR tit_acr.
DEFINE PARAMETER BUFFER b-tit_acr_old FOR tit_acr. 

DEFINE VARIABLE cMsg    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iNivel  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCont   AS INTEGER     NO-UNDO.
DEFINE VARIABLE cParam  AS CHARACTER   NO-UNDO.
{esp/util.i}

RUN getVlParametro('qt_niveis_prog_tit_acr', OUTPUT cParam).
ASSIGN iNivel = INT(cParam).
IF iNivel = 0 THEN
   ASSIGN iNivel = 5.

REPEAT iCont = 1 TO iNivel:
    RUN incrValor(INPUT-OUTPUT cMsg,PROGRAM-NAME(iCont),",").
END.

MESSAGE cMsg
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
