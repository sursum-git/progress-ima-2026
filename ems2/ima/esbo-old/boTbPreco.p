/*
* Programa: esbo/boTbPreco.p
* Objetivo: Tratar todas as regras das tabelas de pre‡o customizadas
* Autor: Tadeu Silva
* Data: 04/2021
* 
* 
*/

 DEFINE BUFFER tb FOR tbs_preco.
 DEFINE TEMP-TABLE ttTbPreco LIKE tbs_preco.
 DEFINE VARIABLE iTbPreco AS INTEGER     NO-UNDO.


 PROCEDURE setTbPreco:
    
     DEFINE INPUT  PARAMETER pTb AS INTEGER     NO-UNDO.
     EMPTY TEMP-TABLE ttTbPreco.
     ASSIGN iTbPreco = pTb.
     FIND tb
         WHERE tb.tb_preco_id = iTbPreco
         NO-LOCK NO-ERROR.
     IF AVAIL tb THEN DO:
        CREATE ttTbPreco.
        BUFFER-COPY tb TO ttTbPreco.
     END.
 END PROCEDURE.

 PROCEDURE getPercReducComis:
     DEFINE OUTPUT  PARAMETER dePerc AS DECIMAL     NO-UNDO.
     FIND FIRST ttTbPreco NO-ERROR.
     IF AVAIL ttTbPreco THEN
        ASSIGN dePerc = ttTbPreco.perc_reduc_comis . 

 END PROCEDURE.

 PROCEDURE getCalcBonus:
     DEFINE OUTPUT  PARAMETER lCalcBonus AS LOGICAL     NO-UNDO.
     FIND FIRST ttTbPreco NO-ERROR.
     IF AVAIL ttTbPreco THEN
        ASSIGN lCalcBonus = ttTbPreco.log_calc_bonus .

 END PROCEDURE.
