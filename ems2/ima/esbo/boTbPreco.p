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
 DEFINE VARIABLE iTbPreco       AS INTEGER     NO-UNDO.
 DEFINE VARIABLE iTipoRepres    AS INTEGER     NO-UNDO.


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
 
 PROCEDURE setTpVendedor:
 
    DEFINE INPUT  PARAMETER pRepres AS INTEGER     NO-UNDO.    
    
    ASSIGN iTipoRepres = pRepres.
 
 
 END PROCEDURE.

 PROCEDURE getPercReducComis:
     DEFINE OUTPUT  PARAMETER dePerc AS DECIMAL     NO-UNDO.
     FIND FIRST ttTbPreco NO-ERROR.
     IF AVAIL ttTbPreco THEN DO:
        IF iTipoRepres = 1 THEN // Vendedor Interno.
        DO:
            ASSIGN dePerc = ttTbPreco.perc_reduc_comis_vend . 
        END.
        ELSE DO: //representante externo
            ASSIGN dePerc = ttTbPreco.perc_reduc_comis . 
        END.
        
     
     END.
        
        
             

 END PROCEDURE.

 PROCEDURE getCalcBonus:
     DEFINE OUTPUT  PARAMETER lCalcBonus AS LOGICAL     NO-UNDO.
     FIND FIRST ttTbPreco NO-ERROR.
     IF AVAIL ttTbPreco THEN
        ASSIGN lCalcBonus = ttTbPreco.log_calc_bonus .

 END PROCEDURE.
