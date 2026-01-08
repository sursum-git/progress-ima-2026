/******************************************************************************
**
**  Programa: esapi/getDescrNFImportacao.p
**
**  Objetivo: Retornar um texto que poder  ser adicionado nas informa‡äes
**  adicionais da nota fiscal
**
**  data: 08/2025
** autor:Tadeu silva
*****************************************************************************/
{esapi/getVlsNfImportacao.i ttImp}
{esp/util.i}
DEFINE INPUT  PARAMETER pRowid      AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER cDescricao  AS CHARACTER   NO-UNDO.
RUN esapi/getVlsNfImportacao.p(pRowid,OUTPUT TABLE ttImp).
FIND FIRST ttImp NO-LOCK .
ASSIGN cDescricao = "DADOS IMPORTA€ÇO:".
IF AVAIL ttImp  THEN DO:   
   RUN incrValor(INPUT-OUTPUT cDescricao, "DI:"  + ttImp.di, "").
   RUN incrValor(INPUT-OUTPUT cDescricao, "PROCESSO:" + string(ttImp.nrContainer) + "(" + ttImp.pedsImp + ")", "|").
   RUN incrValor(INPUT-OUTPUT cDescricao, "FOB:" + TRIM(string(ttImp.vlFob,">>>,>>>,>>9.99")), "|").
   RUN incrValor(INPUT-OUTPUT cDescricao, "PIS:" + trim(string(ttImp.vlPIS,">>>,>>>,>>9.99")), "|").
   RUN incrValor(INPUT-OUTPUT cDescricao, "COFINS:" + trim(string(ttImp.vlCOFINS,">>>,>>>,>>9.99")), "|").
   RUN incrValor(INPUT-OUTPUT cDescricao, "II:" + trim(string(ttImp.vlII,">>>,>>>,>>9.99")), "|").
   RUN incrValor(INPUT-OUTPUT cDescricao, "SEGURO INTERNACIONAL:" + TRIM(string(ttImp.vlSeguro,">>>,>>>,>>9.99")), "|").
   RUN incrValor(INPUT-OUTPUT cDescricao, "FRETE:" + trim(string(ttImp.vlFrete,">>>,>>>,>>9.99")), "|").
   
   
   
    
END.






