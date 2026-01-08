/************************************************************************
programa:esapi/consControlePreco.p
objetivo: Extrair todos os preáos validos de todos os itens conforme 
filtro de parametros
autor: tadeu silva
data: 09/2023
origem: demanda da Sra. Barbara
**************************************************************************/

{esapi/consControlePreco.i}


DEFINE INPUT  PARAMETER pItem AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pRef  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttItem .

DEFINE VARIABLE itemIni AS CHARACTER   NO-UNDO.
DEFINE VARIABLE itemFim AS CHARACTER   NO-UNDO.
DEFINE VARIABLE refIni  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE refFim  AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cErros  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dInd    AS DECIMAL     NO-UNDO.


//seta variaveis conforme parametros
IF pitem <> '' THEN
    ASSIGN itemIni = pitem
           itemFim = pItem.
ELSE 
    ASSIGN itemIni = ''
           itemFim = 'zzzzzzzzzzzzzzzz'.

IF pRef <> '' THEN
   ASSIGN refIni = pRef
          refFim = pRef.
ELSE
   ASSIGN refIni = ''
          refFim = 'zzzz'.

RUN esapi/getIndFinan.p(90,OUTPUT dInd, OUTPUT cErros).


FOR EACH controle_preco NO-LOCK
    WHERE controle_preco.it_codigo  >= itemIni
    AND   controle_preco.it_codigo  <= itemFim
    AND   controle_preco.cod_refer  >= refIni
    AND   controle_preco.cod_refer  <= refFim 
    AND   controle_preco.LOG_vencido = NO
    AND   controle_preco.dt_inicial <= TODAY
    AND   controle_preco.dt_final   >= TODAY .

    // desconsidera preáos zerados, pois os mesmos s∆o desconsiderados nos programas que utilizam os preáos
    IF controle_preco.vl_real = 0 
        AND controle_preco.vl_dolar = 0 THEN NEXT.

    // caso o item n∆o esteja cadastrado o preáo Ç desconsiderado
    FIND ITEM  NO-LOCK
           WHERE ITEM.it-codigo = controle_preco.it_codigo
           NO-ERROR.
    IF NOT AVAIL ITEM THEN NEXT.

    //item branco Ç desconsiderado
    IF ITEM.it-codigo = '' THEN NEXT.


    //caso o container seja diferente de zero e n∆o esteja aberto o mesmo tambÇm Ç desconsiderado
   IF controle_preco.nr_container <> 0 THEN DO:
      FIND pp-container NO-LOCK
          WHERE pp-container.nr-container = controle_preco.nr_container
          NO-ERROR.
      IF AVAIL pp-container THEN DO:
         IF pp-container.situacao <> 1 THEN NEXT.
      END.
   END.

   //desconsidera itens sem saldos fisicos - inicio
   IF controle_preco.tp_preco <> 2 THEN DO: // diferente de PI
      IF controle_preco.cod_refer = '' THEN
         FIND FIRST saldo-estoq NO-LOCK
         WHERE saldo-estoq.it-codigo = controle_preco.it_codigo
         AND   saldo-estoq.qtidade-atu > 0
         NO-ERROR.
 
      ELSE
        FIND FIRST saldo-estoq NO-LOCK
        WHERE saldo-estoq.it-codigo = controle_preco.it_codigo
        AND   saldo-estoq.cod-refer = controle_preco.cod_refer
        AND   saldo-estoq.qtidade-atu > 0
        NO-ERROR.                   

      IF NOT AVAIL saldo-estoq THEN NEXT.
   END.
   
   //desconsidera itens sem saldos fisicos - fim

   FIND FIRST ttItem
        WHERE ttItem.itCodigo    = controle_preco.it_codigo
        AND   ttItem.codRefer    = controle_preco.cod_refer
        AND   ttItem.nrContainer = controle_preco.nr_container
        NO-ERROR.
    IF NOT AVAIL ttItem THEN DO:
       CREATE ttItem.
       
       ASSIGN ttItem.descItem    = item.desc-item 
              ttItem.itCodigo    = controle_preco.it_codigo
              ttItem.codRefer    = controle_preco.cod_refer
              ttItem.nrcontainer = controle_preco.nr_container
             .                                            
    END.

    //TABELA PADR«O

    IF controle_preco.tb_preco = 1 AND controle_preco.tp_preco = 1 THEN // tabela padrao - PE
       ASSIGN ttItem.vlTbPadrao[1] = controle_preco.vl_real  * dInd
              ttItem.vlTbPadrao[2] = controle_preco.vl_dolar * dInd
        .

    IF controle_preco.tb_preco = 1 AND controle_preco.tp_preco = 2 THEN // tabela padrao - PI
       ASSIGN ttItem.vlTbPadrao[3] = controle_preco.vl_real  * dInd
              ttItem.vlTbPadrao[4] = controle_preco.vl_dolar * dInd
        .


    IF controle_preco.tb_preco = 1 AND controle_preco.tp_preco = 3 THEN // tabela padrao - PROMOÄ«O
       ASSIGN ttItem.vlOutlet[1] = controle_preco.vl_real   * dInd
              ttItem.vlOutlet[2] = controle_preco.vl_dolar  * dInd
        .


    //TABELA RUBI

    IF controle_preco.tb_preco = 2 AND controle_preco.tp_preco = 1 THEN // tabela rubi - PE
       ASSIGN ttItem.vlTbRubi[1] = controle_preco.vl_real  * dInd
              ttItem.vlTbRubi[2] = controle_preco.vl_dolar * dInd
        .

    IF controle_preco.tb_preco = 2 AND controle_preco.tp_preco = 2 THEN // tabela rubi - PI
       ASSIGN ttItem.vlTbRubi[3] = controle_preco.vl_real  * dInd
              ttItem.vlTbRubi[4] = controle_preco.vl_dolar * dInd
        .


    IF controle_preco.tb_preco = 2 AND controle_preco.tp_preco = 3 THEN // tabela rubi - PROMOÄ«O
       ASSIGN ttItem.vlTbRubix[1] = controle_preco.vl_real  * dInd
              ttItem.vlTbRubix[2] = controle_preco.vl_dolar * dInd
        .



END.



                                              
