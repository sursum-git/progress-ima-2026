
DEFINE TEMP-TABLE ttNatur  NO-UNDO
    FIELD natOperacao AS CHAR
    FIELD descricao   AS CHAR
    FIELD narrativa   AS CHAR
    FIELD qtNF        AS INT
    FIELD uf          AS CHAR
    FIELD percICMS    AS DEC
    FIELD percReduz   AS DEC
    FIELD tribICMS    AS CHAR
    FIELD cfop        AS CHAR 
    FIELD dtUltNf     AS DATE
    FIELD cParamNat   AS CHAR
    INDEX primario AS PRIMARY IS UNIQUE natOperacao uf.
{esp/util.i}

DEFINE VARIABLE cListaTrib AS CHARACTER INIT 'Tributado,Isento,Outros,Reduzido,Diferido'  NO-UNDO.
DEFINE VARIABLE idParam    AS INTEGER     NO-UNDO.
FOR EACH nota-fiscal 
    WHERE nota-fiscal.cod-estabel = '5'
    AND dt-emis-nota >= 01.01.2018 
    AND nota-fiscal.dt-cancel = ? NO-LOCK.
    FIND natur-oper OF nota-fiscal NO-LOCK NO-ERROR.
    FIND ped-venda
        WHERE ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
        AND   ped-venda.nr-pedcli = nota-fiscal.nr-pedcli
        NO-LOCK NO-ERROR.
    FIND ped-venda-ext 
        WHERE ped-venda-ext.cod-estabel = ped-venda.cod-estabel
        AND   ped-venda-ext.nr-pedido =  ped-venda.nr-pedido
        NO-LOCK NO-ERROR.
   IF AVAIL ped-venda-ext THEN
      ASSIGN idParam = ped-venda-ext.cod_param_nat_operacao.
   ELSE
      ASSIGN idParam = 0.
     

    FIND ttNatur
        WHERE ttNatur.natOperacao = nota-fiscal.nat-operacao
        AND ttNatur.uf            = nota-fiscal.estado NO-LOCK NO-ERROR.
    IF NOT AVAIL ttNatur THEN DO:
       CREATE ttNatur.
       ASSIGN ttNatur.natOperacao = nota-fiscal.nat-operacao
              ttNatur.descricao   = natur-oper.denominacao
              ttNatur.narrativa   = natur-oper.narrativa 
              ttNatur.uf          = nota-fiscal.estado
              ttNatur.percICms    = natur-oper.aliquota-icm
              ttNatur.percReduz   = natur-oper.perc-red-icm
              ttNatur.tribIcms    = ENTRY(natur-oper.cd-trib-icm,cListaTrib)
              ttNatur.cfop        = natur-oper.cod-cfop.
    END.
    FIND PARAM_nat_operacao
        WHERE PARAM_nat_operacao.cod_param_nat_operacao = idParam
        AND    PARAM_nat_operacao.LOG_inativo = NO 
        NO-LOCK NO-ERROR.
     
    IF lookup(string(idParam),ttNatur.cParamNat) = 0 AND AVAIL param_nat_operacao THEN
       RUN incrValor(INPUT-OUTPUT ttNatur.cParamNat,string(idParam),",").

    ASSIGN ttNatur.qtNF = ttNatur.qtNF + 1.

    IF ttNatur.dtUltNF = ? THEN
       ASSIGN ttNatur.dtUltNF = nota-fiscal.dt-emis-nota.
    ELSE 
       IF ttNatur.dtUltNF < nota-fiscal.dt-emis-nota THEN
          ASSIGN ttNatur.dtUltNF = nota-fiscal.dt-emis-nota.


    
/*
perc-red-icm
aliquota-icm
cd-trib-icm 
*/

END.


OUTPUT TO c:\temp\lev_nat_oper.txt NO-CONVERT.
             
    FOR EACH ttNatur:
        EXPORT DELIMITER "|" ttNatur.
    END.
             

OUTPUT CLOSE.
