DEFINE TEMP-TABLE tt      NO-UNDO
    FIELD partida                   AS CHAR
    FIELD origem                    AS CHAR FORMAT 'x(100)'
    FIELD cod_empresa               AS CHAR
    FIELD cod_estab                 AS CHAR
    FIELD cod_emitente              AS CHAR FORMAT 'x(50)'
    FIELD desc_emitente             AS CHAR FORMAT 'x(200)'
    FIELD data                      AS DATE
    FIELD nro_docto                 AS CHAR FORMAT 'x(20)'
    FIELD serie                     AS CHAR FORMAT 'x(20)'
    FIELD parcela                   AS CHAR
    FIELD especie                   AS CHAR
    FIELD cod_refer                 AS CHAR
    /*FIELD cod_classif               AS INT
    FIELD desc_classif              AS CHAR FORMAT 'x(50)'*/
    FIELD conta_contabil            AS CHAR FORMAT 'x(12)'
    FIELD valor                     AS DECIMAL  
    FIELD conta_corrente            AS CHAR FORMAT 'x(20)'
    FIELD base                      AS CHAR 
    FIELD DESC_conta                AS CHAR FORMAT 'x(150)'
    FIELD tipo                      AS CHAR
    FIELD cc                        AS CHAR 
    FIELD DESC_cc                   AS CHAR FORMAT 'x(50)'
    FIELD rowidNota                 AS ROWID
    FIELD id_movto_corren           AS INT
    FIELD sequencia                 AS INT
    FIELD grupo_emitente            AS CHAR
    FIELD classificacao             AS CHAR FORMAT 'x(50)'
    FIELD grupo                     AS CHAR FORMAT 'x(50)'
    FIELD cCusto_Gerencial          AS CHAR FORMAT 'x(50)'
    FIELD cod_modulo                AS CHAR
    FIELD cod_param_desemb          AS INT
    FIELD cod_param_desemb_cCusto   AS INT
    FIELD LOG_desconsiderar         AS LOGICAL FORMAT "sim/nÆo"
    FIELD historico                 AS CHAR FORMAT 'x(2000)'
    FIELD transacao                 AS CHAR .


DEFINE TEMP-TABLE ttSemCtbl       NO-UNDO
      FIELD origem                    AS CHAR FORMAT 'x(100)'
      FIELD cod_estab                 AS CHAR
      FIELD data                      AS DATE
      FIELD nro_docto                 AS CHAR FORMAT 'x(20)'
      FIELD valor                     AS DECIMAL  
      FIELD conta_corrente            AS CHAR FORMAT 'x(20)'
      FIELD historico                 AS CHAR FORMAT 'x(2000)'
      FIELD id_movto_corren           AS INT
      FIELD sequencia                 AS INT.


DEFINE TEMP-TABLE ttFluxo    NO-UNDO
    FIELD linha                     AS INT
    FIELD origem                    AS CHAR FORMAT 'x(100)'
    FIELD cod_empresa               AS CHAR
    FIELD cod_estab                 AS CHAR
    FIELD cod_emitente              AS CHAR FORMAT 'x(50)'
    FIELD desc_emitente             AS CHAR FORMAT 'x(200)'
    FIELD data                      AS DATE
    FIELD nro_docto                 AS CHAR FORMAT 'x(20)'
    FIELD serie                     AS CHAR FORMAT 'x(20)'
    FIELD parcela                   AS CHAR
    FIELD especie                   AS CHAR
    FIELD valor                     AS DECIMAL  
    FIELD codTipoFluxo              AS CHAR
    FIELD descTipoFluxo             AS CHAR FORMAT 'x(50)'
    FIELD historico                 AS CHAR FORMAT 'x(100)'
    FIELD dia                       AS DATE
    FIELD codSemana                 AS INT
    FIELD DESCSemana                AS CHAR FORMAT 'x(50)'
    FIELD sequencia                 AS INT 
    FIELD contaCorrente             AS CHAR FORMAT 'x(50)'
    FIELD idMovtoCorrente           AS INT
    FIELD dtEmissao                 AS DATE.

DEFINE TEMP-TABLE ttFluxoFechamento NO-UNDO
    FIELD id_movto_corren AS INT
    FIELD vl_movto        AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    FIELD vl_aprop_fluxo  AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    FIELD vl_aprop_ctbl   AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX primario AS PRIMARY IS UNIQUE id_movto_corren   .


PROCEDURE sincrTtFluxoFechamento:
    DEFINE INPUT  PARAMETER pId          AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pTipoValor   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pVlMovto     AS DECIMAL     NO-UNDO.
    

    FIND ttFluxoFechamento
        WHERE ttFluxoFechamento.id_movto_corren = pId
        NO-ERROR.
    IF NOT AVAIL ttFluxoFechamento THEN DO:
       CREATE ttFluxoFechamento.
       ASSIGN ttFluxoFechamento.id_movto_corren = pId.
    END.
    CASE  pTipoValor:
    WHEN 'movto' THEN
          ASSIGN ttFluxoFechamento.vl_movto = pVlMovto.
    WHEN 'aprop_fluxo' THEN DO:
          ASSIGN ttFluxoFechamento.vl_aprop_fluxo =  ttFluxoFechamento.vl_aprop_fluxo + pVlMovto. 
    END.

    WHEN 'aprop_ctbl' THEN DO:
          ASSIGN ttFluxoFechamento.vl_aprop_ctbl =  ttFluxoFechamento.vl_aprop_ctbl + pVlMovto. 
    END.
    END CASE.


END PROCEDURE.

    
