
DEFINE TEMP-TABLE tt
    FIELD origem            AS CHAR FORMAT 'x(100)'
    FIELD cod_empresa       AS CHAR
    FIELD cod_estab         AS CHAR
    FIELD cod_emitente      AS CHAR FORMAT 'x(50)'
    FIELD desc_emitente     AS CHAR FORMAT 'x(200)'
    FIELD data              AS DATE FORMAT "99/99/9999" 
    /*FIELD cod_classif       AS INT
    FIELD desc_classif      AS CHAR FORMAT 'x(50)'*/
    FIELD conta_contabil    AS CHAR FORMAT 'x(12)'
    FIELD valor             AS DECIMAL  
    FIELD conta_corrente    AS CHAR FORMAT 'x(20)'
    FIELD base              AS CHAR 
    FIELD DESC_conta        AS CHAR FORMAT 'x(150)'
    FIELD tipo              AS CHAR
    FIELD cc                AS CHAR 
    FIELD DESC_cc           AS CHAR FORMAT 'x(50)'
    FIELD rowidNota         AS ROWID
    FIELD id_movto_corren   AS INT
    FIELD num_id_tit        LIKE ems5.tit_ap.num_id_tit_ap
    FIELD sequencia         AS INT
    FIELD grupo_emitente    AS CHAR
    FIELD classificacao     AS CHAR FORMAT 'x(50)'
    FIELD grupo             AS CHAR FORMAT 'x(50)'
    FIELD cCusto_Gerencial  AS CHAR FORMAT 'x(50)'
    FIELD cod_modulo        AS CHAR
    FIELD cod_param_desemb  AS INT
    FIELD cod_param_desemb_cCusto AS INT
    FIELD LOG_desconsiderar AS LOGICAL FORMAT "sim/n∆o"
    FIELD historico         AS CHAR FORMAT 'x(2000)'
    FIELD cod_tit           LIKE ems5.tit_ap.cod_tit_ap.
