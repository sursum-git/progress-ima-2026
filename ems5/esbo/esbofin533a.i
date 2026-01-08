DEFINE TEMP-TABLE ttSaldo
    FIELD cod_cta_corren  LIKE cta_corren.cod_cta_corren
    FIELD cod_banco       LIKE ems5.banco.cod_banco
    FIELD nome_banco      AS CHAR FORMAT 'x(50)'
    FIELD cod_estab       LIKE cta_corren.cod_estab
    FIELD vl_saldo        AS DECIMAL FORMAT '->>>,>>>,>>9.99'.
