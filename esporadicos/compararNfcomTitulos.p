DEFINE TEMP-TABLE tt
    FIELD nrNota AS CHAR FORMAT 'x(12)'
    FIELD serie  AS CHAR
    FIELD parcela AS CHAR
    FIELD especie AS CHAR
    FIELD valor   AS DECIMAL
    FIELD estabems2 AS CHAR
    FIELD estabems5 AS CHAR
    FIELD LOG_ems5 AS LOGICAL
    FIELD LOG_ems2 AS LOGICAL
    FIELD dtEmissao AS DATE
    FIELD origem    AS CHAR FORMAT 'x(12)'
    FIELD LOG_movto AS LOGICAL.

    
FOR EACH nota-fiscal NO-LOCK
    WHERE nota-fiscal.dt-emis-nota >= 11.02.2017
    AND  nota-fiscal.dt-emis-nota  <= 11.07.2017:
    FOR EACH fat-duplic NO-LOCK
        WHERE fat-duplic.cod-estabel = nota-fiscal.cod-estabel
        AND   fat-duplic.serie       = nota-fiscal.serie
        AND   fat-duplic.nr-fatura   = nota-fiscal.nr-fatura:

        FIND FIRST tit_acr
            WHERE tit_acr.cod_estab  = '501'
            AND   tit_acr.cod_tit_acr = nota-fiscal.nr-nota-fis
            AND   tit_acr.cod_ser_docto = nota-fiscal.serie
            AND   tit_acr.cod_espec_docto = fat-duplic.cod-esp
            AND   tit_acr.cod_parcela     = fat-duplic.parcela 
            NO-LOCK NO-ERROR.
       FIND FIRST movto_tit_acr OF tit_acr
           WHERE movto_tit_acr.ind_trans_acr = 'implanta‡Æo'
           NO-LOCK NO-ERROR.

        CREATE tt.
        ASSIGN tt.nrNota = nota-fiscal.nr-fatura
               tt.serie  = nota-fiscal.serie
               tt.especie = fat-duplic.cod-esp
               tt.parcela = fat-duplic.parcela
               tt.valor   = fat-duplic.vl-parcela
               tt.LOG_ems2 = YES
               tt.Log_ems5 = AVAIL tit_acr
               tt.dtEmissao = nota-fiscal.dt-emis-nota
               tt.estabems2 = '5'
               tt.estabems5 = '501'
               tt.origem    = 'fat'
               tt.LOG_movto = AVAIL movto_tit_acr.
    END.
END.

FOR EACH tit_acr NO-LOCK
    WHERE tit_acr.cod_estab = '501'
    AND   tit_acr.dat_emis_docto >= 11.02.2017
    AND   tit_acr.dat_emis_docto <= 11.07.2017
    AND   tit_acr.cod_espec_docto = 'dp' .
    FIND FIRST movto_tit_acr OF tit_acr
           WHERE movto_tit_acr.ind_trans_acr = 'implanta‡Æo'
           NO-LOCK NO-ERROR.
    FIND FIRST tt
        WHERE tt.estabems5              = '501'
        AND   tit_acr.cod_tit_acr       = tt.nrNota
        AND   tit_acr.cod_ser_docto     = tt.serie
        AND   tit_acr.cod_espec_docto   = tt.especie
        AND   tit_acr.cod_parcela       = tt.parcela NO-ERROR.
    IF NOT AVAIL tt THEN DO:
       CREATE tt.
        ASSIGN tt.nrNota = tit_acr.cod_tit_acr
               tt.serie  = tit_acr.cod_ser_docto
               tt.especie = tit_acr.cod_espec_docto
               tt.parcela = tit_acr.cod_parcela
               tt.valor   = tit_acr.val_origin_tit_acr
               tt.LOG_ems2 = NO
               tt.Log_ems5 = YES
               tt.dtEmissao = tit_acr.dat_emis_docto
               tt.estabems2 = '5'
               tt.estabems5 = '501'
               tt.origem    = ind_orig_tit_acr
               tt.LOG_movto = AVAIL movto_tit_acr. 
    END.
END.

OUTPUT TO c:\temp\tt.txt.
PUT "Nota;Serie;Especie;Parcela;Valor;Ems2?;Ems5?;dt.emissÆo;estab.ems2;estab.ems5;origem;tem movto?" SKIP.
FOR EACH tt:
    EXPORT DELIMITER ";" tt .
END.
OUTPUT CLOSE.
