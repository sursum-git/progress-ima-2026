OUTPUT TO c:\temp\tit_acr.txt.
DEFINE BUFFER bf_movto_tit_acr FOR movto_tit_acr.
DEFINE BUFFER bf_tit_acr FOR tit_acr.

/*FOR EACH tit_acr
    WHERE /*tit_acr.cod_estab = '501' 
    AND   tit_acr.cod_ser_docto =  '3'
    AND   tit_acr.cod_tit_Acr = '0086914'
    AND   tit_acr.cdn_client = 29435
    AND   tit_acr.cod_espec_docto = 'an'*/
    tit_acr.dat_emis_docto > 01.01.2018.*/
    //DISP tit_acr WITH 1 COL WIDTH 550.
    FOR EACH movto_tit_acr 
        WHERE movto_tit_acr.dat_transacao > 01.01.2018
        AND   movto_tit_acr.ind_trans_acr_abrev = 'liq'
        AND   movto_tit_acr.cod_espec_docto = 'an'
        BY movto_tit_acr.dat_transacao.
        FIND FIRST tit_acr OF movto_tit_acr NO-LOCK NO-ERROR.
        FIND FIRST emitente
            WHERE emitente.cod-emitente = tit_acr.cdn_cliente
            NO-LOCK NO-ERROR.

        FIND FIRST bf_movto_tit_acr 
            WHERE  bf_movto_tit_acr.cod_estab = movto_tit_acr.cod_estab
            AND     bf_movto_tit_acr.cod_refer = movto_tit_acr.cod_refer
            AND     bf_movto_tit_acr.num_id_movto_tit_acr <> movto_tit_acr.num_id_movto_tit_acr
            NO-LOCK NO-ERROR.
        IF AVAIL bf_movto_tit_acr THEN DO:
           FIND FIRST bf_tit_acr OF bf_movto_tit_acr
               NO-LOCK NO-ERROR.
           IF AVAIL bf_tit_acr THEN DO:
              EXPORT DELIMITER "|" 
                tit_acr.cod_estab 
                emitente.nome-emit 
                tit_acr.cod_espec_docto 
                tit_acr.cod_ser_docto 
                tit_acr.cod_parcela 
                tit_acr.cdn_cliente  
                tit_acr.cod_tit_acr 
                movto_tit_acr.dat_transacao 
                movto_tit_acr.dat_liquidac_tit_acr
                movto_tit_acr.val_movto_tit_acr
                bf_tit_acr.cod_tit_acr
                bf_tit_acr.cod_ser_docto
                bf_tit_acr.cod_espec_docto
                bf_tit_acr.cod_parcela
                /*tit_acr.val_sdo_tit_acr*/ .
               






           END.
        END.
         
        

    END.


/*
FOR EACH tit_acr
    WHERE tit_acr.cod_estab = '501' 
    AND   tit_acr.cod_ser_docto =  '3'
    AND   tit_acr.cod_tit_Acr = '0086914'
    AND   tit_acr.cdn_client = 29435
    AND   tit_acr.cod_espec_docto = 'dp'.
    //DISP tit_acr WITH 1 COL WIDTH 550.
    FOR EACH movto_tit_acr OF tit_acr.
        DISP movto_tit_acr WITH 1 COL WIDTH 550.
    END.

END.
*/

OUTPUT CLOSE.
