FOR EACH docto-manif-destin
    WHERE cod-chave-nfe = '31220819425487000182550010001218641000710094'.
    FOR EACH ret-manif-destin 
        WHERE ret-manif-destin.cod-chave-nfe =  docto-manif-destin.cod-chave-nfe :
        DELETE ret-manif-destin.

    END.
    DELETE docto-manif-destin.
END.

