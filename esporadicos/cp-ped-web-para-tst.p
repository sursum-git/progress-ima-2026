//109291
DEFINE VARIABLE iPedido AS INTEGER     NO-UNDO.
UPDATE iPedido.
FOR EACH espec.peds_web
    WHERE espec.peds_web.ped_web_id = iPedido:
    CREATE tst.peds_web.
    BUFFER-COPY espec.peds_web TO tst.peds_web.
    FOR EACH espec.itens_ped_web 
        WHERE espec.itens_ped_web.ped_web_id = tst.peds_web.ped_web_id :
        CREATE tst.itens_ped_web.
        BUFFER-COPY espec.itens_ped_web TO tst.itens_ped_web.                
    END.
END.
