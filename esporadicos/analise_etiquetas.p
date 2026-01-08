DEFINE VARIABLE de AS DECIMAL     NO-UNDO.
DEFINE VARIABLE de1 AS DECIMAL     NO-UNDO.
FOR EACH ob-etiqueta
        WHERE it-codigo = '565452' AND quantidade <> 0
        AND ob-origem = '187718' and (situacao = 3 or situacao = 4):
    //DISP int(situacao) ob-origem quantidade cod-refer.
    ASSIGN de = de + quantidade.

END.
DISP de.

FOR EACH saldo-estoq
    WHERE it-codigo = '565452':
    ASSIGN de1 = de1 + saldo-estoq.qtidade-atu.
END.
DISP de1 de - de1.
