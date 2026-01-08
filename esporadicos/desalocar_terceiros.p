FOR EACH saldo-terc
WHERE saldo-terc.it-codigo = '515007'
AND   saldo-terc.cod-refer = 'A01'
AND NRO-DOCTO = '0000776':
    DISP saldo-terc WITH 1 COL WIDTH 550.
    UPDATE dec-1.
END.
