DEF INPUT PARAMETER r-pedido AS ROWID.

FIND ped-venda WHERE
     ROWID(ped-venda) = r-pedido
     NO-LOCK NO-ERROR.
IF AVAIL ped-vend THEN DO:
/*    {esinc\essp0155rp.i "ped-venda"} */
END.
ELSE DO:
/*    {esinc\essp0155rp.i "pp-ped-venda"} */
END.
