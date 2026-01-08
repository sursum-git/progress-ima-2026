OUTPUT TO c:\temp\client.txt.
FOR EACH clien_finan
WHERE  cod_portad_prefer <> '':
    DISP cdn_cliente .
   ASSIGN     clien_finan.cod_portad_prefer = ''
              clien_finan.cod_cart_bcia_prefer   = ''.
END.
