
{utp\ut-glob.i}
IF i-ep-codigo-usuario = "1" THEN DO: 
    IF NOT CONNECTED("dbaux") THEN
       CONNECT -db ems2med -ld dbaux -S 10031 -H 192.168.0.44 -N tcp.
END.
ELSE DO: 
     IF NOT CONNECTED("dbaux") THEN
        CONNECT -db ems2ima -ld dbaux -S 10030 -H 192.168.0.44 -N tcp.

END.

