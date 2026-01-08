DEF INPUT PARAMETER p-base AS CHAR.

IF CONNECTED ("dbaux") THEN DISCONNECT dbaux.

CASE p-base.
    WHEN 'IMA-PRODUCAO' THEN CONNECT -db ems2ima -ld dbaux -S 10030 -H 192.168.0.44 -N tcp.
    WHEN 'MED-PRODUCAO' THEN CONNECT -db ems2med -ld dbaux -S 10031 -H 192.168.0.44 -N tcp.
    WHEN 'IMA-BKP'      THEN CONNECT -db ems2ima -ld dbaux -S 30030 -H 192.168.0.4 -N tcp.
    WHEN 'MED-BKP'      THEN CONNECT -db ems2med -ld dbaux -S 30031 -H 192.168.0.4 -N tcp.
    WHEN 'IMA-TESTE'    THEN CONNECT -db ems2ima -ld dbaux -S 20030 -H 192.168.0.44 -N tcp.
    WHEN 'MED-TESTE'    THEN CONNECT -db ems2med -ld dbaux -S 20031 -H 192.168.0.44 -N tcp.
    WHEN 'IMA-BKT'      THEN CONNECT -db ems2ima -ld dbaux -S 40030 -H 192.168.0.4 -N tcp.
    WHEN 'MED-BKT'      THEN CONNECT -db ems2med -ld dbaux -S 40031 -H 192.168.0.4 -N tcp.
END CASE.
