
IF NOT CONNECTED('ems5bkp') THEN
    CONNECT -db ems5 -ld ems5bkp -S 30032 -H 192.168.0.4 -N tcp.
