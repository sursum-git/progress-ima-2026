DEF INPUT PARAMETER p-base AS CHAR.

/* EMS 2 */
IF CONNECTED('ems2') THEN DO.
   DISCONNECT ems2.
   DELETE ALIAS mgadm.     DELETE ALIAS mgdis.     DELETE ALIAS mgfis.  
   DELETE ALIAS mgcld.     DELETE ALIAS mgcex.     DELETE ALIAS mginv.  
   DELETE ALIAS mgind.     DELETE ALIAS mgmfg.     DELETE ALIAS mgmnt.  
   DELETE ALIAS mgmrp.     DELETE ALIAS mgscm.     DELETE ALIAS mgdbr.  
   DELETE ALIAS emsdca.    DELETE ALIAS ems2oe.    DELETE ALIAS mgsop.  
   DELETE ALIAS mguni.     DELETE ALIAS movadm.    DELETE ALIAS movdis. 
   DELETE ALIAS movfis.    DELETE ALIAS movind.    DELETE ALIAS movmfg. 
   DELETE ALIAS movmnt.    DELETE ALIAS movdbr.    DELETE ALIAS wmovdis.
   DELETE ALIAS ems2ima.   DELETE ALIAS ems2med.
END.

/* Financeiro */
IF CONNECTED('ems5') THEN DO.
   DISCONNECT ems5.
   DELETE ALIAS emsbas.    DELETE ALIAS emsedi.    DELETE ALIAS emsfin.
   DELETE ALIAS emsuni.    DELETE ALIAS emsven.    DELETE ALIAS emsnam.
   DELETE ALIAS movfin.
END.

CASE p-base.
    WHEN 'FOUNDATION' THEN DO.
       CONNECT -db emsfnd  -ld emsfnd -S 10010 -H 192.168.0.44 -N tcp.
    END.
    WHEN 'IMA-PRODUCAO' THEN DO.
        CONNECT -db ems2ima -ld ems2   -S 10030 -H 192.168.0.44 -N tcp.
        CONNECT -db ems5    -ld ems5   -S 10032 -H 192.168.0.44 -N tcp.
    END.
    WHEN 'MED-PRODUCAO' THEN DO.
        CONNECT -db ems2med -ld ems2   -S 10031 -H 192.168.0.44 -N tcp.
        CONNECT -db ems5    -ld ems5   -S 10032 -H 192.168.0.44 -N tcp.
    END.
    WHEN 'IMA-BKP' THEN DO.
        CONNECT -db ems2ima -ld ems2   -S 30030 -H 192.168.0.4 -N tcp.
        CONNECT -db ems5    -ld ems5   -S 30032 -H 192.168.0.4 -N tcp.
    END.
    WHEN 'MED-BKP' THEN DO.
        CONNECT -db ems2med -ld ems2   -S 30031 -H 192.168.0.4 -N tcp.
        CONNECT -db ems5    -ld ems5   -S 30032 -H 192.168.0.4 -N tcp.
    END.
    WHEN 'IMA-TESTE' THEN DO.
        CONNECT -db ems2ima -ld ems2   -S 20030 -H 192.168.0.44 -N tcp.
        CONNECT -db ems5    -ld ems5   -S 20032 -H 192.168.0.44 -N tcp.
    END.
    WHEN 'MED-TESTE' THEN DO.
        CONNECT -db ems2med -ld ems2   -S 20031 -H 192.168.0.44 -N tcp.
        CONNECT -db ems5    -ld ems5   -S 20032 -H 192.168.0.44 -N tcp.
    END.
    WHEN 'IMA-BKT' THEN DO.
        CONNECT -db ems2ima -ld ems2   -S 40030 -H 192.168.0.4 -N tcp.
        CONNECT -db ems5    -ld ems5   -S 40032 -H 192.168.0.4 -N tcp.
    END.
    WHEN 'MED-BKT' THEN DO.
        CONNECT -db ems2med -ld ems2   -S 40031 -H 192.168.0.4 -N tcp.
        CONNECT -db ems5    -ld ems5   -S 40032 -H 192.168.0.4 -N tcp.
    END.
END CASE.

IF NOT CONNECTED("emsfnd") THEN
   CONNECT -db emsfnd  -ld emsfnd -S 10010 -H 192.168.0.44 -N tcp.

/* EMS 2 */
IF CONNECTED('ems2') THEN DO.
   CREATE ALIAS mgadm   FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS mgdis   FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS mgfis   FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS mgcld   FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS mgcex   FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS mginv   FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS mgind   FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS mgmfg   FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS mgmnt   FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS mgmrp   FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS mgscm   FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS mgdbr   FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS emsdca  FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS ems2oe  FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS mgsop   FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS mguni   FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS movadm  FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS movdis  FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS movfis  FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS movind  FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS movmfg  FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS movmnt  FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS movdbr  FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS wmovdis FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS ems2ima FOR DATABASE ems2 NO-ERROR.
   CREATE ALIAS ems2med FOR DATABASE ems2 NO-ERROR.
END.

/* Financeiro */
IF CONNECTED('ems5') THEN DO.
   CREATE ALIAS emsbas  FOR DATABASE ems5  NO-ERROR.
   CREATE ALIAS emsedi  FOR DATABASE ems5  NO-ERROR.
   CREATE ALIAS emsfin  FOR DATABASE ems5  NO-ERROR.
   CREATE ALIAS emsuni  FOR DATABASE ems5  NO-ERROR.
   CREATE ALIAS emsven  FOR DATABASE ems5  NO-ERROR.
   CREATE ALIAS emsnam  FOR DATABASE ems5  NO-ERROR.
   CREATE ALIAS movfin  FOR DATABASE ems5  NO-ERROR.
END.

