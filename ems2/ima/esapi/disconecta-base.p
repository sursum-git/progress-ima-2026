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

/* Foundation */
IF CONNECTED("emsfnd") THEN
   DISCONNECT emsfnd.


