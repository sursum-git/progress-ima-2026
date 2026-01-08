/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/* j-test.p - check to see if table1 & table2 are OF join-able */

/*
-------------------------- READ THIS!!! --------------------------

Note that code was stolen wholesale from j-find1.p and j-find2.p.
Look there and j-find.i for explanations of the algorithms used.

This code must be called TWICE - once with table1 first and table2,
then again (if the first pass fails) with table2 first and table1.

-------------------------- READ THIS!!! --------------------------
*/

/* pi_table1 and pi_table2 must be in the form dbname.tablename.  */
DEFINE VARIABLE  pi_table1 AS CHARACTER   NO-UNDO FORMAT 'x(30)'  INIT 'ems2med.ped-venda'.
DEFINE VARIABLE  pi_table2 AS CHARACTER   NO-UNDO FORMAT 'x(30)'  INIT  'ems2med.ped-item'.
DEFINE VARIABLE  po_ofok  AS LOGICAL     NO-UNDO.
//UPDATE pi_table1 pi_table2.



DEFINE VARIABLE v_joins AS INTEGER   NO-UNDO. /* count of join paths found */
DEFINE VARIABLE v_loop  AS INTEGER   NO-UNDO. /* loop */
DEFINE VARIABLE v_parts AS CHARACTER NO-UNDO. /* index components */

/*IF   LDBNAME("ems2med":u) <> SDBNAME(ENTRY(1,pi_table1,".":u)) 
  OR LDBNAME("ems2ima":u) <> SDBNAME(ENTRY(1,pi_table2,".":u)) THEN DO:
  CREATE ALIAS "ems2med" FOR DATABASE VALUE(SDBNAME(ENTRY(1,pi_table1,".":u))).
  CREATE ALIAS "ems2ima" FOR DATABASE VALUE(SDBNAME(ENTRY(1,pi_table2,".":u))).
  MESSAGE 'oi'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  RUN adecomm/_j-test.p (pi_table1,pi_table2,OUTPUT po_ofok).
  RETURN.
END.*/

FIND FIRST ems2med._Db
  WHERE ems2med._Db._Db-name =
    (IF DBTYPE(ENTRY(1,pi_table1,".":u)) = "PROGRESS":u 
      THEN ? ELSE ENTRY(1,pi_table1,".":u))
  NO-LOCK.
FIND ems2med._File
  OF ems2med._Db
  WHERE ems2med._File._File-name = ENTRY(2,pi_table1,".":u)
  NO-LOCK NO-ERROR.
IF NOT AVAILABLE(ems2med._File) THEN
DO:
  po_ofok = FALSE.
  RETURN.
END.

FIND FIRST ems2ima._Db
  WHERE ems2ima._Db._Db-name =
    (IF DBTYPE(ENTRY(1,pi_table2,".":u)) = "PROGRESS":u 
      THEN ? ELSE ENTRY(1,pi_table2,".":u))
  NO-LOCK.
FIND ems2ima._File
  OF ems2ima._Db
  WHERE ems2ima._File._File-name = ENTRY(2,pi_table2,".":u)
  NO-LOCK NO-ERROR.
IF NOT AVAILABLE(ems2ima._File) THEN
DO:
  po_ofok = FALSE.
  RETURN.
END.

/* For each unique non-word index of the table...
   Added NOT CAN-FIND for non-V7 database cases, i.e. V6 -dma */ 
FOR EACH ems2med._Index OF ems2med._File NO-LOCK
  WHERE ems2med._Index._Unique AND
    ((CAN-FIND (ems2med._Field WHERE ems2med._Field._Field-name = "_Wordidx":u) AND
     (ems2med._Index._Wordidx = ? OR ems2med._Index._Wordidx = 0)) OR
    NOT CAN-FIND (ems2med._Field WHERE ems2med._Field._Field-name = "_Wordidx":u)):

  /* grab the names and types of all the index components... */
  v_parts = "".
  FOR EACH ems2med._Index-field OF ems2med._Index NO-LOCK
    BY ems2med._Index-field._Index-seq:

    FIND ems2med._Field OF ems2med._Index-field NO-LOCK.
    MESSAGE ems2med._Field._Field-name
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    v_parts = v_parts
            + (IF ems2med._Index-field._Index-seq = 1 THEN "" ELSE ",":u)
            + ems2med._Field._Field-name + ",":u
            + STRING(ems2med._Field._dtype).
  END.
  MESSAGE v_parts
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  /* oops! no index components - must be default index */
  IF v_parts = "" THEN NEXT.

  FIND FIRST ems2ima._Field
    OF ems2ima._File
    WHERE ems2ima._Field._Field-name = ENTRY(1,v_parts)
      AND ems2ima._Field._dtype      = INTEGER(ENTRY(2,v_parts))
    NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ems2ima._Field THEN NEXT.

  /* match up rest of components */
  DO v_loop = 2 TO NUM-ENTRIES(v_parts) / 2:

      MESSAGE NUM-ENTRIES(v_parts) / 2 SKIP
              v_loop
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

    FIND ems2ima._Field
      OF ems2ima._File
      WHERE ems2ima._Field._Field-name = ENTRY(v_loop * 2 - 1,v_parts)
        AND ems2ima._Field._dtype = INTEGER(ENTRY(v_loop * 2,v_parts))
      NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ems2ima._Field THEN LEAVE.
  END.

  /* succeeded; can match with at least one OF */
  IF v_loop > NUM-ENTRIES(v_parts) / 2 THEN v_joins = v_joins + 1.
END.

/* only correct if exactly one way to perform OF */
po_ofok = (v_joins = 1).
MESSAGE po_ofok
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*RETURN.*/

/* j-test.p - end of file */

