DEF INPUT-OUTPUT PARAM w-comput AS CHAR NO-UNDO.

  FOR EACH FINCAD._connect:
      IF FINCAD._connect._connect-device <> ?       AND
         FINCAD._connect._connect-device <> "batch" AND
         FINCAD._connect._connect-device <> ""      AND 
         NOT CAN-DO(w-comput, FINCAD._connect._connect-device) THEN
         ASSIGN w-comput = IF w-comput = "" THEN CAPS(FINCAD._connect._connect-device) + "," + CAPS(FINCAD._connect._connect-device) ELSE w-comput + "," + CAPS(FINCAD._connect._connect-device) + "," + CAPS(FINCAD._connect._connect-device).
  END.

  FOR EACH FINMOV._connect:
      IF FINMOV._connect._connect-device <> ?       AND
         FINMOV._connect._connect-device <> "batch" AND
         FINMOV._connect._connect-device <> ""      AND 
         NOT CAN-DO(w-comput, FINMOV._connect._connect-device) THEN
         ASSIGN w-comput = IF w-comput = "" THEN CAPS(FINMOV._connect._connect-device) + "," + CAPS(FINMOV._connect._connect-device) ELSE w-comput + "," + CAPS(FINMOV._connect._connect-device) + "," + CAPS(FINMOV._connect._connect-device).
  END.
  FOR EACH HR209._connect:
      IF HR209._connect._connect-device <> ?       AND
         HR209._connect._connect-device <> "batch" AND
         HR209._connect._connect-device <> ""      AND 
         NOT CAN-DO(w-comput, HR209._connect._connect-device) THEN
         ASSIGN w-comput = IF w-comput = "" THEN CAPS(HR209._connect._connect-device) + "," + CAPS(HR209._connect._connect-device) ELSE w-comput + "," + CAPS(HR209._connect._connect-device) + "," + CAPS(HR209._connect._connect-device).
  END.
  FOR EACH dthrtosh._connect:
      IF dthrtosh._connect._connect-device <> ?       AND
         dthrtosh._connect._connect-device <> "batch" AND
         dthrtosh._connect._connect-device <> ""      AND 
         NOT CAN-DO(w-comput, dthrtosh._connect._connect-device) THEN
         ASSIGN w-comput = IF w-comput = "" THEN CAPS(dthrtosh._connect._connect-device) + "," + CAPS(dthrtosh._connect._connect-device) ELSE w-comput + "," + CAPS(dthrtosh._connect._connect-device) + "," + CAPS(dthrtosh._connect._connect-device).
  END.
  FOR EACH ems2uni._connect:
      IF ems2uni._connect._connect-device <> ?       AND
         ems2uni._connect._connect-device <> "batch" AND
         ems2uni._connect._connect-device <> ""      AND 
         NOT CAN-DO(w-comput, ems2uni._connect._connect-device) THEN
         ASSIGN w-comput = IF w-comput = "" THEN CAPS(ems2uni._connect._connect-device) + "," + CAPS(ems2uni._connect._connect-device) ELSE w-comput + "," + CAPS(ems2uni._connect._connect-device) + "," + CAPS(ems2uni._connect._connect-device).
  END.
