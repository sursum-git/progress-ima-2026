DEF NEW GLOBAL SHARED VAR h-cod-estabel AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-nr-nota-ini AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-nr-nota-fin AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-serie       AS HANDLE NO-UNDO.

DEF NEW GLOBAL SHARED VAR h-ed-dest-email  AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-ed-copia-email AS HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-seg-usuario    AS CHAR NO-UNDO.

ASSIGN h-ed-dest-email:SCREEN-VALUE = ""
       h-ed-dest-email:SENSITIVE = NO
       h-ed-copia-email:SCREEN-VALUE = ""
       h-ed-copia-email:SENSITIVE = NO.

IF SELF:SCREEN-VALUE <> 'Nenhum' AND 
   h-nr-nota-ini:SCREEN-VALUE = h-nr-nota-fin:SCREEN-VALUE THEN DO.
   ASSIGN h-ed-dest-email:SENSITIVE = YES
          h-ed-copia-email:SENSITIVE = YES.

   FIND nota-fiscal WHERE
        nota-fiscal.cod-estabel = h-cod-estabel:SCREEN-VALUE AND
        nota-fiscal.serie = h-serie:SCREEN-VALUE AND
        nota-fiscal.nr-nota-fis = h-nr-nota-ini:SCREEN-VALUE
        NO-LOCK NO-ERROR.
   IF AVAIL nota-fiscal THEN DO.
      FIND emitente WHERE
           emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR.

      FOR EACH cont-emit OF emitente WHERE 
               cont-emit.area = 'Fiscal' NO-LOCK.
          ASSIGN h-ed-dest-email:SCREEN-VALUE = h-ed-dest-email:SCREEN-VALUE +
                                                cont-emit.e-mail.
      END.

      FIND usuar_mestre WHERE
           usuar_mestre.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.
      ASSIGN h-ed-copia-email:SCREEN-VALUE = usuar_mestre.cod_e_mail_local.
   END.
   ASSIGN h-ed-dest-email:SENSITIVE = YES.
   APPLY 'ENTRY' TO h-ed-dest-email.
END.

IF h-nr-nota-ini:SCREEN-VALUE <> h-nr-nota-fin:SCREEN-VALUE THEN DO.
   ASSIGN h-ed-dest-email:SCREEN-VALUE = 'ENVIO DE E-MAIL EM LOTE, PARA O CONTATO FISCAL DO CLIENTE DE CADA NOTA FISCAL'.
END.

