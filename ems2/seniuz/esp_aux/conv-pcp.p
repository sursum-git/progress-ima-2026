FOR EACH ref-item-ext WHERE
         ref-item-ext.it-codigo >= '5' AND
         ref-item-ext.it-codigo <= '5ZZZZZZZZZZZZZZZZZ' AND
         ref-item-ext.qtd-proc > 0 NO-LOCK
         BREAK BY ref-item-ext.it-codigo.

    FIND LAST ob-pcp WHERE          
              ob-pcp.it-codigo = ref-item-ext.it-codigo NO-LOCK.

    IF NOT AVAIL ob-pcp THEN DO.
       CREATE ob-pcp.
       ASSIGN ob-pcp.num-progr = NEXT-VALUE(seq-pcp)
              ob-pcp.dt-progr  = TODAY
              ob-pcp.hr-progr  = TIME
              ob-pcp.it-codigo = ref-item-ext.it-codigo
              ob-pcp.usuario   = 'SUPER1'.
    END.

    FIND ob-pcp-ref OF ob-pcp WHERE
         ob-pcp-ref.cod-refer = ref-item-ext.cod-refer
         NO-LOCK NO-ERROR.

    IF NOT AVAIL ob-pcp-ref THEN DO.
        CREATE ob-pcp-ref.
        ASSIGN ob-pcp-ref.num-progr = ob-pcp.num-progr
               ob-pcp-ref.cod-refer = ref-item-ext.cod-refer.
    END.
    ASSIGN ob-pcp-ref.qtd-prog = ref-item-ext.qtd-proc
           ob-pcp-ref.qtd-proc = ref-item-ext.qtd-proc.
END.
