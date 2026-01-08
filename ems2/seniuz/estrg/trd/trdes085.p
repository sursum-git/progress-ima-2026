TRIGGER PROCEDURE FOR DELETE OF ob-pcp-ref.

DEF BUFFER b-ob-pcp-ref FOR ob-pcp-ref.

IF NOT CAN-FIND(FIRST b-ob-pcp-ref WHERE
                      b-ob-pcp-ref.num-progr = ob-pcp-ref.num-progr AND
                      b-ob-pcp-ref.cod-refer <> ob-pcp-ref.cod-refer) THEN
   DELETE ob-pcp.
