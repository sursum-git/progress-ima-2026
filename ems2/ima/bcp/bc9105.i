            
            &IF Defined(ProcedureClearFrame) 
            &THEN
                 &IF '{&ProcedureClearFrame}' <> ""
                 &then 
                      RUN {&ProcedureClearFrame}.
                 &ENDIF
            &ENDIF
            PAUSE 0.

            Hide All No-pause.
            View Frame _Error.
            Assign vNumErro:screen-value In Frame _Error = "{1}" {3}.
            &IF "{2}" = "return-value" &THEN
                 Assign vDesErro:screen-value In Frame _Error = {2} {4}. /* Alterado de "{2}" para {2} devido t‚cnica de tradu‡Æo */
            &else
                 Assign vDesErro:screen-value In Frame _Error = "{2}" {4}. 
            &ENDIF
                   
            Pause No-message.
            Hide Frame _Error.

            &IF Defined(ProcedureClearFrame) 
            &THEN
                 &IF '{&ProcedureClearFrame}' <> ""
                 &then 
                      RUN {&ProcedureClearFrame}.
                 &ENDIF
            &ENDIF
            Hide All No-pause.
            PAUSE 0.
