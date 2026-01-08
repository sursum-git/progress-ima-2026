&IF Defined(Frame01Name) &THEN
    Define Frame {&Frame01Name}
       {&Frame01Defs}
       
            &IF Defined(FieldEndFrame) &THEN
            {&FieldEndFrame}
            &ENDIF
       
               With 1 down font 3 Size {&FrameSize} No-box.
&ENDIF

&IF Defined(Frame02Name) &THEN
    Define Frame {&Frame02Name}
       {&Frame02Defs}

            &IF Defined(FieldEndFrame) &THEN
            {&FieldEndFrame}
            &ENDIF

               With 1 down font 3 Size {&FrameSize} No-box.
&ENDIF

&IF Defined(Frame03Name) &THEN
    Define Frame {&Frame03Name}
       {&Frame03Defs}

            &IF Defined(FieldEndFrame) &THEN
            {&FieldEndFrame}
            &ENDIF

               With 1 down font 3 Size {&FrameSize} No-box.
&ENDIF

&IF Defined(Frame04Name) &THEN
    Define Frame {&Frame04Name}
       {&Frame04Defs}

            &IF Defined(FieldEndFrame) &THEN
            {&FieldEndFrame}
            &ENDIF

               With 1 down font 3 Size {&FrameSize} No-box.
&ENDIF

&IF Defined(Frame05Name) &THEN
    Define Frame {&Frame05Name}
       {&Frame05Defs}

            &IF Defined(FieldEndFrame) &THEN
            {&FieldEndFrame}
            &ENDIF

               With 1 down font 3 Size {&FrameSize} No-box.
&ENDIF

&IF Defined(Frame06Name) &THEN
    Define Frame {&Frame06Name}
       {&Frame06Defs}

            &IF Defined(FieldEndFrame) &THEN
            {&FieldEndFrame}
            &ENDIF

               With 1 down font 3 Size {&FrameSize} No-box.
&ENDIF

&IF Defined(Frame07Name) &THEN
    Define Frame {&Frame07Name}
       {&Frame07Defs}

            &IF Defined(FieldEndFrame) &THEN
            {&FieldEndFrame}
            &ENDIF


               With 1 down font 3 Size {&FrameSize} No-box.
&ENDIF

&IF Defined(Frame08Name) &THEN
    Define Frame {&Frame08Name}
       {&Frame08Defs}

            &IF Defined(FieldEndFrame) &THEN
            {&FieldEndFrame}
            &ENDIF


               With 1 down font 3 Size {&FrameSize} No-box.
&ENDIF

&IF Defined(Frame09Name) &THEN
    Define Frame {&Frame09Name}
       {&Frame09Defs}

            &IF Defined(FieldEndFrame) &THEN
            {&FieldEndFrame}
            &ENDIF


               With 1 down font 3 Size {&FrameSize} No-box.
&ENDIF

&IF Defined(Frame10Name) &THEN
    Define Frame {&Frame10Name}
       {&Frame10Defs}

            &IF Defined(FieldEndFrame) &THEN
            {&FieldEndFrame}
            &ENDIF


               With 1 down font 3 Size {&FrameSize} No-box.
&ENDIF

/* Definicao da Frame de Erros Inicio */
&IF Defined(FrameSizeError) &THEN

    Define Frame _Error
            'Erro:'         At Row 1 Col 1 
            vNumErro        At Row 1 Col 6 No-label
            vDesErro        At Row 2 Col 1 No-label

            &IF Defined(FieldEndFrame) &THEN
            {&FieldEndFrame}
            &ENDIF
            
                   With 1 down font 3 Size {&FrameSizeError} No-box.

&ELSE

    Define Frame _Error
            'Erro:'         At Row 1 Col 1 
            vNumErro        At Row 1 Col 6 No-label
            vDesErro        At Row 2 Col 1 No-label

            &IF Defined(FieldEndFrame) &THEN
            {&FieldEndFrame}
            &ENDIF

                   With 1 down font 3 Size {&FrameSize} No-box.

&ENDIF



Assign vDesErro:Width  = Frame _Error:Width  - 1
       vDesErro:Height = Frame _Error:height - 1.
/* Definicao da Frame de Erros Fim    */
