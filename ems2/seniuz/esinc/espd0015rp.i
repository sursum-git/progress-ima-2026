for each ob-pcp WHERE 
         ob-pcp.situacao = 1 AND
         ob-pcp.it-codigo >= tt-param.fi-ini-it-codigo AND
         ob-pcp.it-codigo <= tt-param.fi-fin-it-codigo NO-LOCK,
    EACH ob-pcp-ref OF ob-pcp WHERE
         ob-pcp-ref.situacao = 1 AND
         ob-pcp-ref.cod-refer >= tt-param.fi-ini-cod-refer AND 
         ob-pcp-ref.cod-refer <= tt-param.fi-fin-cod-refer and
         ((substr(ob-pcp-ref.cod-refer,3,4)= tt-param.fi-desenho AND   
           tt-param.l-inc-exc = yes) or
          (substr(ob-pcp-ref.cod-refer,3,4) <> tt-param.fi-desenho and 
           tt-param.l-inc-exc = no)  or
          (tt-param.fi-desenho = "")) no-lock,
    FIRST item WHERE 
          item.it-codigo = ob-pcp.it-codigo NO-LOCK,
    FIRST ref-item-ext WHERE
          ref-item-ext.it-codigo = ob-pcp.it-codigo AND
          ref-item-ext.cod-refer = ob-pcp-ref.cod-refer NO-LOCK
          break {1}.
                
    RUN pi-acompanhar in h-acomp (input ob-pcp.it-codigo + " " + ob-pcp-ref.cod-refer).

    ASSIGN l-imprimir = YES.

    if (item.compr-fabric = 1 and tt-param.c-imp-comp-fab = "C")
       or (item.compr-fabric = 2 and tt-param.c-imp-comp-fab = "F") then
       assign l-imprimir = no.

    if (substr(ob-pcp-ref.cod-refer,7,1) =  "0" and tt-param.c-imp-liso-est = "E")
       or (substr(ob-pcp-ref.cod-refer,7,1) <> "0" AND tt-param.c-imp-liso-est = "L") then 
          assign l-imprimir = no.
          
    IF ob-pcp-ref.qtd-sld-prog = 0 AND  
       ob-pcp-ref.qtd-proc = 0 AND 
       ob-pcp-ref.qtd-pron = 0 THEN 
       ASSIGN l-imprimir = NO.

    find first ref-estrut 
         where ref-estrut.it-codigo  = ob-pcp.it-codigo 
           and ref-estrut.cod-ref-it = ob-pcp-ref.cod-refer
         no-lock no-error.
     
    if avail ref-estrut and tt-param.tg-sem-estrutura = yes then 
       assign l-imprimir = no.
     
    if l-imprimir = yes then do:
       assign l-imprimir = no.
       if  tt-param.tg-programado = yes and ob-pcp-ref.qtd-sld-prog <> 0 then
           assign l-imprimir = yes.
       if  tt-param.tg-processo = yes and ob-pcp-ref.qtd-proc <> 0 then
           assign l-imprimir = yes.
       if  tt-param.tg-pronto = yes and ob-pcp-ref.qtd-pron <> 0 then
           assign l-imprimir = yes. 
    end.
        
    if l-imprimir then
       assign i-tot-prog-item = i-tot-prog-item + ob-pcp-ref.qtd-sld-prog
              i-tot-proc-item = i-tot-proc-item + ob-pcp-ref.qtd-proc
              i-tot-pron-item = i-tot-pron-item + ob-pcp-ref.qtd-pron
              i-tot-prog-ger  = i-tot-prog-ger + ob-pcp-ref.qtd-sld-prog
              i-tot-proc-ger  = i-tot-proc-ger + ob-pcp-ref.qtd-proc
              i-tot-pron-ger  = i-tot-pron-ger + ob-pcp-ref.qtd-pron.
    
    if last-of(ob-pcp-ref.cod-refer) and l-imprimir then do:
       find referencia-ext where
            referencia-ext.cod-refer = ob-pcp-ref.cod-refer no-lock no-error.
       assign c-fundo = if avail referencia-ext
                        then referencia-ext.cod-fundo
                        else "".
                               
       if avail item then
          ASSIGN c-compr-fabric = (if item.compr-fabric = 1 then "C" else "F").
       ELSE
          assign c-compr-fabric = "".
  
       if ob-pcp-ref.qtd-sld-prog <> 0 or ob-pcp-ref.qtd-proc <> 0 or
          ob-pcp-ref.qtd-pron <> 0 then do:
          assign i-tot-prog-proc = ob-pcp-ref.qtd-sld-prog +
                                   ob-pcp-ref.qtd-proc.
          find referencia where
               referencia.cod-refer = ob-pcp-ref.cod-refer no-lock no-error.
          IF AVAIL referencia THEN 
             ASSIGN c-desc-refer = SUBSTR(referencia.descricao,1,3).
          ELSE 
             ASSIGN c-desc-refer = "".
             
          assign c-descricao = item.descricao-1 + item.descricao-2.

          display ob-pcp.it-codigo
                  c-descricao
                  ob-pcp-ref.cod-refer
                  c-desc-refer
                  c-compr-fabric
                  c-fundo     
                  ob-pcp-ref.qtd-sld-prog
                  ob-pcp-ref.qtd-proc
                  i-tot-prog-proc
                  ob-pcp-ref.qtd-pron
                  with frame f-detalhe.
          down with frame f-detalhe.
          assign l-pulou = no.
       end.
    end.
  
    if last-of(ob-pcp.it-codigo) and
       (i-tot-prog-item <> 0 or
        i-tot-proc-item <> 0 or
        i-tot-pron-item <> 0) then do:
       assign i-tot-prog-proc = i-tot-prog-item + i-tot-proc-item.
       display "Total do Item"  @ ob-pcp.it-codigo
               i-tot-prog-item  @ ob-pcp-ref.qtd-sld-prog
               i-tot-proc-item  @ ob-pcp-ref.qtd-proc
               i-tot-prog-proc
               i-tot-pron-item  @ ob-pcp-ref.qtd-pron
               with frame f-detalhe.
       down 2 with frame f-detalhe.
       assign i-tot-prog-item = 0
              i-tot-proc-item = 0
              i-tot-pron-item = 0
              l-pulou        = yes.
    end.
    else
       if last-of(ob-pcp.it-codigo)
       and not l-pulou then do:
           put skip(1).
           assign l-pulou = yes.
       end.
end. /* ob-pcp-ref */
