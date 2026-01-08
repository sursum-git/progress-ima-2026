/**************************************************************************
**                                                                       **
**   Include intermedi ria para gera‡Æo de log com as inconformidades    **
**   das integra‡äes com o CB                                            **
**                                                                       **
**************************************************************************/
find first tt-erro no-error.
if  avail tt-erro then do:
    if  l-multi = yes and i-num-ped-exec-rpw = 0 then do: 
        run cdp/cd0666a.w (output v-nom-arquivo-cb).
        if  v-nom-arquivo-cb <> " " then do:
            output to value(v-nom-arquivo-cb) paged page-size 64 append.
            view frame f-cabec.
            view frame f-rodape.
            for each tt-erro:
                assign c-mensagem-cb = tt-erro.mensagem.
                disp tt-erro.cd-erro
                     c-mensagem-cb
                     with frame f-consiste.
                down with frame f-consiste.
            end.
            output close.
       end.
    end.
end.
for each tt-erro:
    delete tt-erro.
end.
