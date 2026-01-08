DEF TEMP-TABLE tt-conteudo
    FIELD cod-emitente   AS INT FORMAT ">>>>>9"
    FIELD nome-emit      AS CHAR FORMAT "x(45)"
    FIELD cod-ramo-ativ  AS INT FORMAT ">>>>>9"
    FIELD desc-ramo-ativ AS CHAR FORMAT "x(45)"
    FIELD cod-finalid  AS INT FORMAT ">>>>>9"
    FIELD desc-finalid AS CHAR FORMAT "x(45)"
    FIELD varejo AS CHAR FORMAT "X"
    FIELD atacado AS CHAR FORMAT "X"
    FIELD industria AS CHAR FORMAT "X"
    FIELD servico AS CHAR FORMAT "X"
    FIELD email-fin AS CHAR FORMAT "x(35)" 
    FIELD email-fis AS CHAR FORMAT "x(35)"
    FIELD email-com AS CHAR FORMAT "x(35)"
    FIELD email-out AS CHAR FORMAT "x(35)"
    INDEX indice1 cod-emitente.

def var h-acomp as handle no-undo.
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH mgadm.nota-fiscal WHERE 
         mgadm.nota-fiscal.dt-emis-nota >= 01.01.2013 AND 
         mgadm.nota-fiscal.dt-cancela    = ? AND 
        (SUBSTRING(mgadm.nota-fiscal.nat-operacao,1,3) = "512" OR
         SUBSTRING(mgadm.nota-fiscal.nat-operacao,1,3) = "612" OR
         SUBSTRING(mgadm.nota-fiscal.nat-operacao,1,3) = "619" OR
         SUBSTRING(mgadm.nota-fiscal.nat-operacao,1,3) = "712") 
         USE-INDEX ch-sit-nota NO-LOCK,
    EACH mgadm.emitente WHERE 
         mgadm.emitente.nome-abrev = mgadm.nota-fiscal.nome-ab-cli AND 
         mgadm.emitente.ind-cre-cli <> 4 NO-LOCK.

    RUN pi-acompanhar IN  h-acomp (INPUT "IMA" + "-" + STRING(mgadm.nota-fiscal.dt-emis-nota) + '-' + mgadm.nota-fiscal.nome-ab-cli).

    IF LOOKUP(mgadm.emitente.estado,"MG,ES") = 0 AND
       mgadm.emitente.ins-estadual = 'ISENTO' THEN NEXT.

    FIND FIRST tt-conteudo WHERE 
               tt-conteudo.cod-emitente = mgadm.nota-fiscal.cod-emitente NO-LOCK NO-ERROR.

    IF AVAIL tt-conteudo THEN NEXT.

    FIND FIRST ext-emitente WHERE
               ext-emitente.cod-emitente = mgadm.emitente.cod-emitente 
               NO-LOCK NO-ERROR.

    FIND ramo-ativ WHERE 
         ramo-ativ.cod-ramo-ativ = ext-emitente.cod-ramo-ativ NO-LOCK NO-ERROR.

    FIND finalidades_venda WHERE
         finalidades_venda.cod_finalidade = ext-emitente.cod_finalidade_venda
         NO-LOCK NO-ERROR.

    CREATE tt-conteudo.
    ASSIGN tt-conteudo.cod-emitente    = mgadm.nota-fiscal.cod-emitente
           tt-conteudo.nome-emit       = mgadm.emitente.nome-emit
           tt-conteudo.cod-ramo-ativ   = ramo-ativ.cod-ramo-ativ WHEN AVAIL ramo-ativ
           tt-conteudo.desc-ramo-ativ  = ramo-ativ.descricao WHEN AVAIL ramo-ativ
           tt-conteudo.cod-finalid     = finalidades_venda.cod_finalidade_venda WHEN AVAIL finalidades_venda
           tt-conteudo.desc-finalid    = finalidades_venda.desc_finalidade_venda WHEN AVAIL finalidades_venda.

    IF AVAIL ext-emitente THEN DO.
       ASSIGN tt-conteudo.varejo      = IF ext-emitente.log_varejo THEN "Y" ELSE "N"
              tt-conteudo.atacado     = IF ext-emitente.log_atacado THEN "Y" ELSE "N"
              tt-conteudo.industria   = IF ext-emitente.log_industria THEN "Y" ELSE "N"        
              tt-conteudo.servico     = IF ext-emitente.log_servico THEN "Y" ELSE "N".
    END.

    FOR EACH mgadm.cont-emit OF mgadm.emitente NO-LOCK.
        CASE mgadm.cont-emit.area.
            WHEN "Financeiro" THEN ASSIGN tt-conteudo.email-fin = mgadm.cont-emit.e-mail.
            WHEN "Fiscal" THEN ASSIGN tt-conteudo.email-fis = mgadm.cont-emit.e-mail.
            WHEN "Comercial" THEN ASSIGN tt-conteudo.email-com = mgadm.cont-emit.e-mail.
            WHEN "Outros" THEN ASSIGN tt-conteudo.email-out = mgadm.cont-emit.e-mail.
        END CASE.
    END.
END.


FOR EACH dbaux.nota-fiscal WHERE 
         dbaux.nota-fiscal.dt-emis-nota >= 01.01.2013 AND 
         dbaux.nota-fiscal.dt-cancela    = ? AND 
        (SUBSTRING(dbaux.nota-fiscal.nat-operacao,1,3) = "512" OR
         SUBSTRING(dbaux.nota-fiscal.nat-operacao,1,3) = "612" OR
         SUBSTRING(dbaux.nota-fiscal.nat-operacao,1,3) = "619" OR
         SUBSTRING(dbaux.nota-fiscal.nat-operacao,1,3) = "712") 
         USE-INDEX ch-sit-nota NO-LOCK,
    EACH dbaux.emitente WHERE 
         dbaux.emitente.nome-abrev = dbaux.nota-fiscal.nome-ab-cli AND 
         dbaux.emitente.ind-cre-cli <> 4 NO-LOCK.
    FIND FIRST tt-conteudo WHERE 
               tt-conteudo.cod-emitente = dbaux.nota-fiscal.cod-emitente NO-LOCK NO-ERROR.

    RUN  pi-acompanhar IN  h-acomp (INPUT "MED" + "-" + STRING(dbaux.nota-fiscal.dt-emis-nota) + '-' + dbaux.nota-fiscal.nome-ab-cli).

    IF LOOKUP(dbaux.emitente.estado,"MG,ES") = 0 AND
       dbaux.emitente.ins-estadual = 'ISENTO' THEN NEXT.

    IF AVAIL tt-conteudo THEN NEXT.

    FIND ext-emitente WHERE 
         ext-emitente.cod-emitente = dbaux.emitente.cod-emitente NO-LOCK NO-ERROR.

    FIND ramo-ativ WHERE 
         ramo-ativ.cod-ramo-ativ = ext-emitente.cod-ramo-ativ NO-LOCK NO-ERROR.

    FIND finalidades_venda WHERE
         finalidades_venda.cod_finalidade = ext-emitente.cod_finalidade_venda
         NO-LOCK NO-ERROR.

    CREATE tt-conteudo.
    ASSIGN tt-conteudo.cod-emitente    = dbaux.nota-fiscal.cod-emitente
           tt-conteudo.nome-emit       = dbaux.emitente.nome-emit
           tt-conteudo.cod-ramo-ativ   = ramo-ativ.cod-ramo-ativ WHEN AVAIL ramo-ativ
           tt-conteudo.desc-ramo-ativ  = ramo-ativ.descricao WHEN AVAIL ramo-ativ
           tt-conteudo.cod-finalid     = finalidades_venda.cod_finalidade_venda WHEN AVAIL finalidades_venda
           tt-conteudo.desc-finalid    = finalidades_venda.desc_finalidade_venda WHEN AVAIL finalidades_venda.

    IF AVAIL ext-emitente THEN DO.
       ASSIGN tt-conteudo.varejo      = IF ext-emitente.log_varejo THEN "Y" ELSE "N"
              tt-conteudo.atacado     = IF ext-emitente.log_atacado THEN "Y" ELSE "N"
              tt-conteudo.industria   = IF ext-emitente.log_industria THEN "Y" ELSE "N"        
              tt-conteudo.servico     = IF ext-emitente.log_servico THEN "Y" ELSE "N".
    END.

    FOR EACH dbaux.cont-emit OF dbaux.emitente NO-LOCK.
        CASE dbaux.cont-emit.area.
            WHEN "Financeiro" THEN ASSIGN tt-conteudo.email-fin = dbaux.cont-emit.e-mail.
            WHEN "Fiscal" THEN ASSIGN tt-conteudo.email-fis = dbaux.cont-emit.e-mail.
            WHEN "Comercial" THEN ASSIGN tt-conteudo.email-com = dbaux.cont-emit.e-mail.
            WHEN "Outros" THEN ASSIGN tt-conteudo.email-out = dbaux.cont-emit.e-mail.
        END CASE.
    END.
END.

OUTPUT TO p:\clientes.csv.
   PUT "CodCliente;NomeCliente;CodRamoAtiv;DescRamoAtiv;CodFinalidVenda;DescFinalidVenda;Varejo;Atacado;Industria;Servi‡o;Email Financeiro;Email Fiscal;Email Comercial;Email Outros" SKIP.
   FOR EACH tt-conteudo NO-LOCK.
       EXPORT DELIMITER ";" tt-conteudo.
   END.
OUTPUT CLOSE.
