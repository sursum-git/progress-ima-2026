{utp/ut-glob.i}
DEFINE TEMP-TABLE ttDados
    FIELD itCodigo   AS CHAR
    FIELD codRef     AS CHAR.


//OUTPUT TO c:\temp\precos.txt.
//PUT "Item | Ref | Saldo | Tp Pre‡o | Vencido? " SKIP.
FOR EACH controle_preco 
    WHERE num_nivel = 2 AND tp_preco = 3 AND LOG_vencido = NO.

    FIND FIRST saldo-estoq WHERE
        saldo-estoq.cod-estabel = '5'
        AND  saldo-estoq.it-codigo = controle_preco.it_codigo 
        AND saldo-estoq.cod-refer = controle_preco.cod_refer 
        AND saldo-estoq.lote = saldo-estoq.cod-refer
        AND saldo-estoq.qtidade-atu <= 0
        AND saldo-estoq.cod-depos = 'arm' NO-LOCK NO-ERROR.


    FIND ttDados WHERE
         ttDados.itCodigo = controle_preco.it_codigo AND
         ttDados.codRef = controle_preco.cod_refer NO-ERROR.
         
    IF NOT AVAIL ttDados THEN DO.
       CREATE ttDados.
       ASSIGN ttDados.itCodigo = controle_preco.it_codigo.
       ASSIGN ttDados.codRef = controle_preco.cod_refer.
    END.

    IF AVAIL saldo-estoq THEN DO:
      /* ASSIGN controle_preco.LOG_vencido = YES
              controle_preco.dt_hr_alt = NOW
              controle_preco.cod_usuario_alt = c-seg-usuario .*/
        
   /* DISP 
             ttDados.itCodigo
             ttDados.codRef
             saldo-estoq.it-codigo
             saldo-estoq.cod-refer
             saldo-estoq.qtidade-atu
             controle_preco.tp_preco
             controle_preco.log_vencido
             .*/

    END.
         
END.



DEFINE var lTemEstoq as logical.
FOR EACH controle_preco WHERE num_nivel = 1 AND tp_preco = 3 AND LOG_vencido = NO.
    
	
    ASSIGN lTemEstoq = false.
    FOR EACH saldo-estoq WHERE
        saldo-estoq.cod-estabel = '5'
        AND saldo-estoq.it-codigo = controle_preco.it_codigo
        AND saldo-estoq.lote = saldo-estoq.cod-refer
        AND saldo-estoq.qtidade-atu > 0
        AND saldo-estoq.cod-depos = 'arm' NO-LOCK.
		Find ttDados 
		WHERE ttDados.itCodigo = controle_preco.it_codigo 
		AND   ttDados.codRef   = saldo-estoq.cod-refer NO-LOCK NO-ERROR.	
	    if AVAIL ttDados then next.
		assign lTemEstoq = true.
		leave.     

    END.
	if lTemEstoq = false then do:
	//vencer o preco
        DISP 
             
             controle_preco.it_codigo
             controle_preco.cod_refer
             
             controle_preco.tp_preco
             controle_preco.log_vencido
             WITH 1 COL WIDTH 600.
	end.

END.
/*FOR EACH controle_preco WHERE num_nivel = 1 AND tp_preco = 3 AND LOG_vencido = NO.
    
    
    FOR EACH saldo-estoq WHERE
        saldo-estoq.cod-estabel = '5'
        AND saldo-estoq.it-codigo = controle_preco.it_codigo
        AND saldo-estoq.lote = saldo-estoq.cod-refer
        AND saldo-estoq.qtidade-atu <= 0
        AND saldo-estoq.cod-depos = 'arm' NO-LOCK.


        FOR EACH ttDados 
            WHERE ttDados.itCodigo = controle_preco.it_codigo AND 
                  ttDados.itCodigo = saldo-estoq.it-codigo AND 
                  ttDados.codRef   <> saldo-estoq.cod-refer NO-LOCK.

       IF AVAIL ttDados THEN DO:
        DISP 
             ttDados.itCodigo
             ttDados.codRef
             saldo-estoq.it-codigo
             saldo-estoq.cod-refer
             saldo-estoq.qtidade-atu
             controle_preco.tp_preco
             controle_preco.log_vencido
             WITH 1 COL WIDTH 600.

          END.

        END.

    END.

END.*/



