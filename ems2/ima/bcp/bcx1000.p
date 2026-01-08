/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
/********************************************************************************************
**   Programa..: bcx1000.p                                                                  **
**                                                                                          **
**   Objetivo..: CONSISTENCIAS DO EMBARQUES                                                 **
**                                                                                          **
**                                                                                          **
********************************************************************************************/

{bcp/bcx1000.i}

def var de-qtd as decimal decimals 10 no-undo.    

PROCEDURE PI-BUSCA-EMBARQUE:
    DEF INPUT  PARAM I-EMBARQUE AS INT NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR TT-EMBARQUE.
    
    FIND FIRST embarque 
         WHERE embarque.cdd-embarq = i-embarque
         NO-LOCK NO-ERROR.
    
    IF  AVAIL embarque 
    THEN DO:
        FOR EACH tt-embarque:
            DELETE tt-embarque.
        END.
        CREATE TT-EMBARQUE.
        BUFFER-COPY EMBARQUE TO TT-EMBARQUE.
        RETURN "OK".
    END.
    ELSE 
        RETURN "NOK".

END PROCEDURE.


PROCEDURE PI-BUSCA-RESUMO:
    DEF INPUT PARAM I-EMBARQUE AS INT NO-UNDO.
    DEF INPUT PARAM I-RESUMO   AS INT NO-UNDO.
        
    FIND FIRST res-cli 
         WHERE res-cli.cdd-embarq = i-embarque
         AND   res-cli.nr-resumo  = i-resumo
         NO-LOCK NO-ERROR.
    
    IF NOT AVAIL res-cli THEN DO:
    
        RETURN "NOK".
    
    END.
    ELSE DO:
    
        RETURN "OK".    
    
    END.
                   
END PROCEDURE.

PROCEDURE PI-ITEM-ATENDIDO:
    DEF INPUT PARAM i-embarque    AS INT  NO-UNDO.
    DEF INPUT PARAM i-resumo-ini  AS INT  NO-UNDO.
    DEF INPUT PARAM i-resumo-fim  AS INT  NO-UNDO.
    DEF INPUT PARAM c-item-ini    AS CHAR NO-UNDO.
    DEF INPUT PARAM c-item-fim    AS CHAR NO-UNDO.
    DEF INPUT PARAM c-refer-ini   AS CHAR NO-UNDO.
    DEF INPUT PARAM c-refer-fim   AS CHAR NO-UNDO.
    DEF OUTPUT PARAM C-ITEM-FALTA AS CHAR NO-UNDO.
    DEF OUTPUT PARAM DE-QT-TOTAL       AS DEC  DECIMALS 4  NO-UNDO.
    DEF OUTPUT PARAM DE-QT-lido        AS DEC  DECIMALS 4  NO-UNDO.
    
    DEF VAR de-count     AS DECIMAL NO-UNDO.
    DEF VAR de-item-lido AS DECIMAL NO-UNDO.
    DEF VAR de-item-alocado AS DECIMAL NO-UNDO.
    DEF VAR l-deleta     AS LOGICAL NO-UNDO.
    DEF VAR i-registro   AS INT     NO-UNDO.
    
    IF  i-resumo-ini = 0
    AND i-resumo-fim = 0 
    THEN
        ASSIGN i-resumo-ini = 0 
               i-resumo-fim = 999999.
    
    ASSIGN  DE-QT-TOTAL = 0
            DE-QT-lido  = 0.
    
    FOR EACH  it-pre-fat NO-LOCK
        WHERE it-pre-fat.cdd-embarq  = i-embarque
        AND   it-pre-fat.nr-resumo  >= i-resumo-ini
        AND   it-pre-fat.nr-resumo  <= i-resumo-fim
        AND   it-pre-fat.it-codigo  >= c-item-ini
        AND   it-pre-fat.it-codigo  <= c-item-fim
        AND   it-pre-fat.cod-refer  >= c-refer-ini
        AND   it-pre-fat.cod-refer  <= c-refer-fim
        BREAK BY it-pre-fat.it-codigo
              BY it-pre-fat.cod-refer:        
        
        ASSIGN de-item-lido = 0.
    
        for each  BC-ETIQUETA 
            WHERE BC-ETIQUETA.cdd-embarq   = IT-PRE-FAT.CDD-EMBARQ
            AND   BC-ETIQUETA.NR-RESUMO    = IT-PRE-FAT.NR-RESUMO
            and   BC-ETIQUETA.it-codigo    = IT-PRE-FAT.IT-CODIGO
            AND   BC-ETIQUETA.NR-SEQ-fat   = IT-PRE-FAT.NR-SEQUENCIA
            AND   BC-ETIQUETA.NOME-ABREV   = IT-PRE-FAT.NOME-ABREV
            AND   BC-ETIQUETA.NR-PEDCLI    = IT-PRE-FAT.NR-PEDCLI
            AND   BC-ETIQUETA.REFERENCIA   = it-pre-fat.cod-refer
            AND   BC-ETIQUETA.NR-ENTREGA   = IT-PRE-FAT.NR-ENTREGA
            NO-LOCK:
            
            ASSIGN de-item-lido = de-item-lido + bc-etiqueta.qt-item.
    
        END.
        
        ASSIGN DE-QT-TOTAL  = DE-QT-TOTAL + it-pre-fat.qt-alocada
               DE-QT-lido   = DE-QT-lido  + de-item-lido.
    
        IF  de-item-lido < it-pre-fat.qt-alocada THEN DO:   
            ASSIGN C-ITEM-FALTA = it-pre-fat.it-codigo.
            RETURN 'NOK'.
        END.
    END.          
    
    RETURN "OK".

END PROCEDURE.

PROCEDURE PI-BUSCA-ETIQUETA:

    DEF INPUT  PARAM c-id-etq AS CHAR NO-UNDO.
    DEF OUTPUT PARAM l-etiqueta AS LOG NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR tt-campo.
    
    FOR EACH tt-campo:
        DELETE tt-campo.
    END.
    
    IF  LENGTH(C-ID-ETQ) >= 11 
    THEN 
        ASSIGN l-etiqueta = YES
               c-id-etq   = trim(c-id-etq).    
    ELSE
        RETURN "NOK".
        
    FIND FIRST bc-etiqueta
         WHERE bc-etiqueta.progressivo  = c-id-etq
         NO-LOCK NO-ERROR.
    
    IF  AVAIL bc-etiqueta THEN DO:
       
        CREATE tt-campo.
        ASSIGN tt-campo.campo    = "NR-EMBARQ"
               tt-campo.INTEIRO  = bc-etiqueta.cdd-embarq.
    
        CREATE tt-campo.
        ASSIGN tt-campo.campo    = "NR-RESUMO"
               tt-campo.INTEIRO = bc-etiqueta.NR-RESUMO.
    
        CREATE tt-campo.
        ASSIGN tt-campo.campo    = "CHAVE-UNICA"
               tt-campo.caracter = bc-etiqueta.progressivo.
                                   
        CREATE tt-campo.
        ASSIGN tt-campo.campo    = "REFERENCIA"
               tt-campo.caracter = bc-etiqueta.referencia.
        
        CREATE tt-campo.
        ASSIGN tt-campo.campo    = "IT-CODIGO"
               tt-campo.caracter = bc-etiqueta.it-codigo.
    
        CREATE tt-campo.
        ASSIGN tt-campo.campo    = "LOTE"
               tt-campo.caracter = bc-etiqueta.lote.
    
        CREATE tt-campo.
        ASSIGN tt-campo.campo    = "QTD-PECA"
               tt-campo.DECIMAL  = bc-etiqueta.qt-item.
    
        CREATE tt-campo.
        ASSIGN tt-campo.campo    = "COD-ESTADO"
               tt-campo.INTEIRO  = bc-etiqueta.cod-estado.
                                   
    
        FIND BC-ESTADO-ETIQ
             WHERE BC-ESTADO-ETIQ.COD-ESTADO = BC-ETIQUETA.COD-ESTADO
             NO-LOCK NO-ERROR.
    
        CREATE tt-campo.
        ASSIGN tt-campo.campo    = "DESC-ESTADO"
               tt-campo.CARACTER = IF AVAIL BC-ESTADO-ETIQ THEN bc-ESTADO-etiq.DESCRICAO ELSE STRING(BC-ETIQUETA.COD-ESTADO).
    
        CREATE tt-campo.
        ASSIGN tt-campo.campo     = "COD-ESTABEL"
               tt-campo.caracter  = bc-etiqueta.COD-ESTABEL.
         
        RETURN "OK".
    END.
    ELSE 
        RETURN "NOK".
    

END PROCEDURE.

PROCEDURE busca-item:
    DEF INPUT  PARAM c-item               AS CHAR NO-UNDO.
    DEF OUTPUT PARAM c-desc-item          AS CHAR NO-UNDO.
    DEF OUTPUT PARAM c-un                 AS CHAR NO-UNDO.
    
    
    FIND ITEM 
         WHERE ITEM.it-codigo = c-item
         NO-LOCK NO-ERROR.
        
    IF  AVAIL ITEM THEN DO:
        
        ASSIGN c-desc-item = ITEM.descricao-1
               c-un        = ITEM.un.
        
        RETURN "OK".
    
    END.
    ELSE 
        RETURN "NOK".
   
END PROCEDURE.   

PROCEDURE PI-TRANSPOTE:
    DEF INPUT-OUTPUT PARAM I-CODIGO AS INT NO-UNDO.
    DEF INPUT-OUTPUT PARAM C-NOME  AS CHAR NO-UNDO.
    
    
    IF i-codigo <> 0 THEN DO:
        
        FIND TRANSPORTE 
             WHERE TRANSPORTE.COD-TRANSP = I-CODIGO
             NO-LOCK NO-ERROR.
    END.
    ELSE 
        FIND TRANSPORTE 
             WHERE TRANSPORTE.nome-abrev = c-nome
             NO-LOCK NO-ERROR.
    
    IF AVAIL TRANSPORTE THEN DO:
    
       ASSIGN C-NOME = TRANSPORTE.NOME-ABREV
              i-codigo = transporte.cod-transp.
       RETURN "OK".
        
    END.
    ELSE 
        RETURN "NOK".

END PROCEDURE.

PROCEDURE PI-BUSCA-IT-PRE-FAT:
    
    DEF INPUT PARAM I-EMBARQUE   AS INT NO-UNDO.
    DEF INPUT PARAM I-RESUMO-INI AS INT NO-UNDO.
    DEF INPUT PARAM I-RESUMO-FIM AS INT NO-UNDO.
    DEF INPUT PARAM C-ITEM       AS CHAR NO-UNDO.
    DEF INPUT PARAM C-REF        AS CHAR NO-UNDO.
    DEF INPUT PARAM c-lote       AS CHAR NO-UNDO.
    DEF INPUT PARAM c-local      AS CHAR NO-UNDO.
    DEF OUTPUT PARAM TABLE FOR tt-it-pre-fat.
    
    DEF VAR de-item-alocado AS DEC DECIMALS 10 NO-UNDO.
    DEF VAR de-saldo        AS DEC DECIMALS 10 NO-UNDO.
    
    DEF VAR de-qt-separada  AS DEC DECIMALS 10 NO-UNDO.
    
    FOR EACH TT-IT-PRE-FAT:
        DELETE TT-IT-PRE-FAT.
    END.        
    
    IF  i-resumo-ini = 0
    AND i-resumo-fim = 0 
    THEN
        ASSIGN i-resumo-ini = 0 
               i-resumo-fim = 999999.
    
    for each  it-pre-fat 
        WHERE it-pre-fat.cdd-embarq  = i-embarque 
        AND   it-pre-fat.nr-resumo  >= i-resumo-ini
        AND   it-pre-fat.nr-resumo  <= i-resumo-fim
        AND   it-pre-fat.it-codigo   = c-item
        AND   it-pre-fat.cod-refer   = c-ref
        no-lock
        BREAK BY IT-PRE-FAT.NR-RESUMO
              BY IT-PRE-FAT.NR-SEQUENCIA:
       
      
        /* BUSCA QUANTIDADE ATENDIDA DA SEQUENCIA */
    
        ASSIGN de-qt-separada = 0.
               
        for each  BC-ETIQUETA 
            WHERE BC-ETIQUETA.cdd-embarq   = IT-PRE-FAT.CDD-EMBARQ
            AND   BC-ETIQUETA.NR-RESUMO    = IT-PRE-FAT.NR-RESUMO
            and   BC-ETIQUETA.it-codigo    = IT-PRE-FAT.IT-CODIGO
            AND   BC-ETIQUETA.NR-SEQ-fat   = IT-PRE-FAT.NR-SEQUENCIA
            AND   BC-ETIQUETA.NOME-ABREV   = IT-PRE-FAT.NOME-ABREV
            AND   BC-ETIQUETA.NR-PEDCLI    = IT-PRE-FAT.NR-PEDCLI
            AND   BC-ETIQUETA.REFERENCIA   = it-pre-fat.cod-refer
            AND   BC-ETIQUETA.NR-ENTREGA   = IT-PRE-FAT.NR-ENTREGA
            NO-LOCK:
           
            ASSIGN de-qt-separada = de-qt-separada + BC-ETIQUETA.qt-item.
            
        end.         
      
        IF  de-qt-separada < it-pre-fat.qt-alocada
        THEN DO:
      
            CREATE tt-it-pre-fat.
            BUFFER-COPY it-pre-fat TO tt-it-pre-fat.
                                     
            LEAVE.
    
        END.
    
    end.
    
    FIND FIRST tt-it-pre-fat
         NO-LOCK NO-ERROR.
    
    IF NOT AVAIL tt-it-pre-fat
    AND AVAIL it-pre-fat
    THEN DO:
    
        CREATE tt-it-pre-fat.
        BUFFER-COPY it-pre-fat TO tt-it-pre-fat.
        
    END.
    
    
END PROCEDURE.

PROCEDURE RETORNA-QT-EMBARCADA:
    
    DEF INPUT  PARAM R-ROWID     AS ROWID               NO-UNDO.
    DEF OUTPUT PARAM DE-ALOCADA  AS DECIMAL DECIMALS 10 NO-UNDO.
    
    
    FIND IT-PRE-FAT
         WHERE ROWID(IT-PRE-FAT) = R-ROWID 
         NO-LOCK NO-ERROR.
    
    IF  AVAIL IT-PRE-FAT THEN DO:
    
        /* verifica quantidade  total j  embarcada */
        
        for each  BC-ETIQUETA 
            WHERE BC-ETIQUETA.cdd-embarq   = IT-PRE-FAT.CDD-EMBARQ
            AND   BC-ETIQUETA.NR-RESUMO    = IT-PRE-FAT.NR-RESUMO
            and   BC-ETIQUETA.it-codigo    = IT-PRE-FAT.IT-CODIGO
            AND   BC-ETIQUETA.NR-SEQ-FAT   = IT-PRE-FAT.NR-SEQUENCIA
            AND   BC-ETIQUETA.NOME-ABREV   = IT-PRE-FAT.NOME-ABREV
            AND   BC-ETIQUETA.NR-PEDCLI    = IT-PRE-FAT.NR-PEDCLI
            AND   BC-ETIQUETA.REFERENCIA   = it-pre-fat.cod-refer
            AND   BC-ETIQUETA.NR-ENTREGA   = IT-PRE-FAT.NR-ENTREGA
            NO-LOCK:
    
            ASSIGN DE-ALOCADA = DE-ALOCADA +  bc-etiqueta.qt-item.
    
        END.
    
    END.
    
END PROCEDURE.
    

PROCEDURE PI-EMBARQUE-ABERTO:
    DEF INPUT PARAM i-cdd-embarq AS INT NO-UNDO.
    DEF INPUT PARAM i-resumo   AS INT NO-UNDO.
    
    
    FOR FIRST bc-etiqueta
        WHERE bc-etiqueta.nr-embarque = 0
          AND bc-etiqueta.cdd-embarq = i-cdd-embarq
          AND bc-etiqueta.nr-resumo  = i-resumo
        NO-LOCK:
    END.
    
    IF AVAIL bc-etiqueta
    AND bc-etiqueta.cod-estado = 4 
    THEN
        RETURN "NOK".
    ELSE 
        RETURN "OK".


END PROCEDURE.


PROCEDURE pi-zera-return:
   RETURN.
END PROCEDURE.

/******************************************/              


