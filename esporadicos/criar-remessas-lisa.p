{utp/ut-glob.i}
 //RUN btb/btb100aa.w .
DISABLE TRIGGERS FOR LOAD OF nota-fiscal. 
DISABLE TRIGGERS FOR DUMP OF nota-fiscal. 
DISABLE TRIGGERS FOR LOAD OF it-nota-fisc. 
DISABLE TRIGGERS FOR DUMP OF it-nota-fisc. 
DEFINE TEMP-TABLE ttItemRef
    FIELD itCodigo  AS CHAR
    FIELD ref       AS CHAR
    FIELD qt        AS DECIMAL
    .
DEFINE VARIABLE i-nr-seq        AS INTEGER     NO-UNDO.
DEF TEMP-TABLE tt-nota-fisc    LIKE nota-fiscal.
DEF TEMP-TABLE tt-it-nota-fisc LIKE it-nota-fisc.
DEFINE VARIABLE h-acomp        AS HANDLE      NO-UNDO.

DEFINE TEMP-TABLE ttPreco NO-UNDO
    FIELD itCodigo LIKE ITEM.it-codigo
    FIELD qtTotal  AS DECIMAL
    FIELD vlTotal  AS DECIMAL
    FIELD vlMedia  AS DECIMAL
    INDEX ind-primario IS PRIMARY itCodigo
    .

DEF TEMP-TABLE tt-notas-geradas NO-UNDO
    FIELD rw-nota-fiscal AS   ROWID
    FIELD nr-nota        LIKE nota-fiscal.nr-nota-fis
    FIELD seq-wt-docto   LIKE wt-docto.seq-wt-docto .

INPUT FROM value('P:\inventario 2025\remessa\parte_20.csv').   
REPEAT:   
    
    CREATE ttItemRef.
    IMPORT DELIMITER ";" ttItemRef.
    ASSIGN ttItemRef.ref = IF LENGTH(ttItemRef.ref) < 3 THEN  FILL('0',3 - LENGTH(ttItemRef.ref)) +   ttItemRef.ref ELSE ttItemRef.ref .
 
END.
INPUT CLOSE.	

INPUT FROM value('p:\inventario 2025\Preco-para-nota-remessa.csv').   

REPEAT:
    
    CREATE ttPreco.
    IMPORT DELIMITER ";" ttPreco.

END.

INPUT CLOSE.


RUN pi-gerar-nota-envio.
IF VALID-HANDLE(h-acomp) THEN
DO:
    RUN pi-finalizar IN h-acomp.     
END.
  
PROCEDURE pi-gerar-nota-envio.
    
    DEF VAR c-natur-oper AS CHAR.
    DEF VAR i-nr-seq AS INT.
    
    ASSIGN c-natur-oper = '59207i'.

    FIND usuar-depos
    WHERE usuar-depos.cod-estab   = '505' 
         AND  usuar-depos.cod-usuario = c-seg-usuario  NO-LOCK NO-ERROR.
    IF NOT AVAIL usuar-depos THEN DO.
        MESSAGE 'Usu†rio ' + c-seg-usuario + ' n∆o est† relacionado ao Dep¢sito do Container' SKIP
                'Ou existe em mais de um deposito no mesmo Estabelecimento' 
                'Utilize cd1760' 
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN 'ADM-ERROR'.
    END.

    FIND deposito WHERE
         deposito.cod-depos = usuar-depos.cod-depos NO-LOCK NO-ERROR.

    FIND emitente WHERE
         emitente.nome-abrev = deposito.nome-abrev NO-LOCK NO-ERROR.

    CREATE tt-nota-fisc.
    ASSIGN tt-nota-fisc.cod-estabel      = '505'
           tt-nota-fisc.serie            ='2'
           tt-nota-fisc.nome-ab-cli      = emitente.nome-abrev
           tt-nota-fisc.nat-oper         = c-natur-oper 
           tt-nota-fisc.dt-emis-nota     = TODAY
           tt-nota-fisc.nro-proc-entrada = 0
           . 

    ASSIGN i-nr-seq = 0.
    

    RUN utp/ut-acomp.p PERSIST SET h-acomp.
    RUN pi-inicializar IN h-acomp('Gerando NF Remessa').
    
    FOR EACH ttItemRef WHERE ttItemRef.itCodigo <> '' AND ttItemRef.itCodigo <> '0' AND ttItemRef.qt > 0:
        FIND ITEM WHERE
             ITEM.it-codigo = ttItemRef.itCodigo NO-LOCK NO-ERROR.
    
        ASSIGN i-nr-seq = i-nr-seq + 10.
        
        RUN pi-acompanhar IN h-acomp('item:' + ttItemRef.itCodigo + '|ref:' + ttItemRef.ref + '|qt:' + string(ttItemRef.qt) ).
        FIND ttPreco
            WHERE ttpreco.itCodigo = ttItemRef.itCodigo  NO-ERROR.
        IF NOT AVAIL ttPreco THEN  DO:
           MESSAGE 'Preáo n∆o encontrado para o item:' SKIP
                   ttItemRef.itCodigo
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
           RETURN 'nok'.    
            
        END.
            
        CREATE tt-it-nota-fisc.
        ASSIGN tt-it-nota-fisc.nr-seq-fat       = i-nr-seq
               tt-it-nota-fisc.cod-estabel      = tt-nota-fisc.cod-estabel
               tt-it-nota-fisc.serie            = tt-nota-fisc.serie
               tt-it-nota-fisc.nat-oper         = tt-nota-fisc.nat-oper
               tt-it-nota-fisc.dt-emis-nota     = tt-nota-fisc.dt-emis-nota
               tt-it-nota-fisc.it-codigo        = ttItemRef.itCodigo
               tt-it-nota-fisc.cod-refer        = ttItemRef.ref
               tt-it-nota-fisc.cod-depos        = 'ARM'
               tt-it-nota-fisc.un-fatur[1]      = item.un
               tt-it-nota-fisc.un-fatur[2]      = item.un
               tt-it-nota-fisc.qt-faturada[1]   = ttItemRef.qt              
               tt-it-nota-fisc.vl-preuni        = round(ttPreco.vlMedia,5) 
               tt-it-nota-fisc.vl-preori        = round(ttPreco.vlMedia,5) 
               tt-it-nota-fisc.vl-tot-item      = round(ttPreco.vlMedia,5) *  tt-it-nota-fisc.qt-faturada[1]   
               .
        FIND FIRST saldo-estoq NO-LOCK
             WHERE saldo-estoq.it-codigo    = tt-it-nota-fisc.it-codigo
             AND   saldo-estoq.cod-refer    = tt-it-nota-fisc.cod-refer
             AND   saldo-estoq.cod-estabel  = tt-it-nota-fisc.cod-estabel
             AND   saldo-estoq.cod-depos    = tt-it-nota-fisc.cod-depos
             AND   saldo-estoq.qtidade-atu >=  tt-it-nota-fisc.qt-faturada[1] NO-ERROR.
        IF NOT AVAIL saldo-estoq THEN DO:
           MESSAGE "item:"  tt-it-nota-fisc.it-codigo "-ref:" tt-it-nota-fisc.cod-refer  SKIP
                   " com saldo menor que o necess†rio:" SKIP
                    tt-it-nota-fisc.qt-faturada[1]
               VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
           RETURN 'nok'.
            
        END.
    
    
    
    END.
    RUN pi-acompanhar IN h-acomp('gerando nota fiscal' ). 
    RUN esapi/cria-nota-ft4003.p (INPUT TABLE tt-nota-fisc,
                                  INPUT TABLE tt-it-nota-fisc,
                                  OUTPUT TABLE tt-notas-geradas).

    FIND FIRST tt-notas-geradas NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-notas-geradas THEN DO:
       MESSAGE 'n∆o foram geradas notas'
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
       RETURN 'ADM-ERROR'.
    END.
    ELSE DO:   
        FOR FIRST tt-notas-geradas:
            MESSAGE 'NF:' tt-notas-geradas.nr-nota
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            
        END.
    END.

   /* FIND nota-fiscal WHERE
         ROWID(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal SHARE-LOCK NO-ERROR.
    IF nota-fiscal.nro-proc-entrada = 0 THEN 
       ASSIGN nota-fiscal.nro-proc-entrada = pp-container.nr-container.*/
       

       
       
    RETURN 'ADM-OK'.
END PROCEDURE.
