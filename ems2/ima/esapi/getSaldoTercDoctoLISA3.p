/*
programa:esapi/getSaldoTercDoctoLISA3.p
Objetivo: retornar o saldo de terceiros de um docto da LISA, o 
rowid do registro e o nivel de saldo que pode ser seq, nota,produto ou nenhum.
Autor: Tadeu Silva
Data: 10/2024

nivel de saldo
1-sequencia
2-nota remessa
3-produto
4-sem saldo
*/
{esapi/getSaldoTercDoctoLisa3.i}
    
DEFINE INPUT  PARAMETER pNroDocto    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pItCodigo    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pSeq         AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER dQt          AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ttSaldoNivel .



DEFINE VARIABLE cSeriePadrao    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNatOperPadrao  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE codEmitPadrao   AS INTEGER     NO-UNDO.
DEFINE VARIABLE dSaldo          AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dMovto          AS DECIMAL     NO-UNDO.


RUN esapi/getVarsSaldoTercLisa.p(OUTPUT cSeriePadrao,
                                 OUTPUT cNatOperPadrao,
                                 OUTPUT codEmitPadrao).


FOR FIRST saldo-terc  NO-LOCK
    WHERE saldo-terc.cod-emitente   = codEmitPadrao
    AND   saldo-terc.nro-docto      = pNroDocto
    AND   saldo-terc.serie          = cSeriePadrao
    AND   saldo-terc.nat-operacao   = cNatOperPadrao
    AND   saldo-terc.it-codigo      = pItCodigo
    AND   saldo-terc.seq            = pSeq
    //AND   saldo-terc.quantidade     > 0
    :
    IF dQt <= saldo-terc.quantidade THEN DO:
       ASSIGN dSaldo = 0. 
    END.
    ELSE DO:
       ASSIGN dSaldo =   dQt - saldo-terc.quantidade. 
    END.
    
    
    CREATE ttSaldoNivel.
    ASSIGN ttSaldoNivel.nivel       = 1
           ttSaldoNivel.qtLimite    = dQt - dSaldo
           ttSaldoNivel.rRowid      = ROWID(saldo-terc)
           .    
END.


IF NOT AVAIL saldo-terc THEN DO:   
   FOR EACH saldo-terc NO-LOCK
   WHERE saldo-terc.cod-emitente   = codEmitPadrao
   AND   saldo-terc.nro-docto      = pNroDocto
   AND   saldo-terc.serie          = cSeriePadrao
   AND   saldo-terc.nat-operacao   = cNatOperPadrao
   AND   saldo-terc.it-codigo      = pItCodigo
   AND   saldo-terc.quantidade     > 0
   //AND   saldo-terc.seq            = pSeq 
   .
       RUN calcSaldo.
       CREATE ttSaldoNivel.
       ASSIGN ttSaldoNivel.nivel       = 2
              ttSaldoNivel.qtLimite    = dMovto
              ttSaldoNivel.rRowid      = ROWID(saldo-terc)
              .  
       END.        
       IF NOT AVAIL saldo-terc THEN DO:
          FOR EACH saldo-terc NO-LOCK
              WHERE saldo-terc.cod-estabel    = '505'
              AND   saldo-terc.it-codigo      = pItCodigo
              AND   saldo-terc.cod-emitente   = codEmitPadrao
              AND   saldo-terc.quantidade     > 0
              USE-INDEX estab-item .          
              
              RUN calcSaldo.
        
              CREATE ttSaldoNivel.
              ASSIGN ttSaldoNivel.nivel       = 3
                     ttSaldoNivel.qtLimite    = saldo-terc.quantidade
                     ttSaldoNivel.rRowid      = ROWID(saldo-terc)
                     .             
                     
          END.      
          IF NOT AVAIL saldo-terc THEN  DO:
             CREATE ttSaldoNivel.
             ASSIGN ttSaldoNivel.nivel       = 4
                    ttSaldoNivel.qtLimite    = 0
                    ttSaldoNivel.rRowid      = ?
                    .
          END.                             
       END.   
END.


PROCEDURE calcSaldo:

    IF dSaldo = 0 THEN    DO:
          RETURN 'ok'.  
           
    END.
    IF dSaldo <= saldo-terc.quantidade THEN   DO:
        ASSIGN dMovto   = dSaldo
              dSaldo    = 0 .      
    END.
    ELSE DO:
     ASSIGN dMovto = saldo-terc.quantidade
            dSaldo = dSaldo -  saldo-terc.quantidade .
    END.

END PROCEDURE.

