//estab.;dt.vencto;fornec;esp;serie;titulo;parc.;vl.original;vl.saldo;vl.novo saldo
DEFINE TEMP-TABLE tt
    FIELD codEstab AS CHAR
    FIELD dtVencto AS DATE
    FIELD cdnFornec AS INT
    FIELD codEsp    AS CHAR
    FIELD codSerie  AS CHAR
    FIELD codTitAp  AS CHAR FORMAT 'x(12)'
    FIELD codParcela AS CHAR
    FIELD vlOriginal AS DEC
    FIELD vlSaldo    AS DEC
    FIELD vlNovoSaldo AS DEC
    FIELD logAchou   AS LOGICAL INIT FALSE .

INPUT FROM p:\tadeu\titulos_zerar2.csv.
REPEAT:
    CREATE tt.
    IMPORT DELIMITER ";" tt.
END.

INPUT CLOSE.


FOR EACH tt 
    WHERE tt.codTitAp <> '':
    FIND FIRST tit_ap
        WHERE tit_ap.cod_estab = tt.CodEstab
        AND   tit_ap.dat_vencto_tit_ap = tt.dtVencto
        AND   tit_ap.cdn_fornecedor    = tt.cdnFornec
        AND   tit_ap.cod_espec_docto   = tt.codEsp
        AND   tit_ap.cod_ser_docto     = tt.codSerie
        AND   tit_ap.cod_tit_ap        = tt.codTitAp
        AND   tit_ap.cod_parcela       = tt.codParcela
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL tit_ap THEN DO:
       ASSIGN tt.logAchou = TRUE.
       ASSIGN tit_ap.val_sdo_tit_ap = tt.vlNovoSaldo
              tit_ap.cod_livre_1 = 'titulo zerado a pedido do setor de contas a pagar. Saldo antes de zerar o titulo:' + STRING(tt.VlSaldo,">>>,>>>,>>9.99")
              tit_ap.LOG_sdo_tit_Ap = NO.
       FIND FIRST val_tit_ap OF tit_ap EXCLUSIVE-LOCK NO-ERROR.
       IF AVAIL val_tit_ap THEN
          ASSIGN val_tit_ap.val_sdo_tit_ap = tt.vlNovoSaldo.

    END.
    DISP tt WITH  WIDTH 550.
    DISP tit_ap.cod_livre_1.
END.
