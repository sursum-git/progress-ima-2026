DEFINE TEMP-TABLE tt
    FIELD portador      AS CHAR
    FIELD cart          AS CHAR
    FIELD estab         AS CHAR
    FIELD Esp	        AS CHAR 
    FIELD serie         AS CHAR 
    FIELD titulo        AS CHAR
    FIELD parcela       AS CHAR
    FIELD codEmitente   AS INT
    /*FIELD NomeAbreviado AS CHAR
    FIELD Cidade	    AS CHAR FORMAT 'x(200)' 
    FIELD dtemissao     AS date
    FIELD dtvencto      AS DATE
    FIELD saldo         AS DECIMAL*/ .
 DEFINE VARIABLE  i AS INTEGER     NO-UNDO.
INPUT FROM u:\destinacao001_novo.txt.
REPEAT:
  CREATE tt.
  IMPORT DELIMITER ";" tt .
 
END.                       
INPUT CLOSE.
FOR EACH tt WHERE portador = '':
    DELETE tt.
END.

FOR EACH tt:
    FIND FIRST tit_acr
        WHERE cod_portador    = tt.portador
        AND   cod_estab       = tt.estab
        AND   cod_tit_acr     = tt.titulo
        AND   cod_espec_docto = tt.esp
        AND   cod_ser_docto   = tt.serie 
        AND   cod_cart_bcia   = tt.cart
        AND   parcela         = tt.parcela
        AND   cdn_cliente     = tt.codEmitente EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL tit_acr THEN DO:
        DISP "alterado"
        cod_portador         
        cod_estab       
        cod_tit_acr     
        cod_espec_docto 
        cod_ser_docto   
        cod_cart_bcia   
        parcela         
        cdn_cliente  .

    END.
    ELSE DO:
       DISP "n∆o encontrado"
       tt.portador         
       tt.estab            
       tt.titulo           
       tt.esp              
       tt.serie            
       tt.cart             
       tt.parcela          
       tt.codEmitente.
    END.

END.



/*
  10 cod_empresa                      char        im
   20 cod_estab                        char        im
   30 cod_espec_docto                  char        im
   40 cod_ser_docto                    char        im
   50 cod_tit_acr                      char        im
   60 cod_parcela                      char        im
   70 cdn_cliente                      inte        im
   80 cdn_clien_matriz                 inte        m
   90 cdn_repres                       inte        i
  100 nom_abrev                        char        im
  110 nom_abrev_contat                 char
  120 num_pessoa                       inte        im
  130 num_fatur_acr                    inte
  140 num_id_movto_tit_acr_ult         inte
  150 num_id_tit_acr                   inte        im
  160 num_id_movto_cta_corren          inte        im
  170 num_bord_acr                     inte        im
  180 num_renegoc_cobr_acr             inte        m
  190 ind_orig_tit_acr                 char        m
  200 ind_sit_tit_acr                  char        m
  210 ind_tip_espec_docto              char        im
  220 ind_sit_bcia_tit_acr             char        m
  230 ind_tip_cobr_acr                 char
  240 ind_ender_cobr                   char        m
  250 dat_transacao                    date        im
  260 dat_emis_docto                   date        im
  270 dat_vencto_tit_acr               date        im
  280 dat_desconto                     date
  290 dat_prev_liquidac                date        im
*/

