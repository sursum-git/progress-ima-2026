def var c-base  as character no-undo. /* base tst ou bkt */
def var c-dbase as character no-undo. /* Descricao da base */
def var c-email as character NO-UNDO.

disable triggers for load of emsfnd.usuar_mestre.

/*output to "/log/prgconnect.log" append.*/

assign c-base  = 'tst'.

assign c-email = "cpd@imatextil.com.br".
       c-dbase = "(BASE TESTE)".

if c-base = "bkt" then assign c-dbase = replace(c-dbase,"TESTE","BKP-TESTE").

find first emsfnd.usuar_mestre share-lock where
           emsfnd.usuar_mestre.cod_usuario = "super".

ASSIGN emsfnd.usuar_mestre.cod_senha            = BASE64-ENCODE(SHA1-DIGEST("teste"))
       emsfnd.usuar_mestre.cod_senha_framework  = BASE64-ENCODE(SHA1-DIGEST("teste"))
       emsfnd.usuar_mestre.dat_inic_valid       = 01/01/2001
       emsfnd.usuar_mestre.dat_fim_valid        = 12/31/9999
       emsfnd.usuar_mestre.dat_valid_senha      = 12/31/9999
       emsfnd.usuar_mestre.num_dias_valid_senha = 999.

for each emsfnd.fnd_empres share-lock:
    assign emsfnd.fnd_empres.des_razao_social =  c-dbase + " " + trim(emsfnd.fnd_empres.des_razao_social) + " [" + string(today,"99/99/99") + " as " + string(time,"hh:mm") + "]".
end.

for each emsfnd.bco_empres share-lock
      by emsfnd.bco_empres.cod_empresa
	  by emsfnd.bco_empres.cod_param_conex:
  assign emsfnd.bco_empres.cod_param_conex = replace(emsfnd.bco_empres.cod_param_conex,"-S 10","-S 20")
         emsfnd.bco_empres.cod_param_conex = replace(emsfnd.bco_empres.cod_param_conex,"-S 30","-S 40")
         emsfnd.bco_empres.cod_param_conex = replace(emsfnd.bco_empres.cod_param_conex,"192.168.0.44","10.1.1.44").
		 
   /*
    put substring(string(now),01,10)      format "x(10)" " as "
        substring(string(now),12,10)      format "x(08)" " "
		emsfnd.bco_empres.cod_empresa     format "x(01)" " "
		emsfnd.bco_empres.cod_bco_fisic   format "x(10)" " "
	    emsfnd.bco_empres.cod_param_conex format "x(31)" " "
		c-base							  format "x(03)" " "
		c-dbase							  format "x(16)" " "
		c-email							  format "x(20)" " "
        skip.
        */
end.


IF CONNECTED("espec") THEN DO:

   FIND FIRST espec.im-param WHERE
              espec.im-param.cod-param = "BASE".

   ASSIGN espec.im-param.val-param = "TESTE".

   for each ems2ima.estabelec share-lock where
            ems2ima.estabelec.cod-estabel = "1" or
            ems2ima.estabelec.cod-estabel = "5".

   /* gt902 */     
       find first espec.gati-nfe-param where
                  espec.gati-nfe-param.cod-estabel = ems2ima.estabelec.cod-estabel
                  no-error.

       assign espec.gati-nfe-param.email-xml     = replace(espec.gati-nfe-param.email-xml    ,"PROD","HOMO")
              espec.gati-nfe-param.end-exp-can   = replace(espec.gati-nfe-param.end-exp-can  ,"PROD","HOMO")
              espec.gati-nfe-param.end-exp-inu   = replace(espec.gati-nfe-param.end-exp-inu  ,"PROD","HOMO")
              espec.gati-nfe-param.end-exp-nfe   = replace(espec.gati-nfe-param.end-exp-nfe  ,"PROD","HOMO")
              espec.gati-nfe-param.end-imp-can   = replace(espec.gati-nfe-param.end-imp-can  ,"PROD","HOMO")
              espec.gati-nfe-param.end-imp-danfe = replace(espec.gati-nfe-param.end-imp-danfe,"PROD","HOMO")
              espec.gati-nfe-param.end-imp-inu   = replace(espec.gati-nfe-param.end-imp-inu  ,"PROD","HOMO")
              espec.gati-nfe-param.end-imp-nfe   = replace(espec.gati-nfe-param.end-imp-nfe  ,"PROD","HOMO")
              espec.gati-nfe-param.end-imp-txt   = replace(espec.gati-nfe-param.end-imp-txt  ,"PROD","HOMO")
              espec.gati-nfe-param.end-imp-xml   = replace(espec.gati-nfe-param.end-imp-xml  ,"PROD","HOMO")
              espec.gati-nfe-param.exp-ins-cce   = replace(espec.gati-nfe-param.exp-ins-cce  ,"PROD","HOMO")
              espec.gati-nfe-param.exp-ins-mde   = replace(espec.gati-nfe-param.exp-ins-mde  ,"PROD","HOMO")
              espec.gati-nfe-param.imp-ins-cce   = replace(espec.gati-nfe-param.imp-ins-cce  ,"PROD","HOMO")
              espec.gati-nfe-param.imp-ins-mde   = replace(espec.gati-nfe-param.imp-ins-mde  ,"PROD","HOMO")
              espec.gati-nfe-param.txt-ins-cce   = replace(espec.gati-nfe-param.txt-ins-cce  ,"PROD","HOMO")
              espec.gati-nfe-param.txt-ins-mde   = replace(espec.gati-nfe-param.txt-ins-mde  ,"PROD","HOMO").
    end.
end.

/*
run prgprocessa-ima (input c-base,
                     input c-dbase,
		             input c-email).

run prgprocessa-med (input c-base,
                     input c-dbase,
		             input c-email).

output close.
*/
