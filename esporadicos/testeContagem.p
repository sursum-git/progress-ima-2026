DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
DEFINE VARIABLE dtini AS DATETIME    NO-UNDO.
DEFINE VARIABLE dtfim AS DATETIME    NO-UNDO.
ASSIGN dtini = NOW.
    /*FOR EACH nota-fiscal FIELDS() NO-LOCK
        WHERE nota-fiscal.dt-emis-nota >= 01.01.2020
        :
        ASSIGN iCont = iCont + 1.
    
    END.*/
  /*FOR EACH docum-est FIELDS() NO-LOCK
      //  WHERE dt-trans >= 01.01.2020 
      :
        ASSIGN iCont = iCont + 1.
    
   END.*/

/*FOR EACH ems5.cliente FIELDS() NO-LOCK
    //WHERE cliente.dat_impl_clien > 01.01.2020
    :
    ASSIGN iCont = iCont + 1.
END.*/

 /* FOR EACH tit_ap FIELDS()
        
        :
        ASSIGN iCont = iCont + 1.
    
    END.*/

  /*FOR EACH ob-etiqueta FIELDS()
        //WHERE nota-fiscal.dt-emis-nota >= 01.01.2020
        NO-LOCK :
        ASSIGN iCont = iCont + 1.
    
    END.*/


ASSIGN dtfim = NOW.
DISP INTERVAL(dtfim, dtini,'seconds').
DISP iCont.
