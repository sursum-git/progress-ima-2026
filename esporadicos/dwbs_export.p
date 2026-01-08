OUTPUT TO c:\temp\dwb_rpt_param.csv.
FOR EACH dwb_rpt_param :

    EXPORT DELIMITER ";" dwb_rpt_param.
    
END.

OUTPUT CLOSE.

OUTPUT TO c:\temp\dwb_rpt_select.csv.
FOR EACH dwb_rpt_select :

    EXPORT DELIMITER ";" dwb_rpt_select.
    
END.

OUTPUT CLOSE.


OUTPUT TO c:\temp\dwb_set_list.csv.
FOR EACH dwb_set_list :

    EXPORT DELIMITER ";" dwb_set_list.
    
END.

OUTPUT CLOSE.


OUTPUT TO c:\temp\dwb_set_list_param.csv.
FOR EACH dwb_set_list_param :

    EXPORT DELIMITER ";" dwb_set_list_param.
    
END.

OUTPUT CLOSE.


OUTPUT TO c:\temp\dwb_set_list_param_aux.csv.
FOR EACH dwb_set_list_param_aux :

    EXPORT DELIMITER ";" dwb_set_list_param_aux .
    
END.

OUTPUT CLOSE.
