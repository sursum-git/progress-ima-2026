DEFINE BUFFER bf FOR user-web.
    
        
FOR EACH user-web
    WHERE user-web.id = 0 .

    FIND LAST bf USE-INDEX ind_id
        NO-LOCK NO-ERROR.

    ASSIGN user-web.id = IF AVAIL bf THEN bf.id + 1 ELSE 1.
    DISP user-web.id user-web.login . 

  
END.
