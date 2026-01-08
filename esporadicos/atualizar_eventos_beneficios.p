FOR EACH efp_emis_inform_rendto 
WHERE cdn_event_fp = '470'
AND idi_grp_dirf = 2 :
    UPDATE efp_emis_inform_rendto EXCEPT idi_grp_dirf WITH 1 COL WIDTH 600.

END.
