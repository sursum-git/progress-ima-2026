MESSAGE "Diretorio: " session:temp-directory

VIEW-AS ALERT-BOX INFO BUTTONS OK.

ASSIGN 

SESSION:DEBUG-ALERT = YES

LOG-MANAGER:LOGFILE-NAME = string(session:temp-directory + 'clientlog.txt')

LOG-MANAGER:LOGGING-LEVEL = 4

LOG-MANAGER:LOG-ENTRY-TYPES = "4GLMessages,4GLTrace,FileID".

/**************************************************************************/ 
