//JES2JOB1  JOB
//COMBINE   EXEC PGM=IRXJCL,PARM='COMBINE'
//SYSEXEC   DD DSN=MTM2020.PUBLIC.SOURCE,DISP=SHR
//SYSTSPRT  DD DSN=&SYSUID..OUTPUT(JES2JOB1),DISP=SHR
//SYSTSIN   DD DUMMY
//ITEM      DD DSN=MTM2020.PUBLIC.INPUT(ITEM),DISP=SHR
//PRICE     DD DSN=MTM2020.PUBLIC.INPUT(PRICE),DISP=SHR
//LOCATION  DD DSN=MTM2020.PUBLIC.INPUT(LOCATION),DISP=SHR
