//JES1JOB2 JOB 1
//*
//*  JCL line 5 needs a SECRET= value to execute
//*
//SETVAR   SET SECRET=GIRAFFE
//SECRET   EXEC PGM=IEBGENER
//SYSPRINT DD DUMMY
//SYSUT1   DD DSN=MTM2020.PUBLIC.WORK(&SECRET),DISP=SHR
//SYSUT2   DD DSN=&SYSUID..OUTPUT(JES1JOB2),DISP=SHR
//SYSIN    DD DUMMY
