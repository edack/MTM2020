//TOPACCT  JOB 1,NOTIFY=&SYSUID
//***************************************************/
// SET COBPGM='TOPACCTS'
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(&COBPGM),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(&COBPGM),DISP=SHR
//COBOL.SYSLIB DD DISP=SHR,
//        DSN=&SYSUID..COPYLIB
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN     EXEC PGM=&COBPGM
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//ACCTREC   DD DSN=MTM2020.PUBLIC.INPUT(CUSTRECS),DISP=SHR
//PRTLINE   DD DSN=&SYSUID..OUTPUT(TOPACCTS),DISP=SHR
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
