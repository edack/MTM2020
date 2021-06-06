//CBLGRPH  JOB 1,NOTIFY=&SYSUID
//***************************************************/
//  SET COBPGM='GRAPHUSA'
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(&COBPGM),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(&COBPGM),DISP=SHR
//COBOL.SYSLIB DD DISP=SHR,
//        DSN=&SYSUID..COPYLIB
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//RUN       EXEC PGM=&COBPGM
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//ACCTREC   DD DSN=&SYSUID..DATA(DATA),DISP=SHR
//FAVRFP    DD DSN=&SYSUID..DATA(FAVRFP),DISP=SHR
//RFPFILE   DD DSN=&SYSUID..DATA(RFPFILE),DISP=SHR
//CSVFILE   DD DSN=&SYSUID..DATA(CLAIMFL),DISP=SHR
//CLAIMFL   DD DSN=&SYSUID..DATA(FIXEDFL),DISP=SHR
//USAFILE   DD DSN=&SYSUID..NCOV19.USAFILE,DISP=SHR
//PRTFILE   DD SYSOUT=*,OUTLIM=15000
//CLAIMRPT  DD SYSOUT=*,OUTLIM=15000
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD SYSOUT=*,OUTLIM=15000
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF