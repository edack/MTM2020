//PDS1CCAT JOB 1,NOTIFY=&SYSUID                                         00010000
//COPY     EXEC PGM=IEBGENER                                            00020000
//SYSPRINT DD SYSOUT=*                                                  00030000
//SYSUT1   DD DSN=&SYSUID..WORK(PDSPART1),DISP=SHR                      00040000
//         DD DSN=&SYSUID..WORK(PDSPART2),DISP=SHR                      00050000
//SYSUT2   DD DSN=&SYSUID..WORK(PDS1OUT),DISP=SHR                       00060000
//SYSIN    DD DUMMY                                                     00080000
