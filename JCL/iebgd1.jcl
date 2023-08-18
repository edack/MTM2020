//IEBDGW JOB  1,NOTIFY=&SYSUID
//**********************************************************************
//* CREATION OF A DATASET To BE USED LATER ON
//**********************************************************************
//CRSTEP EXEC PGM=IEFBR14
//DDCREA   DD DSN=&SYSUID..MVSUT.SEQOUT,DISP=SHR
//**********************************************************************
//* CREATION OF THE TESTDATA
//**********************************************************************
//STEP1  EXEC PGM=IEBDG
//SYSPRINT DD SYSOUT=*
//SEQOUT   DD DSN=&SYSUID..MVSUT.SEQOUT,DISP=OLD
//SYSIN    DD DATA
      DSD OUTPUT=(SEQOUT)
      FD  NAME=FIELD1,LENGTH=30,STARTLOC=1,FORMAT=AL,ACTION=TL
      FD  NAME=FIELD2,LENGTH=30,STARTLOC=31,FORMAT=AL,ACTION=TR
      FD  NAME=FIELD3,LENGTH=10,STARTLOC=71,PICTURE=10,                X
                  P'1234567890',INDEX=1
      CREATE QUANTITY=500,NAME=(FIELD1,FIELD2,FIELD3),FILL=X'FF'
      END
/*
