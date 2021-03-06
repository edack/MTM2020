//STRTOVER JOB 1,NOTIFY=&SYSUID ,TYPRUN=SCAN
//*****************************************************/
//*                                                   */
//*  DELETE ALL EXISTING MTM2020  DATASETS AND        */
//*  GET FRESH COPIES OF ALL OF THEM. USE WITH CARE!  */
//*                                                   */
//*****************************************************/
//*  DELETE ALL EXISTING MTM2020 DATASETS             */
//*****************************************************/
// SET ID=&SYSUID
//STEP010 EXEC PGM=IEFBR14
//DD2     DD DSN=&ID..JCL,DISP=(MOD,DELETE),
//           SPACE=(TRK,0),UNIT=SYSALLDA
//DD3     DD DSN=&ID..WORK,DISP=(MOD,DELETE),
//           SPACE=(TRK,0),UNIT=SYSALLDA
//DD4     DD DSN=&ID..INPUT,DISP=(MOD,DELETE),
//           SPACE=(TRK,0),UNIT=SYSALLDA
//DD5     DD DSN=&ID..PDS,DISP=(MOD,DELETE),
//           SPACE=(TRK,0),UNIT=SYSALLDA
//DD6     DD DSN=&ID..SOURCE,DISP=(MOD,DELETE),
//           SPACE=(TRK,0),UNIT=SYSALLDA
//DD7     DD DSN=&ID..OUTPUT,DISP=(MOD,DELETE),
//           SPACE=(TRK,0),UNIT=SYSALLDA
//*
//*****************************************************/
//*  GET FRESH COPIES OF ALL MTM2020 DATASETS         */
//*****************************************************/
//STEP020 EXEC PROC=P1X,ID=&ID
//
