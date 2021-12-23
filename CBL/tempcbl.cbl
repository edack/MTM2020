       IDENTIFICATION         DIVISION.
       PROGRAM-ID.            TREI0012.
      *AUTHOR                 Mr. W. B. RIBEIRO
      *Rewritten              05.12.2020
      *First written on       10.08.2013
      *------------------------------------------------------*
      * Remarks:  Cobol PGM open reads records and move it   *
      *           from SAMFILE into ISAM & DISKFIL           *
      *           Records received from master dataset       *
      *-------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SAM-FILE ASSIGN TO DISKFIL
                  FILE STATUS IS WS-FS-SAMFILE.
           SELECT ISAM-FILE ASSIGN TO ISAMFIL
                  FILE STATUS IS WS-FS-ISAMFLE.
           SELECT DISKFIL ASSIGN TO UT-S-DISKFILJ
                  FILE STATUS IS WS-FS-DKFIL.
       DATA DIVISION.
       FILE SECTION.
       FD  SAM-FILE
           LABEL RECORD STANDARD
           RECORDING MODE F
           .
       01  SAM-RECORD               PIC X(80).
       FD  ISAM-FILE
           LABEL RECORD STANDARD
           RECORDING MODE F
           .
       01  ISAM-RECORD              PIC X(80).
       FD  DISKFIL
           LABEL RECORD STANDARD
           RECORDING MODE F
           .
       01   REG-DISKFIL            PIC X(80).
       WORKING-STORAGE SECTION.
       01  WS-FLAG-AREA.
           05  WS-FS-SAMFILE        PIC X(02).
           05  WS-FS-DKFIL          PIC X(02).
           05  WS-FS-ISAMFLE        PIC X(02).
           05  WS-COUNTREAD         PIC 9(02).
           05  WS-RCORDISAMFIL      PIC 9(02).
           05  WS-RCORD-DKFIL       PIC 9(02).
           05  WS-MSG               PIC X(30) VALUE SPACES.
           05  WS-FS-MSG            PIC X(02) VALUE SPACES.
       01  WS-SAM-FILE.
           05  WS-ACCNT-NMBER      PIC X(05) VALUE SPACES.
           05  WS-CLIENT-NMBER     PIC X(20) VALUE SPACES.
           05  WS-ADDRESS          PIC X(20) VALUE SPACES.
           05  WS-BILL-NMBER       PIC X(05) VALUE SPACES.
           05  WS-DSKF-NMBER       PIC X(05) VALUE SPACES.
           05  WS-ENERGY-ACCNT     PIC X(05) VALUE SPACES.
           05  WS-GAS-ACCNT        PIC X(05) VALUE SPACES.
           05  WS-RESERVED         PIC X(15) VALUE SPACES.
       01  WS-ISAM-FILE.
           05  WS-ACCNT-NMBER1    PIC X(05) VALUE SPACES.
           05  WS-CLIENT-NMBER1   PIC X(20) VALUE SPACES.
           05  WS-ADDRESS1        PIC X(20) VALUE SPACES.
           05  WS-BILL-NMBER1     PIC X(05) VALUE SPACES.
           05  WS-ENERGY-ACCNT1   PIC X(05) VALUE SPACES.
           05  WS-RESERVED1       PIC X(25) VALUE SPACES.
       01  WS-DISKFILL.
           05  WS-ACCNT-NMBER2    PIC X(05) VALUE SPACES.
           05  WS-CLIENT-NMBER2   PIC X(20) VALUE SPACES.
           05  WS-ADDRESS2        PIC X(20) VALUE SPACES.
           05  WS-DSKF-NMBER2     PIC X(05) VALUE SPACES.
           05  WS-GAS-ACCNT2      PIC X(05) VALUE SPACES.
           05  WS-RESERVED2       PIC X(25) VALUE SPACES.
       PROCEDURE DIVISION.
      *------------------------------------------------------*
      *     MAIN PROCESS
      *------------------------------------------------------*
       000-TREI0012.
           PERFORM 000-MAINLINE
           PERFORM 030-PROCESS UNTIL WS-FS-SAMFILE = '10'
           PERFORM 090-EXIT
           STOP RUN
           .
      *------------------------------------------------------*
      *     INITIAL PROCESSING
      *------------------------------------------------------*
       000-MAINLINE.
           INITIALIZE WS-FLAG-AREA
           PERFORM 020-OPEN-FILE
           PERFORM 025-READ-SAM-FILE
           IF WS-FS-SAMFILE   = '10'
              MOVE 'ERROR:SAM-FILE EMPTY' TO WS-MSG
               MOVE WS-FS-SAMFILE   TO WS-FS-MSG
              GO TO ERROR-ROUTINE
           END-IF
            .
      *------------------------------------------------------*
      *    PROCESS TO OPEN UP FILES
      *------------------------------------------------------*
       020-OPEN-FILE.
           OPEN  INPUT SAM-FILE
           IF WS-FS-SAMFILE NOT = '00'
              MOVE 'ERROR UPON OPENING SAM-FILE' TO WS-MSG
               MOVE  WS-FS-SAMFILE         TO WS-FS-MSG
                 GO TO ERROR-ROUTINE
           END-IF
           OPEN  OUTPUT  ISAM-FILE
           IF WS-FS-ISAMFLE NOT = '00'
              MOVE 'ERROR UPON OPENING ISAM-FILE' TO WS-MSG
               MOVE  WS-FS-ISAMFLE         TO WS-FS-MSG
                GO TO ERROR-ROUTINE
           END-IF
           OPEN  OUTPUT  DISKFIL
           IF WS-FS-DKFIL NOT = '00'
              MOVE  'ERROR UPON OPENING DISKFIL' TO WS-MSG
               MOVE  WS-FS-DKFIL           TO WS-FS-MSG
                GO TO ERROR-ROUTINE
           END-IF
           .
      *------------------------------------------------------*
      *    READING SAM FILES PROCESS
      *------------------------------------------------------*
       025-READ-SAM-FILE.
           READ  SAM-FILE INTO   WS-SAM-FILE
           IF WS-FS-SAMFILE NOT = '00' AND '10'
              MOVE 'ERROR UPON READING SAMFILE' TO WS-MSG
               MOVE  WS-FS-SAMFILE   TO WS-FS-MSG
                GO TO ERROR-ROUTINE
             ELSE
              IF WS-FS-SAMFILE = '00'
                ADD    1  TO WS-COUNTREAD
              END-IF
           END-IF
           .
      *------------------------------------------------------*
      *    PROCESSING
      *------------------------------------------------------*
       030-PROCESS.
           MOVE WS-ACCNT-NMBER TO WS-ACCNT-NMBER1 WS-ACCNT-NMBER2
      *
           MOVE WS-CLIENT-NMBER TO WS-CLIENT-NMBER1 WS-CLIENT-NMBER2
           MOVE WS-ADDRESS     TO WS-ADDRESS1     WS-ADDRESS2
           MOVE WS-BILL-NMBER TO WS-BILL-NMBER1
           MOVE WS-DSKF-NMBER  TO WS-DSKF-NMBER2
           MOVE WS-ENERGY-ACCNT TO WS-ENERGY-ACCNT1
           MOVE WS-GAS-ACCNT TO WS-GAS-ACCNT2
           PERFORM 035-RCORD-ISAM-FILE
           PERFORM 035-RCORD-DISKFIL
           PERFORM 025-READ-SAM-FILE
           .
      *------------------------------------------------------*
      *    RECORDING ON ISAM FILE
      *------------------------------------------------------*
       035-RCORD-ISAM-FILE.
           WRITE ISAM-RECORD FROM WS-ISAM-FILE
           IF WS-FS-ISAMFLE NOT = '00'
              MOVE  'ERROR UPON RECORDING ISAM-FILE' TO WS-MSG
              MOVE WS-FS-ISAMFLE TO WS-FS-MSG
              GO TO ERROR-ROUTINE
           END-IF
           ADD 1 TO WS-RCORDISAMFIL
           .
      *------------------------------------------------------*
      *    RECORDING FILE DISKFIL
      *------------------------------------------------------*
       035-RCORD-DISKFIL.
           WRITE REG-DISKFIL FROM WS-DISKFILL
           IF WS-FS-ISAMFLE NOT = '00'
              MOVE 'ERROR UPON RECORDING DISKFIL' TO WS-MSG
              MOVE WS-FS-DKFIL TO WS-FS-MSG
              GO TO ERROR-ROUTINE
           END-IF
           ADD 1 TO WS-RCORD-DKFIL
           .
      *------------------------------------------------------------
      *     FINAL PROCEDURE
      *------------------------------------------------------------
       090-EXIT.
           DISPLAY '*-----------------------------------------*'
           DISPLAY '*         QUICK REPORT ON FILES           *'
           DISPLAY '*-----------------------------------------*'
           DISPLAY '* FILES READ        - SAMFILE: ' WS-COUNTREAD
           DISPLAY '* FILES RECORDED    - ISAMFIL: ' WS-RCORDISAMFIL
           DISPLAY '* FILES RECORDED    - DISFILL: ' WS-RCORD-DKFIL
           DISPLAY '*-----------------------------------------*'
           PERFORM  055-CLOSE-FILE
           DISPLAY '*-----------------------------------------*'        -*'
           DISPLAY '*      NORMAL END OF TREI0012             *'
           DISPLAY '*-----------------------------------------*'
           .
      *------------------------------------------------------*
      *    PROCESS FOR CLOSING UP THE FILES
      *------------------------------------------------------*
       055-CLOSE-FILE.
           CLOSE SAM-FILE
           IF WS-FS-SAMFILE NOT = '00'
              MOVE 'ERROR UPON CLOSING SAM-FILE' TO WS-MSG
              MOVE WS-FS-SAMFILE TO WS-FS-MSG
              GO TO ERROR-ROUTINE
           END-IF
           CLOSE ISAM-FILE
           IF WS-FS-ISAMFLE NOT = '00'
              MOVE 'ERROR UPON CLOSING ISAM-FILE' TO WS-MSG
              MOVE WS-FS-ISAMFLE TO WS-FS-MSG
              GO TO ERROR-ROUTINE
           END-IF
           CLOSE DISKFIL
           IF WS-FS-DKFIL NOT = '00'
              MOVE 'ERROR UPON CLOSING DISKFIL' TO WS-MSG
              MOVE WS-FS-DKFIL TO WS-FS-MSG
              GO TO ERROR-ROUTINE
           END-IF
           .
      *---------------------------------------------------------*
      *    DEALING WITH ERROR ROUTINE
      *---------------------------------------------------------*
       ERROR-ROUTINE.
           DISPLAY '*------------------------------*'
           DISPLAY '     Your PGM has been cancelled *'
           DISPLAY '-------------------------------*'
           DISPLAY '* Ops message = ' WS-MSG
           DISPLAY '* FILE STATUS = ' WS-FS-MSG
           DISPLAY '*---------------------------------*'
           DISPLAY '* ABNORMAL END OF COBPGM TREI0012 *'
           DISPLAY '*---------------------------------*'
           STOP RUN .
