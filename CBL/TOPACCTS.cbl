       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TOPACCTS.
       AUTHOR.        EDWIN ACKERMAN.
       INSTALLATION.  MORONS LOSERS AND BIMBOS LP.
       DATE-WRITTEN.       09/27/2020.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.     IBM-3906.
       OBJECT-COMPUTER.     IBM-3906.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-FILE
               ASSIGN TO PRTLINE.
           SELECT INPUT-FILE
               ASSIGN TO ACCTREC.
      *==========================================================*
       DATA DIVISION.
      *----------------------------------------------------------*
       FILE SECTION.
      *----------------------------------------------------------*
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05  IR-FIRST-NAME               PIC X(11).
           05  IR-LAST-NAME                PIC X(22).
           05  IR-FILLER                   PIC X(28).
           05  IR-ACCT-AMT                 PIC X(12).
           05  FILLER                      PIC X(07).
      *----------------------------------------------------------*
       FD  PRINT-FILE.
       01  PRINT-RECORD.
      *    05 CC                           PIC X(01).
           05 PRINT-LINE                   PIC X(79).
      *----------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------*
       01  PRINT-LINES.
           05  NEXT-REPORT-LINE            PIC X(80) VALUE SPACE.
      *----------------------------------------------------------*
       01  HEADING-LINES.
      *----------------------------------------------------------*
           05  HEADING-LINE-1.
               10  FILLER  PIC X(20) VALUE 'REPORT OF TOP ACCOUN'.
               10  FILLER  PIC X(20) VALUE 'T BALANCE HOLDERS   '.
               10  FILLER  PIC X(20) VALUE '                    '.
               10  FILLER  PIC X(14) VALUE '        PAGE: '.
               10  HL1-PAGE-COUNT          PIC ZZ9.
               10  FILLER                  PIC X(03) VALUE SPACE.
      *----------------------------------------------------------*
           05  HEADING-LINE-2.
               10  FILLER              PIC X(13) VALUE 'PREPARED FOR '.
               10  HL2-PREPARED-NAME   PIC X(27).
               10  FILLER              PIC X(04) VALUE "ON: ".
               10  HL2-MONTH           PIC X(02).
               10  FILLER              PIC X(01) VALUE "/".
               10  HL2-DAY             PIC X(02).
               10  FILLER              PIC X(01) VALUE "/".
               10  HL2-YEAR            PIC X(04).
               10  FILLER              PIC X(26) VALUE SPACE.
      *----------------------------------------------------------*
           05  HEADING-LINE-3.
               10  FILLER  PIC X(20) VALUE 'CUSTOMER NAME       '.
               10  FILLER  PIC X(20) VALUE '        ACCOUNT AMOU'.
               10  FILLER  PIC X(20) VALUE 'NT                  '.
               10  FILLER  PIC X(20) VALUE '                    '.
      *----------------------------------------------------------*
           05  HEADING-LINE-4.
               10  FILLER  PIC X(80) VALUE ALL "=".
      *----------------------------------------------------------*
           05  TOTAL-LINE-1.
               10  FILLER  PIC X(20) VALUE 'TOTAL HIGH ACCOUNTS:'.
               10  FILLER  PIC X(02) VALUE SPACE.
               10  TL1-TOT-HIGH-ACCTS  PIC ZZ,ZZ9.
               10  FILLER  PIC X(02) VALUE SPACE.
               10  FILLER  PIC X(50) VALUE SPACE.
      *----------------------------------------------------------*
       01 DETAIL-LINES.
      *----------------------------------------------------------*
           05  DETAIL-LINE-1.
               10  DL1-CLIENT-NAME          PIC X(25).
               10  FILLER                  PIC X(03) VALUE SPACE.
               10  DL1-ACCT-AMT             PIC $$,$$$,$$9.99.
               10  FILLER                  PIC X(40) VALUE SPACE.
      *         10  FILLER  PIC X(80).
      *----------------------------------------------------------*
       01  WS-SWITCHES-SUBSCRIPTS-MISC.
      *----------------------------------------------------------*
           05  END-OF-FILE-SW              PIC X VALUE 'N'.
               88  END-OF-FILE                   VALUE 'Y'.
           05  WS-ACCUM-FIELDS.
               10  WS-HIGH-ACCT-CNT        PIC 9(04) VALUE 0.
           05  WS-NUMBER-FIELDS.
               10  WS-NUM-ACCT-AMT         PIC 9(09)V99.
       COPY PRINTCTL.
      *==========================================================*
       PROCEDURE DIVISION.
      *----------------------------------------------------------*
       0000-MAIN-PROCESSING.
      *----------------------------------------------------------*
           PERFORM 1000-OPEN-FILES.
           PERFORM 8000-READ-ACCT-FILE.
           PERFORM 2000-PROCESS-ACCT-FILE
               UNTIL END-OF-FILE.
           PERFORM 3000-PRINT-TOTAL-LINES.
           PERFORM 4000-CLOSE-FILES.
           GOBACK.
      *----------------------------------------------------------*
       1000-OPEN-FILES.
      *----------------------------------------------------------*
           OPEN    INPUT  INPUT-FILE
                   OUTPUT PRINT-FILE.
           MOVE "ED ACKERMAN / Z00070" TO HL2-PREPARED-NAME.
           MOVE FUNCTION CURRENT-DATE  TO WS-CURRENT-DATE-DATA.
           MOVE WS-CURRENT-MONTH       TO HL2-MONTH.
           MOVE WS-CURRENT-DAY         TO HL2-DAY.
           MOVE WS-CURRENT-YEAR        TO HL2-YEAR.
      *----------------------------------------------------------*
       2000-PROCESS-ACCT-FILE.
      *----------------------------------------------------------*
           MOVE SPACE                   TO DL1-CLIENT-NAME.
           COMPUTE WS-NUM-ACCT-AMT = FUNCTION NUMVAL-C(IR-ACCT-AMT)
           IF  WS-NUM-ACCT-AMT  > 8500000
               STRING IR-FIRST-NAME DELIMITED BY SPACE
                   SPACE DELIMITED BY SIZE
                   IR-LAST-NAME DELIMITED BY SPACE
                   INTO DL1-CLIENT-NAME
               MOVE WS-NUM-ACCT-AMT      TO DL1-ACCT-AMT
               ADD 1                     TO WS-HIGH-ACCT-CNT
               MOVE DETAIL-LINE-1        TO NEXT-REPORT-LINE
               PERFORM 9000-PRINT-REPORT-LINE.
           PERFORM 8000-READ-ACCT-FILE.
      *----------------------------------------------------------*
       3000-PRINT-TOTAL-LINES.
      *----------------------------------------------------------*
           MOVE WS-HIGH-ACCT-CNT         TO TL1-TOT-HIGH-ACCTS.
           MOVE 2                        TO LINE-SPACEING.
           MOVE  TOTAL-LINE-1            TO NEXT-REPORT-LINE.
           PERFORM 9000-PRINT-REPORT-LINE.
      *----------------------------------------------------------*
       4000-CLOSE-FILES.
      *----------------------------------------------------------*
           CLOSE INPUT-FILE
                 PRINT-FILE.
      *----------------------------------------------------------*
       8000-READ-ACCT-FILE.
      *----------------------------------------------------------*
           READ INPUT-FILE
               AT END MOVE 'Y' TO END-OF-FILE-SW.
      *----------------------------------------------------------*
       9000-PRINT-REPORT-LINE.
      *----------------------------------------------------------*
           IF LINE-COUNT GREATER THAN LINES-ON-PAGE
               PERFORM 9100-PRINT-HEADING-LINES.
           MOVE NEXT-REPORT-LINE TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
      *----------------------------------------------------------*
       9100-PRINT-HEADING-LINES.
      *----------------------------------------------------------*
           MOVE PAGE-COUNT           TO HL1-PAGE-COUNT.
           MOVE HEADING-LINE-1       TO PRINT-LINE.
           PERFORM 9110-WRITE-TOP-OF-PAGE.
           MOVE 1                    TO LINE-SPACEING.
           MOVE HEADING-LINE-2       TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           MOVE HEADING-LINE-3       TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           MOVE HEADING-LINE-4       TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           ADD  1                    TO PAGE-COUNT.
           MOVE 1                    TO LINE-SPACEING.
           MOVE 5                    TO LINE-COUNT.
      *----------------------------------------------------------*
       9110-WRITE-TOP-OF-PAGE.
      *----------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING PAGE.
           MOVE SPACE                TO PRINT-LINE.
      *----------------------------------------------------------*
       9120-WRITE-PRINT-LINE.
      *----------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING LINE-SPACEING.
           MOVE SPACE                TO PRINT-LINE.
           ADD  1                    TO LINE-COUNT.
           MOVE 1                    TO LINE-SPACEING.
