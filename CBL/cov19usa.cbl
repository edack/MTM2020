       IDENTIFICATION DIVISION.
       PROGRAM-ID. COV19USA.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USA-HIST-FILE ASSIGN TO USAFILE.
           SELECT PRINT-FILE    ASSIGN TO UT-S-PRTFILE.
      *===============================================================*
       DATA DIVISION.
      *---------------------------------------------------------------*
       FILE SECTION.
       FD  USA-HIST-FILE
               RECORDING MODE F.
       01  USA-HIST-RECORD            PIC X(225).
      *---------------------------------------------------------------*
       FD  PRINT-FILE
               RECORDING MODE IS F.
       01  PRINT-RECORD.
      *    05  CC                     PIC X(01).
           05  PRINT-LINE             PIC X(132).
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
       01   REPORT-LINES.
      *---------------------------------------------------------------*
           05  NEXT-REPORT-LINE       PIC X(132).
      *---------------------------------------------------------------*
           05  DL1-RECORD.
               10  DL1-TIMESTAMP.
                   15  DL1-MONTH      PIC X(02).
                   15  FILLER         PIC X(01)  VALUE '/'.
                   15  DL1-DAY        PIC X(02).
                   15  FILLER         PIC X(01)  VALUE '/'.
                   15   DL1-YEAR      PIC X(04).
               10  DL1-STATE          PIC X(01)  VALUE SPACE.
               10  DL1-CASE-POSITIVE  PIC ZZ,ZZZ,ZZ9.
               10  FILLER             PIC X(01)  VALUE SPACE.
               10  DL1-CASE-NEGATIVE  PIC ZZ,ZZZ,ZZ9.
               10  FILLER             PIC X(02)  VALUE SPACE.
               10  DL1-CASE-PENDING   PIC ZZ,ZZ9.
               10  FILLER             PIC X(01)  VALUE SPACE.
               10  DL1-CASE-NEW       PIC Z,ZZZ,ZZ9.
               10  DL1-HOSPITAL-TOT   PIC Z,ZZZ,ZZ9.
               10  DL1-ICU-TOT        PIC Z,ZZZ,ZZ9.
               10  FILLER             PIC X(01)  VALUE SPACE.
               10  DL1-VENT-TOT       PIC ZZZ,ZZ9.
               10  FILLER             PIC X(02)  VALUE SPACE.
               10  DL1-RECOVERED      PIC Z,ZZZ,ZZ9.
               10  DL1-DEATH          PIC ZZ,ZZZ,ZZ9.
               10  FILLER             PIC X(01)  VALUE SPACE.
               10  DL1-DEATH-NEW      PIC Z,ZZZ,ZZ9.
               10  FILLER             PIC X(01)  VALUE SPACE.
               10  DL1-DEATH-PERCENT  PIC Z9.9999.
               10  FILLER             PIC X(03)  VALUE '%  '.
               10  DL1-CASE-PERCENT   PIC Z9.9999.
               10  FILLER             PIC X(01)  VALUE '%'.
      *---------------------------------------------------------------*
           05  HEADING-LINE-1.
               10 HL1-DATE.
                   15  FILLER         PIC X(01) VALUE SPACE.
                   15  FILLER         PIC X(12) VALUE 'TODAYS DATE:'.
                   15  HL1-MONTH-OUT  PIC XX.
                   15  FILLER         PIC X     VALUE '/'.
                   15  HL1-DAY-OUT    PIC XX.
                   15  FILLER         PIC X     VALUE '/'.
                   15  HL1-YEAR-OUT   PIC XX.
               10  FILLER             PIC X(80) VALUE SPACE.
               10  HL1-PAGE-COUNT-AREA.
                   15  FILLER         PIC X(04) VALUE SPACE.
                   15  FILLER         PIC X(05) VALUE 'PAGE:'.
                   15  HL1-PAGE-NUM   PIC ZZZZ9.
                   15  FILLER         PIC X(05) VALUE SPACE.
      *---------------------------------------------------------------*
           05  HEADING-LINE-2.
               10  FILLER    PIC X(12) VALUE '  AS OF     '.
               10  FILLER    PIC X(20) VALUE '  POSITIVE  NEGATIVE'.
               10  FILLER    PIC X(20) VALUE '    PEND     NEW +  '.
               10  FILLER    PIC X(20) VALUE 'HOSPITAL   ICU      '.
               10  FILLER    PIC X(20) VALUE 'VENT                '.
               10  FILLER    PIC X(20) VALUE 'TOTAL     NEW    DEA'.
               10  FILLER    PIC X(20) VALUE 'TH     N CASE       '.
      *---------------------------------------------------------------*
           05  HEADING-LINE-3.
               10  FILLER    PIC X(12) VALUE '  DATE      '.
               10  FILLER    PIC X(20) VALUE '   TESTS      TESTS '.
               10  FILLER    PIC X(20) VALUE '   TESTS     TESTS  '.
               10  FILLER    PIC X(20) VALUE ' ADMITS   ADMITS    '.
               10  FILLER    PIC X(20) VALUE 'ADMIT  RECOVER     D'.
               10  FILLER    PIC X(20) VALUE 'EATHS    DEATHS  PER'.
               10  FILLER    PIC X(20) VALUE 'CENT   PERCENT      '.
      *---------------------------------------------------------------*
           05  HEADING-LINE-4.
               10  FILLER    PIC X(12) VALUE '  ----      '.
               10  FILLER    PIC X(20) VALUE '  -------  ---------'.
               10  FILLER    PIC X(20) VALUE '   -----     -----  '.
               10  FILLER    PIC X(20) VALUE ' ------   ------    '.
               10  FILLER    PIC X(20) VALUE '-----  -------     -'.
               10  FILLER    PIC X(20) VALUE '-----    ------  ---'.
               10  FILLER    PIC X(20) VALUE '----   -------      '.
       COPY UHRECORD.
      *---------------------------------------------------------------*
       01  SWITCHES-MISC-FIELDS.
      *---------------------------------------------------------------*
           05  WS-PERCENT                  PIC 99V999999.
           05  TOTAL-ACCUMULATORS.
               10  TA-CASE-TOT             PIC 9(08).
               10  TA-DEATH-TOT            PIC 9(08).
           05  FILE-STATUS                 PIC X(02).
           05  END-OF-FILE-SW              PIC X(01)   VALUE 'N'.
               88  END-OF-FILE                         VALUE 'Y'.
           05  VALID-RECORD-SW             PIC X(01)   VALUE 'Y'.
               88  VALID-RECORD                        VALUE 'Y'.
       COPY PRINTCTL.
      *===============================================================*
       PROCEDURE DIVISION.
      *---------------------------------------------------------------*
       0000-MAIN-PROCESSING.
      *---------------------------------------------------------------*
           PERFORM 1000-OPEN-FILES.
           PERFORM 8000-READ-USA-HIST-FILE.
           PERFORM 2000-PROCESS-USA-HIST-FILE
               UNTIL END-OF-FILE.
           PERFORM 3000-CLOSE-FILES.
           GOBACK.
      *---------------------------------------------------------------*
       1000-OPEN-FILES.
      *---------------------------------------------------------------*
           OPEN INPUT  USA-HIST-FILE
                OUTPUT PRINT-FILE.
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE-DATA.
           MOVE WS-CURRENT-YEAR            TO HL1-YEAR-OUT.
           MOVE WS-CURRENT-MONTH           TO HL1-MONTH-OUT.
           MOVE WS-CURRENT-DAY             TO HL1-DAY-OUT.
      *---------------------------------------------------------------*
       2000-PROCESS-USA-HIST-FILE.
      *---------------------------------------------------------------*
           MOVE UHR-DAY                    TO DL1-DAY.
           MOVE UHR-MONTH                  TO DL1-MONTH.
           MOVE UHR-YEAR                   TO DL1-YEAR.
           MOVE UHR-CASE-POSITIVE          TO DL1-CASE-POSITIVE.
           MOVE UHR-CASE-NEGATIVE          TO DL1-CASE-NEGATIVE.
           MOVE UHR-CASE-PENDING           TO DL1-CASE-PENDING.
           MOVE UHR-POSITIVE-INCREASE      TO DL1-CASE-NEW.
           MOVE UHR-HOSPITAL-TOT           TO DL1-HOSPITAL-TOT.
           MOVE UHR-ICU-TOT                TO DL1-ICU-TOT.
           MOVE UHR-VENT-TOT               TO DL1-VENT-TOT.
           MOVE UHR-RECOVERED              TO DL1-RECOVERED.
           MOVE UHR-DEATH                  TO DL1-DEATH.
           MOVE UHR-DEATH-INCREASE         TO DL1-DEATH-NEW.
           IF  UHR-CASE-POSITIVE > ZERO
               DIVIDE UHR-DEATH  BY UHR-CASE-POSITIVE
                   GIVING WS-PERCENT
               MULTIPLY WS-PERCENT BY 100 GIVING DL1-DEATH-PERCENT
               DIVIDE UHR-POSITIVE-INCREASE   BY UHR-CASE-POSITIVE
                   GIVING WS-PERCENT
               MULTIPLY WS-PERCENT BY 100 GIVING DL1-CASE-PERCENT
           ELSE
               MOVE ZERO                   TO DL1-DEATH-PERCENT
                                              DL1-CASE-PERCENT.
           MOVE DL1-RECORD                 TO NEXT-REPORT-LINE.
           PERFORM 9000-PRINT-REPORT-LINE.
           PERFORM 8000-READ-USA-HIST-FILE.
      *---------------------------------------------------------------*
       3000-CLOSE-FILES.
      *---------------------------------------------------------------*
           CLOSE USA-HIST-FILE
                 PRINT-FILE.
      *---------------------------------------------------------------*
       8000-READ-USA-HIST-FILE.
      *---------------------------------------------------------------*
           READ USA-HIST-FILE
               AT END MOVE 'Y'             TO END-OF-FILE-SW
                      MOVE 'N'             TO VALID-RECORD-SW.
           IF VALID-RECORD
               UNSTRING USA-HIST-RECORD DELIMITED BY ','
               INTO UHR-DATE
                   UHR-STATE
                   UHR-CASE-POSITIVE
                   UHR-CASE-NEGATIVE
                   UHR-CASE-PENDING
                   UHR-HOSPITAL-CURR
                   UHR-HOSPITAL-TOT
                   UHR-ICU-CURR
                   UHR-ICU-TOT
                   UHR-VENT-CURR
                   UHR-VENT-TOT
                   UHR-RECOVERED
                   UHR-DATE-CHECKED
                   UHR-DEATH
                   UHR-HOSPTALIZED
                   UHR-TOT-TESTS
                   UHR-LAST-MODIFIED
                   UHR-TOTAL
                   UHR-POS-NEG
                   UHR-DEATH-INCREASE
                   UHR-POSITIVE-INCREASE
                   UHR-NEGATIVE-INCREASE
                   UHR-TOT-TEST-INCREASE
                   UHR-HOSPITAL-INCREASE
                   UHR-HASH.
      *---------------------------------------------------------------*
       9000-PRINT-REPORT-LINE.
      *---------------------------------------------------------------*
           IF LINE-COUNT GREATER THAN LINES-ON-PAGE
              PERFORM 9100-PRINT-HEADING-LINES.
           MOVE NEXT-REPORT-LINE           TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
      *---------------------------------------------------------------*
       9100-PRINT-HEADING-LINES.
      *---------------------------------------------------------------*
           MOVE PAGE-COUNT                 TO HL1-PAGE-NUM.
           MOVE HEADING-LINE-1             TO PRINT-LINE.
           PERFORM 9110-WRITE-TOP-OF-PAGE.
           MOVE 2                          TO LINE-SPACEING.
           MOVE HEADING-LINE-2             TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           MOVE 1                          TO LINE-SPACEING.
           MOVE HEADING-LINE-3             TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           MOVE HEADING-LINE-4             TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           ADD 1                           TO PAGE-COUNT.
           MOVE 6                          TO LINE-COUNT.
      *---------------------------------------------------------------*
       9110-WRITE-TOP-OF-PAGE.
      *---------------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING PAGE.
      *---------------------------------------------------------------*
       9120-WRITE-PRINT-LINE.
      *---------------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING LINE-SPACEING.
           ADD LINE-SPACEING               TO LINE-COUNT.
           MOVE 1                          TO LINE-SPACEING.
           MOVE SPACE                      TO PRINT-LINE.
