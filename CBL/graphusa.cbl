       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRPHUSA.
       AUTHOR.        ED ACKERMAN.
       INSTALLATION.  MORONS, LOSERS AND BIMBOES.
       DATE-WRITTEN.  11/28/2020.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-3096.
       OBJECT-COMPUTER.  IBM-3096.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USA-HIST-FILE ASSIGN TO USAFILE.
           SELECT PRINT-FILE    ASSIGN TO PRTFILE.
      *===============================================================*
       DATA DIVISION.
      *---------------------------------------------------------------*
       FILE SECTION.
       FD  USA-HIST-FILE
               RECORDING MODE IS F.
       01  UHR-RECORD                      PIC X(130).
      *---------------------------------------------------------------*
       FD  PRINT-FILE
               RECORDING MODE IS F.
       01  PRINT-RECORD.
      *     05  CC                          PIC X(01).
           05  PRINT-LINE                  PIC X(132).
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
       01   REPORT-LINES.
      *---------------------------------------------------------------*
           05  UHR-PRINT-RECORD.
      *---------------------------------------------------------------*
               10  UHR-TIMESTAMP.
                   15  PR-MONTH            PIC X(02).
                   15  FILLER              PIC X(01)  VALUE '/'.
                   15  PR-DAY              PIC X(02).
                   15  FILLER              PIC X(01)  VALUE '/'.
                   15  PR-YEAR             PIC X(04).
               10  FILLER                  PIC X(02)  VALUE SPACE.
               10  FILLER                  PIC X(02)  VALUE ' |'.
               10  UHR-GRAPH.
                   15  UHR-GRAPH-DATA      PIC X(01) OCCURS 130 TIMES.
               10  FILLER                  PIC X(05)  VALUE SPACE.
      *---------------------------------------------------------------*
           05  NEXT-REPORT-LINE            PIC X(132)  VALUE SPACE.
      *---------------------------------------------------------------*
       01  HEADING-LINES.
      *---------------------------------------------------------------*
           05  HEADING-LINE-1.
      *---------------------------------------------------------------*
               10 HL1-DATE.
                   15  FILLER              PIC X(01) VALUE SPACE.
                   15  FILLER  PIC X(12) VALUE 'TODAYS DATE:'.
                   15  HL1-MONTH-OUT       PIC XX.
                   15  FILLER              PIC X     VALUE '/'.
                   15  HL1-DAY-OUT         PIC XX.
                   15  FILLER              PIC X     VALUE '/'.
                   15  HL1-YEAR-OUT        PIC XXXX.
               10  FILLER      PIC X(20) VALUE '   REPORTING STATE: '.
               10  HL1-REPORTING-STATE     PIC X(03) VALUE SPACE.
               10  FILLER                  PIC X(05) VALUE SPACE.
               10  FILLER      PIC X(20) VALUE '* = NEW, + = MORTALI'.
               10  FILLER      PIC X(20) VALUE 'TY                  '.
               10  FILLER                  PIC X(10) VALUE SPACE.
               10  HL1-PAGE-COUNT-AREA.
                   15  FILLER              PIC X(04) VALUE SPACE.
                   15  FILLER              PIC X(05) VALUE 'PAGE:'.
                   15  HL1-PAGE-NUM        PIC ZZZZ9.
      *---------------------------------------------------------------*
           05  HEADING-LINE-2.
      *---------------------------------------------------------------*
               10  FILLER      PIC X(51) VALUE SPACE.
               10  FILLER      PIC X(20) VALUE '  CASE %     % /100K'.
               10  FILLER      PIC X(47) VALUE SPACE.
      *---------------------------------------------------------------*
           05  HEADING-LINE-3.
      *---------------------------------------------------------------*
               10  FILLER      PIC X(20) VALUE '             |  %  0'.
               10  FILLER      PIC X(20) VALUE '----+----1----+----2'.
               10  FILLER      PIC X(20) VALUE '----+----3----+----4'.
               10  FILLER      PIC X(20) VALUE '----+----5----+----6'.
               10  FILLER      PIC X(20) VALUE '----+----7----+----8'.
               10  FILLER      PIC X(20) VALUE '----+----9----+----0'.
               10  FILLER      PIC X(10) VALUE '----+----1'.
      *---------------------------------------------------------------*
           05  ERROR-LINE-1.
      *---------------------------------------------------------------*
               10  EL-TIMESTAMP.
                   15  EL-MONTH            PIC X(02).
                   15  FILLER              PIC X(01)  VALUE '/'.
                   15  EL-DAY              PIC X(02).
                   15  FILLER              PIC X(01)  VALUE '/'.
                   15  EL-YEAR             PIC X(04).
               10  FILLER                  PIC X(06) VALUE '   |  '.
               10  EL-PERCENT.
                   15  EL-GRAPH-POINT      PIC ZZ9.999.
                   15  FILLER              PIC X(04) VALUE '%   '.
               10  FILLER      PIC X(11) VALUE '      ***  '.
               10  EL-CAUSE                PIC X(05).
               10  FILLER      PIC X(10) VALUE ' VALUE IS '.
               10  FILLER      PIC X(20) VALUE 'TO LARGE TO GRAPH MU'.
               10  FILLER      PIC X(20) VALUE 'ST BE LESS THAN 11% '.
               10  FILLER      PIC X(20) VALUE ' ***                '.
               10  FILLER      PIC X(13) VALUE '             '.
       COPY USAFILE.
      *---------------------------------------------------------------*
       01  SWITCHES-INDEX-COUNTER-FIELDS.
      *---------------------------------------------------------------*
           05  END-OF-FILE-SW              PIC X(01)  VALUE 'N'.
               88  END-OF-FILE                        VALUE 'Y'.
           05  VALID-RECORD-SW             PIC X(01)  VALUE 'Y'.
               88  VALID-RECORD                       VALUE 'Y'.
           05  REPORT-STATE-SW             PIC X(03)  VALUE 'ALL'.
               88  ALL-STATE-REPORT                   VALUE 'ALL'.
           05  WS-COUNTER                  PIC 9(02)  VALUE ZERO.
           05  WS-CASES                    PIC 9(09).
           05  WS-CASE-NEW                 PIC 9(09).
           05  WS-CASE-NEW-2               PIC 9(09).
           05  WS-CASE-PEND                PIC 9(09).
           05  WS-DEATH                    PIC 9(09).
           05  WS-DEATH-NEW                PIC 9(09).
           05  WS-DEATH-NEW-2              PIC 9(09).
           05  WS-DEATH-PEND               PIC 9(09).
           05  WS-PERCENT                  PIC 999V9(10).
           05  WS-C-GRAPH-PNT              PIC 999V9(10).
           05  WS-D-GRAPH-PNT              PIC 999V9(10).
           05  WS-GRAPH-PNT-X              PIC ZZ9.99999.
           05  WS-GRAPH-INDEX               PIC 999.
           05  WS-PNT1                     PIC 99.
           05  WS-PNT2                     PIC 99.
           05  WS-PREV-DATE.
               10 WS-YEAR                  PIC X(04).
               10 FILLER                   PIC X(01).
               10 WS-MONTH                 PIC X(02).
               10 FILLER                   PIC X(01).
               10 WS-DAY                   PIC X(02).
               10 FILLER                   PIC X(13).
      *---------------------------------------------------------------*
       01  STATE-ACCUMULATION-FIELDS.
      *---------------------------------------------------------------*
           05  STATE-TABLE OCCURS 60 TIMES
                           INDEXED BY STATE-INDEX.
               10  ST-STATE                PIC X(03).
               10  ST-CASES                PIC 9(09).
               10  ST-CASE-NEW             PIC 9(09).
               10  ST-CASE-PEND            PIC 9(09).
               10  ST-DEATH                PIC 9(09).
               10  ST-DEATH-NEW            PIC 9(09).
               10  ST-DEATH-PEND           PIC 9(09).
       COPY PRINTCTL.
      *===============================================================*
       PROCEDURE DIVISION.
      *---------------------------------------------------------------*
       0000-MAIN-PROCESSING.
      *---------------------------------------------------------------*
           PERFORM 1000-OPEN-FILES.
           PERFORM 8000-READ-USA-HIST-FILE.
           MOVE  UHR-DATE                  TO  WS-PREV-DATE.
           PERFORM 2000-PROCESS-USA-HIST-FILE
               UNTIL END-OF-FILE.
           PERFORM 2200-PRINT-DATE-TOTALS.
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
           MOVE SPACE                      TO WS-PREV-DATE.
           INITIALIZE STATE-ACCUMULATION-FIELDS
               REPLACING   NUMERIC DATA BY 0
                           ALPHANUMERIC DATA BY SPACE.
           ACCEPT REPORT-STATE-SW.
      *---------------------------------------------------------------*
       2000-PROCESS-USA-HIST-FILE.
      *---------------------------------------------------------------*
           IF  UHR-DATE NOT = WS-PREV-DATE
               PERFORM 2200-PRINT-DATE-TOTALS
               MOVE  ZERO                  TO  WS-CASES
               MOVE  ZERO                  TO  WS-CASE-NEW
               MOVE  ZERO                  TO  WS-CASE-PEND
               MOVE  ZERO                  TO  WS-DEATH
               MOVE  ZERO                  TO  WS-DEATH-NEW
               MOVE  ZERO                  TO  WS-DEATH-PEND
               INITIALIZE STATE-ACCUMULATION-FIELDS
                   REPLACING NUMERIC DATA BY 0
                             ALPHANUMERIC DATA BY SPACE
               MOVE  UHR-DATE              TO  WS-PREV-DATE.
           PERFORM 2100-ACCUMULATE-DATE-TOTALS.
           PERFORM 8000-READ-USA-HIST-FILE.
      *---------------------------------------------------------------*
       2100-ACCUMULATE-DATE-TOTALS.
      *---------------------------------------------------------------*
           ADD  UHR-CASE                   TO  WS-CASES.
           IF  UHR-CASE-NEW GREATER THAN SPACE
               COMPUTE WS-CASE-NEW-2
                   = FUNCTION NUMVAL-C(UHR-CASE-NEW)
               ADD  WS-CASE-NEW-2          TO  WS-CASE-NEW.
           ADD  UHR-CASE-NEW-PROB          TO  WS-CASE-PEND.
           ADD  UHR-DEATH                  TO  WS-DEATH.
           IF  UHR-DEATH-NEW GREATER THAN SPACE
               COMPUTE WS-DEATH-NEW-2
                   = FUNCTION NUMVAL-C(UHR-DEATH-NEW)
               ADD  WS-DEATH-NEW-2         TO  WS-DEATH-NEW.
           ADD  UHR-DEATH-NEW-PROB         TO  WS-DEATH-PEND.
           PERFORM  2110-ACCUMULATE-STATE-TOTALS.
      *---------------------------------------------------------------*
       2110-ACCUMULATE-STATE-TOTALS.
      *---------------------------------------------------------------*
           SET STATE-INDEX TO 1.
           SEARCH STATE-TABLE
               AT END
                   PERFORM 9900-TABLE-ERROR
               WHEN ST-STATE(STATE-INDEX) = UHR-STATE
                   ADD UHR-CASE           TO ST-CASES(STATE-INDEX)
                   ADD WS-CASE-NEW-2      TO ST-CASE-NEW(STATE-INDEX)
                   ADD UHR-CASE-NEW-PROB  TO ST-CASE-PEND(STATE-INDEX)
                   ADD UHR-DEATH          TO ST-DEATH(STATE-INDEX)
                   ADD WS-DEATH-NEW-2     TO ST-DEATH-NEW(STATE-INDEX)
                   ADD UHR-DEATH-NEW-PROB TO ST-DEATH-PEND(STATE-INDEX)
               WHEN ST-STATE(STATE-INDEX) = SPACE
                   MOVE UHR-STATE         TO ST-STATE(STATE-INDEX)
                   ADD UHR-CASE           TO ST-CASES(STATE-INDEX)
                   ADD WS-CASE-NEW-2      TO ST-CASE-NEW(STATE-INDEX)
                   ADD UHR-CASE-NEW-PROB  TO ST-CASE-PEND(STATE-INDEX)
                   ADD UHR-DEATH          TO ST-DEATH(STATE-INDEX)
                   ADD WS-DEATH-NEW-2     TO ST-DEATH-NEW(STATE-INDEX)
                   ADD UHR-DEATH-NEW-PROB TO ST-DEATH-PEND(STATE-INDEX).
      *---------------------------------------------------------------*
       2200-PRINT-DATE-TOTALS.
      *---------------------------------------------------------------*
           MOVE WS-DAY                     TO PR-DAY.
           MOVE WS-MONTH                   TO PR-MONTH.
           MOVE WS-YEAR                    TO PR-YEAR.
           MOVE ALL SPACES                 TO UHR-GRAPH.
           IF  NOT ALL-STATE-REPORT
               PERFORM 2210-SETUP-STATE.
           IF  WS-CASES > ZERO
               COMPUTE WS-CASE-NEW = WS-CASE-NEW + WS-CASE-PEND
               DIVIDE WS-CASE-NEW  BY WS-CASES
                   GIVING WS-PERCENT
               MULTIPLY WS-PERCENT     BY 100 GIVING WS-C-GRAPH-PNT
      *         COMPUTE WS-PERCENT = (UHR-DEATH / 331000000)
      *         MULTIPLY WS-PERCENT     BY 100000 GIVING WS-D-GRAPH-PNT
               DIVIDE WS-DEATH     BY WS-CASES
                   GIVING WS-PERCENT
               MULTIPLY WS-PERCENT     BY 100 GIVING WS-D-GRAPH-PNT
           ELSE
               MOVE ZERO                   TO WS-C-GRAPH-PNT
                                              WS-D-GRAPH-PNT.
           IF  WS-C-GRAPH-PNT GREATER THAN 10 OR
               WS-D-GRAPH-PNT GREATER THAN 10
               MOVE UHR-DAY                TO EL-DAY
               MOVE UHR-MONTH              TO EL-MONTH
               MOVE UHR-YEAR               TO EL-YEAR
               IF  WS-D-GRAPH-PNT GREATER THAN 10
                   MOVE WS-D-GRAPH-PNT     TO EL-GRAPH-POINT
                   MOVE 'DEATH'            TO EL-CAUSE
               ELSE
                   MOVE WS-C-GRAPH-PNT     TO EL-GRAPH-POINT
                   MOVE 'CASES'            TO EL-CAUSE
               END-IF
               MOVE ERROR-LINE-1           TO NEXT-REPORT-LINE
           ELSE
               COMPUTE WS-GRAPH-INDEX = (WS-D-GRAPH-PNT * 10) + 6
               MOVE ' '              TO UHR-GRAPH-DATA(WS-GRAPH-INDEX)
               COMPUTE WS-GRAPH-INDEX = (WS-C-GRAPH-PNT * 100) + 6
               MOVE '*'              TO UHR-GRAPH-DATA(WS-GRAPH-INDEX)
               MOVE 1                TO WS-PNT1
               PERFORM  2220-FORMAT-PERCENT
                   VARYING WS-PNT2 FROM 3 BY 1
                       UNTIL WS-PNT2 GREATER THAN 7
                   MOVE UHR-PRINT-RECORD   TO NEXT-REPORT-LINE.
           PERFORM 9000-PRINT-REPORT-LINE.
           MOVE ALL SPACES                 TO  UHR-GRAPH.
      *---------------------------------------------------------------*
       2210-SETUP-STATE.
      *---------------------------------------------------------------*
      *     DISPLAY REPORT-STATE-SW.
           SET STATE-INDEX  TO 1.
           SEARCH STATE-TABLE
               WHEN ST-STATE(STATE-INDEX) = REPORT-STATE-SW
                   MOVE ST-CASES(STATE-INDEX)      TO WS-CASES
                   MOVE ST-CASE-NEW(STATE-INDEX)   TO WS-CASE-NEW
                   MOVE ST-CASE-PEND(STATE-INDEX)  TO WS-CASE-PEND
                   MOVE ST-DEATH(STATE-INDEX)      TO WS-DEATH
                   MOVE ST-DEATH-NEW(STATE-INDEX)  TO WS-DEATH-NEW
                   MOVE ST-DEATH-PEND(STATE-INDEX) TO WS-DEATH-PEND.
      *---------------------------------------------------------------*
       2220-FORMAT-PERCENT.
      *---------------------------------------------------------------*
           MOVE WS-C-GRAPH-PNT             TO WS-GRAPH-PNT-X.
           MOVE WS-GRAPH-PNT-X(WS-PNT2:1)  TO UHR-GRAPH-DATA(WS-PNT1).
           ADD  1                          TO WS-PNT1.
           MOVE ' '                        TO UHR-GRAPH-DATA(WS-PNT1).
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
               MOVE ZERO                   TO WS-COUNTER
               INSPECT FUNCTION UPPER-CASE(UHR-RECORD)
                   TALLYING WS-COUNTER FOR ALL "XX:XX:XX"
               IF WS-COUNTER > 0
                   UNSTRING UHR-RECORD DELIMITED BY ','
                   INTO UHR-DATE
                       UHR-STATE
                       UHR-CASE
                       UHR-CASE-CONF
                       UHR-CASE-PROB
                       UHR-CASE-NEW
                       UHR-CASE-NEW-PROB
                       UHR-DEATH
                       UHR-DEATH-CONF
                       UHR-DEATH-PROB
                       UHR-DEATH-NEW
                       UHR-DEATH-NEW-PROB
                       UHR-CREATED-AT
               ELSE
                   UNSTRING UHR-RECORD DELIMITED BY ','
                   INTO UHR-DATE
                       UHR-STATE
                       UHR-CASE
                       UHR-CASE-CONF
                       UHR-CASE-PROB
                       UHR-CASE-NEW
                       UHR-CASE-NEW-PROB
                       UHR-DEATH
                       UHR-DEATH-NEW
                       UHR-DEATH-NEW-PROB
                       UHR-CREATED-AT.
      *---------------------------------------------------------------*
       9000-PRINT-REPORT-LINE.
      *---------------------------------------------------------------*
           IF LINE-COUNT GREATER THAN LINES-ON-PAGE
              PERFORM 9100-PRINT-HEADING-LINES.
           MOVE NEXT-REPORT-LINE           TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           MOVE SPACE                      TO NEXT-REPORT-LINE.
      *---------------------------------------------------------------*
       9100-PRINT-HEADING-LINES.
      *---------------------------------------------------------------*
           MOVE PAGE-COUNT                 TO HL1-PAGE-NUM.
           MOVE REPORT-STATE-SW            TO HL1-REPORTING-STATE.
           MOVE HEADING-LINE-1             TO PRINT-LINE.
           PERFORM 9110-WRITE-TOP-OF-PAGE.
           MOVE 1                          TO LINE-SPACEING.
           MOVE HEADING-LINE-2             TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           MOVE 2                          TO LINE-SPACEING.
           MOVE HEADING-LINE-3             TO PRINT-LINE.
           PERFORM 9120-WRITE-PRINT-LINE.
           MOVE 1                          TO LINE-SPACEING.
           ADD  1                          TO PAGE-COUNT.
           MOVE 6                          TO LINE-COUNT.
      *---------------------------------------------------------------*
       9110-WRITE-TOP-OF-PAGE.
      *---------------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING PAGE.
           MOVE SPACE                      TO PRINT-RECORD.
      *---------------------------------------------------------------*
       9120-WRITE-PRINT-LINE.
      *---------------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING LINE-SPACEING.
           ADD LINE-SPACEING               TO LINE-COUNT.
           MOVE 1                          TO LINE-SPACEING.
           MOVE SPACE                      TO PRINT-RECORD.
      *---------------------------------------------------------------*
       9900-TABLE-ERROR.
      *---------------------------------------------------------------*
