       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRAPHST.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  ST-HIST-FILE ASSIGN TO STATEFL.
           SELECT PRINT-FILE    ASSIGN TO PRTFILE.
      *===============================================================*
       DATA DIVISION.
      *---------------------------------------------------------------*
       FILE SECTION.
       FD  ST-HIST-FILE
               RECORDING MODE IS F.
       01  STR-RECORD                  PIC X(300).
      *---------------------------------------------------------------*
       FD  PRINT-FILE
               RECORDING MODE F.
       01  PRINT-RECORD.
      *    05  CC                      PIC X(01).
           05  PRINT-LINE              PIC X(130).
      *---------------------------------------------------------------*
       WORKING-STORAGE SECTION.
      *---------------------------------------------------------------*
       01   REPORT-LINES.
      *---------------------------------------------------------------*
           05  NEXT-REPORT-LINE        PIC X(132)  VALUE SPACE.
      *---------------------------------------------------------------*
           05      STR-PRINT-RECORD.
               10  STR-TIMESTAMP.
                   15  PR-MONTH        PIC X(02).
                   15  FILLER          PIC X(01)  VALUE '/'.
                   15  PR-DAY          PIC X(02).
                   15  FILLER          PIC X(01)  VALUE '/'.
                   15  PR-YEAR         PIC X(04).
               10  FILLER              PIC X(02)  VALUE SPACE.
               10  FILLER              PIC X(02)  VALUE ' |'.
               10  STR-GRAPH.
                   15  STR-GRAPH-DATA  PIC X(01) OCCURS 110 TIMES.
               10  FILLER              PIC X(05)  VALUE SPACE.
      *---------------------------------------------------------------*
           05  HEADING-LINE-1.
               10 HL1-DATE.
                   15  FILLER          PIC X(01) VALUE SPACE.
                   15  FILLER          PIC X(12) VALUE 'TODAYS DATE:'.
                   15  HL1-MONTH-OUT   PIC XX.
                   15  FILLER          PIC X     VALUE '/'.
                   15  HL1-DAY-OUT     PIC XX.
                   15  FILLER          PIC X     VALUE '/'.
                   15  HL1-YEAR-OUT    PIC XX.
               10  FILLER    PIC X(20) VALUE '   REPORTING STATE: '.
               10  HL1-REPORTING-STATE PIC X(03) VALUE SPACE.
               10  FILLER              PIC X(07) VALUE SPACE.
               10  FILLER    PIC X(20) VALUE '* = NEW, + = MORTALI'.
               10  FILLER    PIC X(20) VALUE 'TY                  '.
               10  FILLER              PIC X(10) VALUE SPACE.
               10  HL1-PAGE-COUNT-AREA.
                   15  FILLER          PIC X(04) VALUE SPACE.
                   15  FILLER          PIC X(05) VALUE 'PAGE:'.
                   15  HL1-PAGE-NUM    PIC ZZZZ9.
      *---------------------------------------------------------------*
           05  HEADING-LINE-2.
               10  FILLER              PIC X(51) VALUE SPACE.
               10  FILLER    PIC X(20) VALUE '  CASE %     % /100K'.
               10  FILLER              PIC X(47) VALUE SPACE.
      *---------------------------------------------------------------*
           05  HEADING-LINE-3.
               10  FILLER    PIC X(20) VALUE '             |  %  0'.
               10  FILLER    PIC X(20) VALUE '----+----1----+----2'.
               10  FILLER    PIC X(20) VALUE '----+----3----+----4'.
               10  FILLER    PIC X(20) VALUE '----+----5----+----6'.
               10  FILLER    PIC X(20) VALUE '----+----7----+----8'.
               10  FILLER    PIC X(20) VALUE '----+----9----+----0'.
               10  FILLER    PIC X(10) VALUE '----+----1'.
      *---------------------------------------------------------------*
           05  ERROR-LINE-1.
               10  EL-TIMESTAMP.
                   15  EL-MONTH        PIC X(02).
                   15  FILLER          PIC X(01)  VALUE '/'.
                   15  EL-DAY          PIC X(02).
                   15  FILLER          PIC X(01)  VALUE '/'.
                   15  EL-YEAR         PIC X(04).
               10  FILLER    PIC X(06) VALUE '   |  '.
               10  EL-PERCENT.
                   15  EL-GRAPH-POINT  PIC ZZ9.999.
                   15  FILLER          PIC X(04) VALUE '%   '.
               10  FILLER    PIC X(11) VALUE '      ***  '.
               10  EL-CAUSE  PIC X(05).
               10  FILLER    PIC X(10) VALUE ' VALUE IS '.
               10  FILLER    PIC X(20) VALUE 'TO LARGE TO GRAPH MU'.
               10  FILLER    PIC X(20) VALUE 'ST BE LESS THAN 11% '.
               10  FILLER    PIC X(20) VALUE ' ***                '.
               10  FILLER    PIC X(13) VALUE '             '.
       COPY STATEFL.
      *---------------------------------------------------------------*
       01  SWITCHES-INDEX-COUNTER-FIELDS.
      *---------------------------------------------------------------*
           05  END-OF-FILE-SW              PIC X(01)  VALUE 'N'.
               88  END-OF-FILE                        VALUE 'Y'.
           05  VALID-RECORD-SW             PIC X(01)  VALUE 'Y'.
               88  VALID-RECORD                       VALUE 'Y'.
           05  WS-PERCENT                  PIC 99V9999999999.
           05  WS-C-GRAPH-PNT              PIC 999V999999.
           05  WS-D-GRAPH-PNT              PIC 999V9999999999.
           05  WS-GRAPH-PNT-X              PIC ZZ9.99999.
           05  WS-GRAPH-DATA               PIC 999.
           05  WS-PNT1                     PIC 99.
           05  WS-PNT2                     PIC 99.
           05  WS-PREV-STATE               PIC X(02).
       COPY PRINTCTL.
      *===============================================================*
       PROCEDURE DIVISION.
      *---------------------------------------------------------------*
       0000-MAIN-PROCESSING.
      *---------------------------------------------------------------*
           PERFORM 1000-OPEN-FILES.
           PERFORM 8000-READ-ST-HIST-FILE.
           IF      STR-STATE NOT = WS-PREV-STATE
               MOVE 999                    TO  LINE-COUNT
               MOVE STR-STATE              TO  WS-PREV-STATE.
           PERFORM 2000-PROCESS-ST-HIST-FILE
               UNTIL END-OF-FILE.
           PERFORM 3000-CLOSE-FILES.
           GOBACK.
      *---------------------------------------------------------------*
       1000-OPEN-FILES.
      *---------------------------------------------------------------*
           OPEN INPUT  ST-HIST-FILE
                OUTPUT PRINT-FILE.
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE-DATA.
           MOVE WS-CURRENT-YEAR            TO HL1-YEAR-OUT.
           MOVE WS-CURRENT-MONTH           TO HL1-MONTH-OUT.
           MOVE WS-CURRENT-DAY             TO HL1-DAY-OUT.
      *---------------------------------------------------------------*
       2000-PROCESS-ST-HIST-FILE.
      *---------------------------------------------------------------*
           IF  STR-STATE NOT = WS-PREV-STATE
               MOVE 999                    TO LINE-COUNT
               MOVE  STR-STATE             TO WS-PREV-STATE.
           MOVE STR-DAY                    TO PR-DAY.
           MOVE STR-MONTH                  TO PR-MONTH.
           MOVE STR-YEAR                   TO PR-YEAR.
           MOVE ALL SPACES                 TO STR-GRAPH.
           IF  STR-CASE-POSITIVE > ZERO
      *         COMPUTE WS-PERCENT = (STR-DEATH / 12700000)
      *         MULTIPLY WS-PERCENT     BY 100000 GIVING WS-D-GRAPH-PNT
               DIVIDE STR-DEATH     BY STR-CASE-POSITIVE
                  GIVING WS-PERCENT
               MULTIPLY WS-PERCENT     BY 100 GIVING WS-D-GRAPH-PNT
               DIVIDE STR-POSITIVE-INCREASE  BY STR-CASE-POSITIVE
                   GIVING WS-PERCENT
               MULTIPLY WS-PERCENT     BY 100 GIVING WS-C-GRAPH-PNT
           ELSE
               MOVE ZERO                   TO WS-C-GRAPH-PNT
                                              WS-D-GRAPH-PNT.
           COMPUTE WS-GRAPH-DATA = (WS-D-GRAPH-PNT * 10) + 6.
           IF  WS-D-GRAPH-PNT GREATER THAN 110 OR
               WS-C-GRAPH-PNT GREATER THAN 11
               MOVE STR-DAY                TO EL-DAY
               MOVE STR-MONTH              TO EL-MONTH
               MOVE STR-YEAR               TO EL-YEAR
               IF  WS-D-GRAPH-PNT GREATER THAN 110
                   MOVE WS-D-GRAPH-PNT     TO EL-GRAPH-POINT
                   MOVE 'DEATH'            TO EL-CAUSE
               ELSE
                   MOVE WS-C-GRAPH-PNT     TO EL-GRAPH-POINT
                   MOVE 'CASES'            TO EL-CAUSE
               END-IF
      *         MOVE '   ERROR  '           TO EL-PERCENT
               MOVE ERROR-LINE-1           TO NEXT-REPORT-LINE
           ELSE
               MOVE '+'              TO STR-GRAPH-DATA(WS-GRAPH-DATA)
               COMPUTE WS-GRAPH-DATA = (WS-C-GRAPH-PNT * 10) + 6
               MOVE '*'              TO STR-GRAPH-DATA(WS-GRAPH-DATA)
               MOVE 3                TO WS-PNT1
               PERFORM  2100-FORMAT-PERCENT
                   VARYING WS-PNT2 FROM 1 BY 1
                       UNTIL WS-PNT2 GREATER THAN 7
                   MOVE STR-PRINT-RECORD   TO NEXT-REPORT-LINE.

           PERFORM 9000-PRINT-REPORT-LINE.
           PERFORM 8000-READ-ST-HIST-FILE.
      *---------------------------------------------------------------*
       2100-FORMAT-PERCENT.
      *---------------------------------------------------------------*
           MOVE WS-C-GRAPH-PNT             TO WS-GRAPH-PNT-X.
           MOVE WS-GRAPH-PNT-X(WS-PNT2:1)  TO STR-GRAPH-DATA(WS-PNT1).
           ADD  1                          TO WS-PNT1.
           MOVE ' '                        TO STR-GRAPH-DATA(WS-PNT1).
      *---------------------------------------------------------------*
       3000-CLOSE-FILES.
      *---------------------------------------------------------------*
           CLOSE ST-HIST-FILE
                 PRINT-FILE.
      *---------------------------------------------------------------*
       8000-READ-ST-HIST-FILE.
      *---------------------------------------------------------------*
           READ ST-HIST-FILE
               AT END MOVE 'Y' TO END-OF-FILE-SW
                      MOVE 'N' TO VALID-RECORD-SW.
           IF VALID-RECORD
               UNSTRING STR-RECORD DELIMITED BY ','
               INTO STR-DATE
                   STR-STATE
                   STR-CASE-POSITIVE
                   STR-CASE-PROBOBALE
                   STR-CASE-NEGATIVE
                   STR-CASE-PENDING
                   STR-TOT-TEST-RES-SRC
                   STR-TOT-TEST-RESULTS
                   STR-HOSPITAL-CURR
                   STR-HOSPITAL-TOT
                   STR-ICU-CURR
                   STR-ICU-TOT
                   STR-VENT-CURR
                   STR-VENT-TOT
                   STR-RECOVERED
                   STR-DATA-GRADE
                   STR-DATE-UPDATED
                   STR-DATE-MODIFIED
                   STR-CHECK-TIME
                   STR-DEATH
                   STR-HOSPTALIZED
                   STR-CHECK-DATE
                   STR-TOT-TESTS-VIRAL
                   STR-POS-TESTS-VIRAL
                   STR-NEG-TESTS-VIRAL
                   STR-POS-CASES-VIRAL
                   STR-DEATH-CONFIRMED
                   STR-DEATH-PROBABLE
                   STR-TOT-TEST-ENCNTR-V
                   STR-TOT-TEST-PEOPLE-V
                   STR-TOT-TEST-ANTIBODY
                   STR-POS-TEST-ANTIBODY
                   STR-NEG-TEST-ANTIBODY
                   STR-TOT-TEST-ANTIBODY-P
                   STR-POS-TEST-ANTIBODY-P
                   STR-NEG-TEST-ANTIBODY-P
                   STR-TOT-TEST-ANTIGEN-P
                   STR-POS-TEST-ANTIGEN-P
                   STR-TOT-TEST-ANTIGEN
                   STR-POS-TEST-ANTIGEN
                   STR-FIPS-NUMBER
                   STR-POSITIVE-INCREASE
                   STR-NEGATIVE-INCREASE
                   STR-TOTAL
                   STR-TOT-TEST-INCREASE
                   STR-POS-NEG
                   STR-DEATH-INCREASE
                   STR-HOSPITAL-INCREASE
                   STR-HASH
                   STR-COMMERCIAL-SCORE
                   STR-NEG-REG-SCORE
                   STR-NEGATIVE-SCORE
                   STR-POSITIVE-SCORE
                   STR-SCORE
                   STR-GRADE
           ELSE
               MOVE 'Y' TO END-OF-FILE-SW.
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
           IF  STR-STATE  = '56'
               MOVE 'ALL'                  TO HL1-REPORTING-STATE
           ELSE
               MOVE STR-STATE              TO HL1-REPORTING-STATE.
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
           MOVE SPACE                      TO PRINT-LINE.
      *---------------------------------------------------------------*
       9120-WRITE-PRINT-LINE.
      *---------------------------------------------------------------*
           WRITE PRINT-RECORD
               AFTER ADVANCING LINE-SPACEING.
           ADD LINE-SPACEING               TO LINE-COUNT.
           MOVE 1                          TO LINE-SPACEING.
           MOVE SPACE                      TO PRINT-LINE.
