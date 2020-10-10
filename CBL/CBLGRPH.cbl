       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLGRPH.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USA-HIST-FILE ASSIGN TO USAHIST.
           SELECT PRINT-FILE    ASSIGN TO PRTLINE.
      *===============================================================*
       DATA DIVISION.
      *---------------------------------------------------------------*
       FILE SECTION.
       FD  USA-HIST-FILE
               RECORDING MODE IS F.
       01  UHR-RECORD                PIC X(130).
      *
      *01  UHR-RECORD.
      *    05  UHR-DATE.
      *        10  UHR-YEAR            PIC X(04).
      *        10  UHR-MONTH           PIC X(02).
      *        10  UHR-DAY             PIC X(02).
      *    05  UHR-STATE               PIC X(02).
      *    05  UHR-CASE-POSITIVE       PIC 9(07).
      *    05  UHR-CASE-NEGATIVE       PIC 9(07).
      *    05  UHR-CASE-PENDING        PIC 9(07).
      *    05  UHR-CASE-NEW            PIC 9(07).
      *    05  UHR-HOSPITAL-CURR       PIC 9(07).
      *    05  UHR-HOSPITAL-TOT        PIC 9(07).
      *    05  UHR-ICU-CURR            PIC 9(07).
      *    05  UHR-ICU-TOT             PIC 9(07).
      *    05  UHR-VENT-CURR           PIC 9(07).
      *    05  UHR-VENT-TOT            PIC 9(07).
      *    05  UHR-RECOVERED           PIC 9(07).
      *    05  UHR-DEATH               PIC 9(06).
      *    05  UHR-DEATH-NEW           PIC 9(06).
      *    05  UHR-D-PERCENT           PIC 99V99.
      *    05  UHR-C-PERCENT           PIC 99V99.
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
           05  UHR-PRINT-RECORD.
               10  UHR-TIMESTAMP.
                   15  PR-MONTH        PIC X(02).
                   15  FILLER          PIC X(01)  VALUE '/'.
                   15  PR-DAY          PIC X(02).
                   15  FILLER          PIC X(01)  VALUE '/'.
                   15  PR-YEAR         PIC X(04).
               10  FILLER              PIC X(02)  VALUE SPACE.
               10  FILLER              PIC X(02)  VALUE ' |'.
               10  UHR-GRAPH.
                   15  UHR-GRAPH-DATA  PIC X(01) OCCURS 110 TIMES.
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
               10  FILLER    PIC X(20) VALUE '   CASES     % /100K'.
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
      *---------------------------------------------------------------*
       01  WS-HOLD-FIELDS.
      *---------------------------------------------------------------*
           05  WS-UHR-RECORD.
               10  WS-UHR-DATE.
                   15  WS-UHR-YEAR         PIC X(04).
                   15  WS-UHR-MONTH        PIC X(02).
                   15  WS-UHR-DAY          PIC X(02).
               10  WS-UHR-STATE            PIC X(02).
               10  WS-UHR-CASE-POSITIVE    PIC 9(07).
               10  WS-UHR-CASE-NEGATIVE    PIC 9(07).
               10  WS-UHR-CASE-PENDING     PIC 9(07).
               10  WS-UHR-CASE-NEW         PIC 9(07).
               10  WS-UHR-HOSPITAL-CURR    PIC 9(07).
               10  WS-UHR-HOSPITAL-TOT     PIC 9(07).
               10  WS-UHR-ICU-CURR         PIC 9(07).
               10  WS-UHR-ICU-TOT          PIC 9(07).
               10  WS-UHR-VENT-CURR        PIC 9(07).
               10  WS-UHR-VENT-TOT         PIC 9(07).
               10  WS-UHR-RECOVERED        PIC 9(07).
               10  WS-UHR-DEATH            PIC 9(07).
               10  WS-UHR-DEATH-NEW        PIC 9(07).
               10  WS-UHR-D-PERCENT        PIC X(06).
               10  WS-UHR-C-PERCENT        PIC X(06).
      *---------------------------------------------------------------*
           05  WS-PERCENT                  PIC 99V9999999999.
           05  WS-C-GRAPH-PNT              PIC 999V999999.
           05  WS-D-GRAPH-PNT              PIC 999V9999999999.
           05  WS-GRAPH-PNT-X              PIC ZZ9.99999.
           05  WS-GRAPH-DATA               PIC 999.
           05  WS-PNT1                     PIC 99.
           05  WS-PNT2                     PIC 99.
           05  WS-PREV-STATE               PIC X(02).
           05  TODAYS-DATE.
               10  TD-YEAR                 PIC 99.
               10  TD-MONTH                PIC 99.
               10  TD-DAY                  PIC 99.
           05  END-OF-FILE-SW              PIC X(01)  VALUE 'N'.
               88  END-OF-FILE                        VALUE 'Y'.
           05  VALID-RECORD-SW             PIC X(01)  VALUE 'Y'.
               88  VALID-RECORD                       VALUE 'Y'.
      *---------------------------------------------------------------*
       01  PRINTER-CONTROL-FIELDS.
      *---------------------------------------------------------------*
           05  LINE-SPACEING               PIC 9(02)  VALUE 1.
           05  LINE-COUNT                  PIC 9(03)  VALUE 999.
           05  LINES-ON-PAGE               PIC 9(03)  VALUE 60.
           05  PAGE-COUNT                  PIC 9(03)  VALUE 1.
           05  TOP-OF-PAGE                 PIC X      VALUE '1'.
           05  SINGLE-SPACE                PIC X      VALUE ' '.
           05  DOUBLE-SPACE                PIC X      VALUE '0'.
           05  TRIPLE-SPACE                PIC X      VALUE '-'.
           05  OVERPRINT                   PIC X      VALUE '+'.
      *===============================================================*
       PROCEDURE DIVISION.
      *---------------------------------------------------------------*
       0000-MAIN-PROCESSING.
      *---------------------------------------------------------------*
           PERFORM 1000-OPEN-FILES.
           PERFORM 8000-READ-USA-HIST-FILE.
           IF  WS-UHR-STATE NOT = WS-PREV-STATE
               MOVE 999                    TO  LINE-COUNT
               MOVE  WS-UHR-STATE          TO  WS-PREV-STATE.
           PERFORM 2000-PROCESS-USA-HIST-FILE
               UNTIL END-OF-FILE.
           PERFORM 3000-CLOSE-FILES.
           GOBACK.
      *---------------------------------------------------------------*
       1000-OPEN-FILES.
      *---------------------------------------------------------------*
           OPEN INPUT  USA-HIST-FILE
                OUTPUT PRINT-FILE.
           ACCEPT TODAYS-DATE FROM DATE.
           MOVE TD-YEAR                    TO HL1-YEAR-OUT.
           MOVE TD-MONTH                   TO HL1-MONTH-OUT.
           MOVE TD-DAY                     TO HL1-DAY-OUT.
      *---------------------------------------------------------------*
       2000-PROCESS-USA-HIST-FILE.
      *---------------------------------------------------------------*
           IF  WS-UHR-STATE NOT = WS-PREV-STATE
               MOVE 999                    TO LINE-COUNT
               MOVE  WS-UHR-STATE          TO WS-PREV-STATE.
           MOVE WS-UHR-DAY                 TO PR-DAY.
           MOVE WS-UHR-MONTH               TO PR-MONTH.
           MOVE WS-UHR-YEAR                TO PR-YEAR.
           MOVE ALL SPACES                 TO UHR-GRAPH.
           IF  WS-UHR-CASE-POSITIVE > ZERO
      *         DIVIDE WS-UHR-DEATH     BY WS-UHR-CASE-POSITIVE
      *            GIVING WS-PERCENT
               COMPUTE WS-PERCENT = (WS-UHR-DEATH / 331000000)
               MULTIPLY WS-PERCENT     BY 100000 GIVING WS-D-GRAPH-PNT
      *         MULTIPLY WS-PERCENT     BY 100 GIVING WS-D-GRAPH-PNT
               DIVIDE WS-UHR-CASE-NEW  BY WS-UHR-CASE-POSITIVE
                   GIVING WS-PERCENT
               MULTIPLY WS-PERCENT     BY 100 GIVING WS-C-GRAPH-PNT
           ELSE
               MOVE ZERO                   TO WS-C-GRAPH-PNT
                                              WS-D-GRAPH-PNT.
           COMPUTE WS-GRAPH-DATA = (WS-D-GRAPH-PNT * 1) + 6.
           IF  WS-D-GRAPH-PNT GREATER THAN 110 OR
               WS-C-GRAPH-PNT GREATER THAN 11
               MOVE WS-UHR-DAY             TO EL-DAY
               MOVE WS-UHR-MONTH           TO EL-MONTH
               MOVE WS-UHR-YEAR            TO EL-YEAR
               IF  WS-D-GRAPH-PNT GREATER THAN 110
                   MOVE WS-D-GRAPH-PNT     TO EL-GRAPH-POINT
                   MOVE 'DEATH'            TO EL-CAUSE
               ELSE
                   MOVE WS-C-GRAPH-PNT     TO EL-GRAPH-POINT
                   MOVE 'CASES'            TO EL-CAUSE
               END-IF
      *         MOVE '   ERROR  '       TO EL-PERCENT
               MOVE ERROR-LINE-1           TO NEXT-REPORT-LINE
           ELSE
               MOVE '+'                 TO UHR-GRAPH-DATA(WS-GRAPH-DATA)
               COMPUTE WS-GRAPH-DATA = (WS-C-GRAPH-PNT * 10) + 6
               MOVE '*'                 TO UHR-GRAPH-DATA(WS-GRAPH-DATA)
               MOVE 3                      TO WS-PNT1
               PERFORM  2100-FORMAT-PERCENT
                   VARYING WS-PNT2 FROM 1 BY 1
                       UNTIL WS-PNT2 GREATER THAN 7
               MOVE UHR-PRINT-RECORD       TO NEXT-REPORT-LINE.
           PERFORM 9000-PRINT-REPORT-LINE.
           PERFORM 8000-READ-USA-HIST-FILE.
      *--------------------------------------------------------------*
       2100-FORMAT-PERCENT.
      *---------------------------------------------------------------*
           MOVE WS-D-GRAPH-PNT             TO WS-GRAPH-PNT-X.
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
               UNSTRING UHR-RECORD DELIMITED BY ','
               INTO  WS-UHR-DATE
                     WS-UHR-STATE
                     WS-UHR-CASE-POSITIVE
                     WS-UHR-CASE-NEGATIVE
                     WS-UHR-CASE-PENDING
                     WS-UHR-CASE-NEW
                     WS-UHR-HOSPITAL-CURR
                     WS-UHR-HOSPITAL-TOT
                     WS-UHR-ICU-CURR
                     WS-UHR-ICU-TOT
                     WS-UHR-VENT-CURR
                     WS-UHR-VENT-TOT
                     WS-UHR-RECOVERED
                     WS-UHR-DEATH
                     WS-UHR-DEATH-NEW
                     WS-UHR-D-PERCENT
                     WS-UHR-C-PERCENT
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
           IF  WS-UHR-STATE  = '56'
               MOVE 'ALL'                  TO HL1-REPORTING-STATE
           ELSE
               MOVE WS-UHR-STATE           TO HL1-REPORTING-STATE.
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
