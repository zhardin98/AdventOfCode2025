       IDENTIFICATION DIVISION.
       PROGRAM-ID. GIFT-SHOP.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 3 2025.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2025 DAY 3 PROBLEM                     *
      * LINK: https://adventofcode.com/2025/day/3                   *
      *************************************************************** 
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    INPUT FILE
           SELECT INPUT-FILE ASSIGN TO 'INFILE.TXT'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 100 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD.
           05 IN-JOLTS                          PIC X(100).

       WORKING-STORAGE SECTION.
       01  WS-BEGIN                             PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.
          
       01  LRECL                                PIC 9(3)   VALUE 100.
              
       01  WS-EOF                               PIC X(1).
           88 EOF                                          VALUE 'Y'.
           88 NOT-EOF                                      VALUE 'N'.
       01  WS-HIGHEST-JOLTAGE                   PIC 9(2).
       01  WS-HIGHEST-JOLTAGE-A REDEFINES WS-HIGHEST-JOLTAGE.
           05 WS-HIGHEST-JOLTAGE-TENS           PIC 9(1).
           05 WS-HIGHEST-JOLTAGE-ONES           PIC 9(1).
       01  WS-REC-PTR                           PIC 9(3).
       01  OUT-SUM                              PIC 9(18)  VALUE 0.

       01  WS-END                               PIC X(25)
           VALUE 'WORKING STORAGE ENDS HERE'.

       PROCEDURE DIVISION.
      ***************************************************************
      * MAINLINE                                                    *
      *************************************************************** 
       0000-MAINLINE.
           
           PERFORM 1000-OPEN-FILE      THRU 1000-EXIT
           PERFORM 2000-PROCESS-INFILE THRU 2000-EXIT
               UNTIL EOF
           PERFORM 9000-CLOSE-FILE     THRU 9000-EXIT
           DISPLAY 'SUM: ' OUT-SUM
           .
       0000-EXIT.
           GOBACK.

      ****************************************************************
      * OPEN FILE                                                    *
      ****************************************************************
       1000-OPEN-FILE.

           OPEN INPUT INPUT-FILE
           MOVE SPACE TO WS-EOF
           .
       1000-EXIT.
           EXIT.

      ****************************************************************
      * PORT INPUT DATA                                              *
      ****************************************************************
       2000-PROCESS-INFILE.

      *    READ INFILE THRU EOF
           READ INPUT-FILE
               AT END
                   SET EOF TO TRUE
               NOT AT END
                   PERFORM 3000-EVALUATE-JOLTAGE THRU 3000-EXIT
           END-READ
           .
       2000-EXIT.
           EXIT.
          
      ****************************************************************
      * READ RECORD TO DETERMINE BEST JOLTAGE                        *
      ****************************************************************
       3000-EVALUATE-JOLTAGE.
           
           PERFORM 3100-GET-LEADING-DIGIT  THRU 3100-EXIT
           PERFORM 3200-GET-LESS-SIG-DIGIT THRU 3200-EXIT
           ADD WS-HIGHEST-JOLTAGE TO OUT-SUM
          .
       3000-EXIT.
           EXIT.

      ****************************************************************
      * GET HIGHEST POSSIBLE DIGIT (UNLESS IN FINAL COLUMN)          *
      ****************************************************************
       3100-GET-LEADING-DIGIT.

           MOVE 9 TO WS-HIGHEST-JOLTAGE-TENS
           
           PERFORM UNTIL WS-HIGHEST-JOLTAGE-TENS EQUAL 0
               MOVE 1 TO WS-REC-PTR
               PERFORM UNTIL WS-REC-PTR EQUAL LRECL
                   IF IN-JOLTS(WS-REC-PTR:1) EQUAL 
                                                 WS-HIGHEST-JOLTAGE-TENS
                       GO TO 3100-EXIT
                   END-IF
                   ADD 1 TO WS-REC-PTR
               END-PERFORM
               SUBTRACT 1 FROM WS-HIGHEST-JOLTAGE-TENS
           END-PERFORM  
      *    REACHING THESE INSTRUCTIONS SHOULD BE IMPOSSIBLE
           DISPLAY 'LEADING JOLT DIGIT REACHED ZERO'
           PERFORM 9999-ABEND THRU 9999-EXIT
           .
       3100-EXIT.
           EXIT.

      ****************************************************************
      * FIND HIGHEST LESS SIGNIFICANT DIGIT PAST THE HIGHEST DIGIT   *
      ****************************************************************
       3200-GET-LESS-SIG-DIGIT.

           ADD 1 TO WS-REC-PTR
           MOVE 0 TO WS-HIGHEST-JOLTAGE-ONES
           PERFORM UNTIL WS-REC-PTR GREATER LRECL
               IF IN-JOLTS(WS-REC-PTR:1) GREATER WS-HIGHEST-JOLTAGE-ONES
                   MOVE IN-JOLTS(WS-REC-PTR:1) 
                                              TO WS-HIGHEST-JOLTAGE-ONES
               END-IF
               ADD 1 TO WS-REC-PTR
           END-PERFORM
           .
       3200-EXIT.
           EXIT.

      ****************************************************************
      * CLOSE FILE                                                   *
      ****************************************************************
       9000-CLOSE-FILE.

           CLOSE INPUT-FILE
           .       
       9000-EXIT.
           EXIT.

      ****************************************************************
      * ABEND PARAGRAPH IF A FATAL ERROR IS FOUND                    *
      ****************************************************************
       9999-ABEND.
           
           DISPLAY 'ABENDING PROGRAM'
           STOP RUN
           .
       9999-EXIT.
           EXIT.