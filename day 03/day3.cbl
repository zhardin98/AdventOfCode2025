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
       01  WS-HIGHEST-JOLTAGE                   PIC 9(12)  VALUE 0.
       01  WS-MULTIPLIER                        PIC 9(12).
       01  WS-HIGHEST-DIGIT                     PIC 9(1).
       01  WS-REC-START-POS                     PIC 9(3).
       01  WS-REC-PTR-1                         PIC 9(3).
       01  WS-REC-PTR-2                         PIC 9(3).
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
           
           MOVE 0            TO WS-HIGHEST-JOLTAGE
           MOVE 100000000000 TO WS-MULTIPLIER
           MOVE 1            TO WS-REC-START-POS
           SUBTRACT 11 FROM LRECL GIVING WS-REC-PTR-2
           PERFORM UNTIL WS-MULTIPLIER EQUAL 0
               PERFORM 3100-GET-HIGHEST-DIGIT THRU 3100-EXIT
               COMPUTE WS-HIGHEST-JOLTAGE = 
                 WS-HIGHEST-JOLTAGE + (WS-HIGHEST-DIGIT * WS-MULTIPLIER)
               DIVIDE 10 INTO WS-MULTIPLIER
               ADD 1 TO WS-REC-PTR-1 GIVING  WS-REC-START-POS
               ADD 1 TO WS-REC-PTR-2
           END-PERFORM
           ADD WS-HIGHEST-JOLTAGE TO OUT-SUM
          .
       3000-EXIT.
           EXIT.

      ****************************************************************
      * GET HIGHEST POSSIBLE DIGIT IN GIVEN POINTER RANGE            *
      ****************************************************************
       3100-GET-HIGHEST-DIGIT.

           MOVE 9 TO WS-HIGHEST-DIGIT
           MOVE WS-REC-START-POS TO WS-REC-PTR-1
           PERFORM UNTIL WS-HIGHEST-DIGIT EQUAL 0
               PERFORM UNTIL WS-REC-PTR-1 GREATER WS-REC-PTR-2
                   IF IN-JOLTS(WS-REC-PTR-1:1) EQUAL WS-HIGHEST-DIGIT
                       GO TO 3100-EXIT
                   END-IF
                   ADD 1 TO WS-REC-PTR-1
               END-PERFORM 
               SUBTRACT 1 FROM WS-HIGHEST-DIGIT 
               MOVE WS-REC-START-POS TO WS-REC-PTR-1
           END-PERFORM  
      *    REACHING THESE INSTRUCTIONS SHOULD BE IMPOSSIBLE
           DISPLAY 'LEADING JOLT DIGIT REACHED ZERO'
           PERFORM 9999-ABEND THRU 9999-EXIT
           .
       3100-EXIT.
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