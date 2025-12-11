       IDENTIFICATION DIVISION.
       PROGRAM-ID. GIFT-SHOP.
       AUTHOR. ZACHARY HARDIN.
       DATE-WRITTEN. DEC 2 2025.
      ***************************************************************
      * PROGRAM TO SOLVE AOC 2025 DAY 2 PROBLEM                     *
      * LINK: https://adventofcode.com/2025/day/2                   *
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
           RECORD CONTAINS 500 CHARACTERS
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS INPUT-RECORD.
       01  INPUT-RECORD.
           05 IN-DIRECTION                      PIC X(500).

       WORKING-STORAGE SECTION.
       01  WS-BEGIN                             PIC X(27)
           VALUE 'WORKING STORAGE BEGINS HERE'.
       
       01  WS-IF-ARR.
           05  WS-ID-RANGE OCCURS 40 TIMES          PIC X(21).
       01  WS-RANGE-CNT                         PIC 9(2)   VALUE 0.
       01  WS-FIRST-ID                          PIC X(10).
       01  WS-SECOND-ID                         PIC X(10).
       01  WS-FIRST-ID-N                        PIC 9(10).
       01  WS-SECOND-ID-N                       PIC 9(10).
       01  WS-NUM-CNT                           PIC 9(2).
       01  WS-CURR-NUM                          PIC 9(10).
       01  WS-CURR-NUM-A                        PIC X(10).
       01  WS-START-NUM                         PIC 9(2).
       01  WS-NUM-LENGTH                        PIC 9(2).
       01  WS-HALF                              PIC 9(1).
       01  WS-FRS-VAL-START-POS                 PIC 9(2).
       01  WS-SEC-VAL-START-POS                 PIC 9(1).
       01  WS-NUM-FIRST-HALF                    PIC 9(5).
       01  WS-NUM-SECOND-HALF                   PIC 9(5).
       01  WS-LEADING-ZEROS                     PIC 9(2).
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
           PERFORM 3000-FIND-INVALIDS  THRU 3000-EXIT
               UNTIL WS-ID-RANGE(WS-RANGE-CNT) EQUAL SPACES
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
           .
       1000-EXIT.
           EXIT.

      ****************************************************************
      * PORT INPUT DATA                                              *
      ****************************************************************
       2000-PROCESS-INFILE.

           READ INPUT-FILE
           
           MOVE SPACES TO WS-IF-ARR

           UNSTRING INPUT-RECORD DELIMITED BY ','
               INTO WS-ID-RANGE(1)
                    WS-ID-RANGE(2)
                    WS-ID-RANGE(3)
                    WS-ID-RANGE(4)
                    WS-ID-RANGE(5)
                    WS-ID-RANGE(6)
                    WS-ID-RANGE(7)
                    WS-ID-RANGE(8)
                    WS-ID-RANGE(9)
                    WS-ID-RANGE(10)
                    WS-ID-RANGE(11)
                    WS-ID-RANGE(12)
                    WS-ID-RANGE(13)
                    WS-ID-RANGE(14)
                    WS-ID-RANGE(15)
                    WS-ID-RANGE(16)
                    WS-ID-RANGE(17)
                    WS-ID-RANGE(18)
                    WS-ID-RANGE(19)
                    WS-ID-RANGE(20)
                    WS-ID-RANGE(21)
                    WS-ID-RANGE(22)
                    WS-ID-RANGE(23)
                    WS-ID-RANGE(24)
                    WS-ID-RANGE(25)
                    WS-ID-RANGE(26)
                    WS-ID-RANGE(27)
                    WS-ID-RANGE(28)
                    WS-ID-RANGE(29)
                    WS-ID-RANGE(30)
                    WS-ID-RANGE(31)
                    WS-ID-RANGE(32)
                    WS-ID-RANGE(33)
                    WS-ID-RANGE(34)
                    WS-ID-RANGE(35)
                    WS-ID-RANGE(36)
                    WS-ID-RANGE(37)
                    WS-ID-RANGE(38)
                    WS-ID-RANGE(39)
                    WS-ID-RANGE(40)
           END-UNSTRING
           .
       2000-EXIT.
           EXIT.

      ****************************************************************
      * SLOT IDS INTO NUMERICS AND FIND THE INVALID IDS              *
      ****************************************************************
       3000-FIND-INVALIDS.
           
           ADD 1 TO WS-RANGE-CNT
           IF WS-ID-RANGE(WS-RANGE-CNT) EQUAL SPACES
               GO TO 3000-EXIT
           END-IF

           UNSTRING WS-ID-RANGE(WS-RANGE-CNT) DELIMITED BY '-'
               INTO WS-FIRST-ID WS-SECOND-ID               
           END-UNSTRING

      *    FIND LENGTH OF FIRST ID TO CONVERT IT TO NUMERIC
           MOVE 1 TO WS-NUM-CNT
           MOVE 1 TO WS-NUM-LENGTH
           PERFORM UNTIL WS-FIRST-ID(1:WS-NUM-CNT) NOT NUMERIC
               MOVE WS-FIRST-ID(1:WS-NUM-CNT) TO WS-FIRST-ID-N
               ADD 1 TO WS-NUM-CNT
           END-PERFORM

      *    FIND LENGTH OF SECOND ID TO CONVERT IT TO NUMERIC
           MOVE 1 TO WS-NUM-CNT
           MOVE 1 TO WS-NUM-LENGTH
           PERFORM UNTIL WS-SECOND-ID(1:WS-NUM-CNT) NOT NUMERIC
               MOVE WS-SECOND-ID(1:WS-NUM-CNT) TO WS-SECOND-ID-N
               ADD 1 TO WS-NUM-CNT
           END-PERFORM

           SUBTRACT 1 FROM WS-NUM-CNT GIVING WS-NUM-LENGTH
           MOVE WS-NUM-CNT                    TO WS-START-NUM
           

           MOVE WS-FIRST-ID-N TO WS-CURR-NUM
           PERFORM UNTIL WS-CURR-NUM GREATER WS-SECOND-ID-N
      *        REDETERMINE LENGTH IN CASE IT CHANGES DURING VALIDATION
               MOVE 0 TO WS-LEADING-ZEROS 
               INSPECT WS-CURR-NUM TALLYING WS-LEADING-ZEROS 
                   FOR LEADING '0'
               ADD 1 TO WS-LEADING-ZEROS GIVING WS-FRS-VAL-START-POS

      *        BYPASS ODD-LENGTHED NUMBERS
               IF WS-LEADING-ZEROS EQUAL 0 OR 2 OR 4 OR 6 OR 8 OR 10    
                   MOVE WS-CURR-NUM(WS-FRS-VAL-START-POS:WS-NUM-LENGTH)
                                                        TO WS-CURR-NUM-A
                   DIVIDE WS-NUM-LENGTH BY 2 GIVING WS-HALF
                   MOVE WS-CURR-NUM-A(1:WS-HALF) TO WS-NUM-FIRST-HALF
                   ADD 1 TO WS-HALF GIVING WS-SEC-VAL-START-POS
                   MOVE WS-CURR-NUM-A(WS-SEC-VAL-START-POS:WS-HALF) 
                                                   TO WS-NUM-SECOND-HALF 
                   IF WS-NUM-FIRST-HALF EQUAL WS-NUM-SECOND-HALF
                       ADD WS-CURR-NUM TO OUT-SUM
                   END-IF
               END-IF
               ADD 1 TO WS-CURR-NUM
           END-PERFORM
           .
       3000-EXIT.
           EXIT.

      ****************************************************************
      * CLOSE FILE                                                   *
      ****************************************************************
       9000-CLOSE-FILE.
           CLOSE INPUT-FILE
           .       
       9000-EXIT.
           EXIT.

