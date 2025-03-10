      **********************************************************
      * PROGRAM: PROCESS-SIBS.CBL                              *
      * DESCRIPTION: PROCESS SIBS INFO                         *
      * DEVELOPER: FRANCISCO BORGES                            *
      **********************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROCESS-SIBS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SIBS-FILE ASSIGN TO '/home/kikos/ficheiros/ENT002'
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  SIBS-FILE.
       01  SIBS-RECORD.
           05 TRANSACTION-ID        PIC X(6).
           05 TRANSACTION-DATE      PIC X(8).
           05 TRANSACTION-TIME      PIC X(4).
           05 CARD-NUMBER           PIC X(16).
           05 TRANSACTION-AMOUNT    PIC 9(7)V99.
           05 TRANSACTION-CODE      PIC X(4).
           05 REGION-CODE           PIC X(1).
           05 TRANSACTION-STATUS    PIC X(10).

       WORKING-STORAGE SECTION.
       01  WS-END-OF-FILE           PIC X(1) VALUE 'N'.
       01  WS-REGION-DESC           PIC X(20).
       01  WS-FS-SIBS               PIC 9(2) VALUE ZERO.

       PROCEDURE DIVISION.
       
           OPEN INPUT SIBS-FILE.
           IF WS-FS-SIBS NOT = ZERO
               DISPLAY 'ERROR OPENING SIBS FILE' WS-FS-SIBS.

           PERFORM R100-READ-SIBS UNTIL WS-END-OF-FILE = 'S'.

           CLOSE SIBS-FILE
           IF WS-FS-SIBS NOT = ZERO
               DISPLAY 'ERROR CLOSING SIBS FILE' WS-FS-SIBS.

           STOP RUN.

       R100-READ-SIBS.
           READ SIBS-FILE INTO SIBS-RECORD
              AT END 
                  MOVE 'S' TO WS-END-OF-FILE
              NOT AT END 
                  DISPLAY 'Transaction ID: ' TRANSACTION-ID
                  DISPLAY 'Date: ' TRANSACTION-DATE 
                  DISPLAY 'Time: ' TRANSACTION-TIME
                  DISPLAY 'Card: ' CARD-NUMBER
                  DISPLAY 'Amount: €' TRANSACTION-AMOUNT
                  DISPLAY 'Type: ' TRANSACTION-CODE
                  PERFORM R200-REGION
                  DISPLAY 'Status: ' TRANSACTION-STATUS
                  DISPLAY '-------------------------------------------'.
       EXIT.

       R200-REGION.
           IF REGION-CODE = 'C' 
              MOVE 'Portugal Continental' TO WS-REGION-DESC
           ELSE IF REGION-CODE = 'A' 
              MOVE 'Acores' TO WS-REGION-DESC
           ELSE IF REGION-CODE = 'M'
              MOVE 'Madeira' TO WS-REGION-DESC
           ELSE
              MOVE 'Region unknown' TO WS-REGION-DESC
           END-IF.

           DISPLAY 'Region: ' WS-REGION-DESC.
           EXIT.
         