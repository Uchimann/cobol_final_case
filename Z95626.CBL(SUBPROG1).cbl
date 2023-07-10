       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBPROG1.
       AUTHOR      Ismail.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INP-FILE ASSIGN TO INPFILE
                           STATUS INP-ST.
           SELECT OUT-FILE ASSIGN TO OUTFILE
                           STATUS OUT-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  OUT-FILE RECORDING MODE F.
         01  OUT-REC.
           05 OUT-ISLEM-TIPI    PIC X(01).
           05 OUT-ID            PIC 9(05).
           05 OUT-DVZ           PIC 9(03).
           05 OUT-RETURN-CODE   PIC 9(02).
           05 OUT-ACIKLAMA      PIC X(30).
           05 OUT-FNAME-FROM    PIC X(15).
           05 OUT-FNAME-TO      PIC X(15).
           05 OUT-LNAME-FORM    PIC X(15).
           05 OUT-LNAME-TO      PIC X(15).

       FD  INP-FILE RECORDING MODE F.
         01  INP-REC.
           05 INP-ISLEM-TIPI    PIC X(01).
           05 INP-ID            PIC 9(05).
           05 INP-DVZ           PIC 9(03).
       WORKING-STORAGE SECTION.
         01  WS-WORK-AREA.
           05 WS-SUBPROG2       PIC X(08)       VALUE 'SUBPROG2'.
           05 INP-ST            PIC 9(02).
              88 INP-EOF                        VALUE 10.
              88 INP-SUCCES                     VALUE 00 97.
           05 OUT-ST            PIC 9(02).
              88 OUT-SUCCESS                    VALUE 00 97.
           05 WS-ISLEM-TIPI     PIC 9(01).
              88 WS-ISLEM-TIPI-VALID            VALUE 1 THRU 9.
           05 WS-SUB-AREA.
              07 WS-SUB-FUNC    PIC 9(01).
                 88 WS-FUNC-OPEN                VALUE 1.
                 88 WS-FUNC-READ                VALUE 2.
                 88 WS-FUNC-UPDATE              VALUE 3.
                 88 WS-FUNC-WRITE               VALUE 4.
                 88 WS-FUNC-CLOSE               VALUE 9.
              07 WS-SUB-ID      PIC 9(05).
              07 WS-SUB-DVZ     PIC 9(03).
              07 WS-SUB-RC      PIC 9(02).
              07 WS-SUB-DATA    PIC X(60).
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM H100-OPEN-FILES
           PERFORM H200-PROCCES UNTIL INP-EOF
           PERFORM H999-PROGRAM-EXIT.
       0000-END. EXIT.

       H100-OPEN-FILES.
           OPEN INPUT  INP-FILE.
           OPEN OUTPUT OUT-FILE.
           IF (INP-ST NOT = 0) AND (INP-ST NOT = 97)
           DISPLAY 'UNABLE TO OPEN INPFILE: ' INP-ST
           MOVE INP-ST TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           IF (OUT-ST NOT = 0) AND (OUT-ST NOT = 97)
           DISPLAY 'UNABLE TO OPEN OUTFILE: ' OUT-ST
           MOVE OUT-ST TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           READ INP-FILE
           SET WS-FUNC-OPEN TO TRUE.
           CALL WS-SUBPROG2 USING WS-SUB-AREA.
       H100-END. EXIT.

       H200-PROCCES.
           MOVE INP-ISLEM-TIPI TO WS-ISLEM-TIPI

           IF INP-ISLEM-TIPI = 'R'
              COMPUTE WS-ISLEM-TIPI = 2
           ELSE IF INP-ISLEM-TIPI = 'U'
              COMPUTE WS-ISLEM-TIPI = 3
           END-IF

           IF WS-ISLEM-TIPI-VALID
              EVALUATE WS-ISLEM-TIPI
                 WHEN 3
                   SET WS-FUNC-UPDATE TO TRUE
                 WHEN 4
                   SET WS-FUNC-WRITE TO TRUE
                 WHEN OTHER
                   SET WS-FUNC-READ TO TRUE
              END-EVALUATE
           END-IF

            MOVE INP-ID           TO WS-SUB-ID.
            MOVE INP-DVZ          TO WS-SUB-DVZ.
            MOVE ZEROS            TO WS-SUB-RC.
       H200-END. EXIT.

       H999-PROGRAM-EXIT.
           CLOSE INP-FILE.
           CLOSE OUT-FILE.
           STOP RUN.
      *
