       IDENTIFICATION DIVISION.
       PROGRAM-ID. DNM.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IDX-FILE   ASSIGN TO IDXFILE
                             ORGANIZATION INDEXED
                             ACCESS MODE RANDOM
                             RECORD KEY IDX-KEY
                             STATUS IDX-ST.
           SELECT INP-FILE ASSIGN TO INPFILE
                             STATUS INP-ST.
           SELECT OUT-FILE   ASSIGN TO OUTFILE
                             STATUS OUT-ST.
       DATA DIVISION.
       FILE SECTION.
       FD  OUT-FILE RECORDING MODE F.
         01  OUT-REC.
           05 OUT-ISLEM-TIPI    PIC 9(01).
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
           03 INP-ISLEM-TIPI    PIC 9(01).
           03 INP-ID            PIC 9(5).
           03 INP-DVZ           PIC 9(3).
       FD  IDX-FILE.
         01  IDX-REC.
           03 IDX-KEY.
             05 IDX-ID          PIC S9(5) COMP-3.
             05 IDX-DVZ         PIC S9(3) COMP.
           03 IDX-NAME          PIC X(15).
           03 IDX-SRNAME        PIC X(15).
           03 IDX-DATE          PIC S9(7) COMP-3.
           03 IDX-BALANCE       PIC S9(15) COMP-3.
       WORKING-STORAGE SECTION.
         01  WS-WORK-AREA.
           05 WS-SUBPROG2       PIC X(08)       VALUE 'SUBPROG2'.
           05 INP-ST            PIC 9(02).
              88 INP-EOF                   VALUE 10.
              88 INP-SUCCES                VALUE 00 97.
           05 OUT-ST            PIC 9(02).
              88 OUT-SUCCESS               VALUE 00 97.
           05 IDX-ST            PIC 9(02).
              88 IDX-SUCCES                VALUE 00 97.
           05 WS-ISLEM-TIPI     PIC 9(01).
              88 WS-ISLEM-TIPI-VALID       VALUE 1 THRU 9.
           05 WS-SUB-AREA.
              07 WS-SUB-FUNC    PIC 9(01).
                 88 WS-FUNC-OPEN           VALUE 1.
                 88 WS-FUNC-READ           VALUE 2.
                 88 WS-FUNC-UPDATE         VALUE 3.
                 88 WS-FUNC-WRITE          VALUE 4.
                 88 WS-FUNC-DELETE         VALUE 5.
                 88 WS-FUNC-CLOSE          VALUE 9.
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
           OPEN I-O IDX-FILE.
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
           IF (IDX-ST NOT = 0) AND (IDX-ST NOT = 97)
           DISPLAY 'UNABLE TO OPEN IDXFILE: ' IDX-ST
           MOVE IDX-ST TO RETURN-CODE
           PERFORM H999-PROGRAM-EXIT
           END-IF.
           READ INP-FILE
           SET WS-FUNC-OPEN TO TRUE.
       H100-END. EXIT.

       H400-SUBPROG2.
           MOVE INP-ID TO IDX-ID.
           MOVE INP-DVZ TO IDX-DVZ.
           READ IDX-FILE KEY IS IDX-KEY
           INVALID KEY PERFORM H210-INVALID-KEY
           NOT INVALID KEY PERFORM H220-VALID-KEY.
       H400-END. EXIT.


       H200-PROCCES.
           PERFORM H400-SUBPROG2.
           READ INP-FILE.
       H200-END. EXIT.

       H700-UPDATE.
           DISPLAY 'ISMAIL'.
       H700-END. EXIT.
       
       H770-WRITE.
           DISPLAY 'WRITEKISMINDASIN'.
       H770-END. EXIT.

       H760-READ.
           MOVE INP-ISLEM-TIPI TO OUT-ISLEM-TIPI
           MOVE INP-ID         TO OUT-ID
           MOVE INP-DVZ        TO OUT-DVZ
           MOVE IDX-ST         TO OUT-RETURN-CODE
           STRING 'BASARILIOKUMAGERCEKLESTI RC:'IDX-ST
               DELIMITED BY SIZE INTO OUT-ACIKLAMA.
           MOVE IDX-NAME       TO OUT-FNAME-FROM
           MOVE SPACES         TO OUT-FNAME-TO
           MOVE IDX-SRNAME     TO OUT-LNAME-FORM
           MOVE SPACES         TO OUT-LNAME-TO.
           WRITE OUT-REC.
       H760-END. EXIT.

       H750-DELETE.
           MOVE INP-ISLEM-TIPI TO OUT-ISLEM-TIPI
           MOVE INP-ID         TO OUT-ID
           MOVE INP-DVZ        TO OUT-DVZ
           MOVE IDX-ST         TO OUT-RETURN-CODE
           MOVE IDX-NAME       TO OUT-FNAME-FROM
           MOVE SPACES         TO OUT-FNAME-TO
           MOVE IDX-SRNAME     TO OUT-LNAME-FORM
           MOVE SPACES         TO OUT-LNAME-TO.

             DELETE IDX-FILE RECORD
               NOT INVALID KEY
                 IF IDX-ST = 00
                      STRING 'BASARILISILMEGERCEKLESTI RC:'IDX-ST
                      DELIMITED BY SIZE INTO OUT-ACIKLAMA
                      DISPLAY 'BASARIILESILINDI'
                 ELSE
                      STRING 'BASARSIZSILMEGERCEKLESTI RC:'IDX-ST
                      DELIMITED BY SIZE INTO OUT-ACIKLAMA
                      DISPLAY 'HATAOLUSTU' IDX-ST
                 END-IF
             END-DELETE.

           WRITE OUT-REC.

       H750-END. EXIT.

       H210-INVALID-KEY.
           DISPLAY 'INVALID KEY, PLEASE CHECK IT : ' IDX-KEY.
       H210-END. EXIT.
      *
       H220-VALID-KEY.
      *ISLEM TIPI WRITE OLANI YAP
           MOVE INP-ISLEM-TIPI TO WS-ISLEM-TIPI
           IF WS-ISLEM-TIPI = 1
              COMPUTE WS-SUB-FUNC = 2
           ELSE IF WS-ISLEM-TIPI = 2
              COMPUTE WS-SUB-FUNC = 5
           ELSE IF WS-ISLEM-TIPI = 3
              COMPUTE WS-SUB-FUNC = 4
           END-IF.

           EVALUATE TRUE
              WHEN WS-FUNC-READ
                PERFORM H760-READ
              WHEN WS-FUNC-DELETE
                PERFORM H750-DELETE
              WHEN WS-FUNC-WRITE 
                 PERFORM H770-WRITE
              WHEN OTHER
                DISPLAY 'WHEN OTHER'
           END-EVALUATE.

      *    MOVE INP-ID           TO WS-SUB-ID.
      *    MOVE INP-DVZ          TO WS-SUB-DVZ.
      *    MOVE ZEROS            TO WS-SUB-RC.
      *    WRITE OUT-REC.
      *    READ INP-FILE.
       H220-END. EXIT.

       H999-PROGRAM-EXIT.
           CLOSE INP-FILE.
           CLOSE OUT-FILE.
           CLOSE IDX-FILE.
           STOP RUN.
      *