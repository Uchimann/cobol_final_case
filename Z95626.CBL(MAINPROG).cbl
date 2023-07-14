       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINPROG.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
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
           05 OUT-SPACE         PIC X(01) VALUE SPACE.
           05 OUT-RETURN-CODE   PIC 9(02).
           05 OUT-ACIKLAMA      PIC X(30).
           05 OUT-SPACE2        PIC X(01) VALUE SPACE.
           05 OUT-FNAME-FROM    PIC X(15).
           05 OUT-FNAME-TO      PIC X(15).
           05 OUT-LNAME-FROM    PIC X(15).
           05 OUT-LNAME-TO      PIC X(15).
       FD  INP-FILE RECORDING MODE F.
         01  INP-REC.
           07 INP-ISLEM-TIPI    PIC 9(01).
           07 INP-ID            PIC 9(5).
           07 INP-DVZ           PIC 9(3).
       WORKING-STORAGE SECTION.
         01  WS-WORK-AREA.
           05 WS-SUBPROG       PIC X(08)       VALUE 'SUBPROG'.
           05 INP-ST            PIC 9(02).
              88 INP-EOF                   VALUE 10.
              88 INP-SUCCES                VALUE 00 97.
           05 OUT-ST            PIC 9(02).
              88 OUT-SUCCESS               VALUE 00 97.
         01 WS-SUB-AREA.
              07 WS-ISLEM-TIPI     PIC 9(01).
                 88 WS-ISLEM-TIPI-VALID       VALUES 1 THRU 5.
              07 WS-SUB-FUNC    PIC 9(01).
                 88 WS-FUNC-OPEN           VALUE 1.
                 88 WS-FUNC-READ           VALUE 2.
                 88 WS-FUNC-UPDATE         VALUE 3.
                 88 WS-FUNC-WRITE          VALUE 4.
                 88 WS-FUNC-DELETE         VALUE 5.
                 88 WS-FUNC-CLOSE          VALUE 9.
      *       07 WS-SUB-ID      PIC 9(05).
      *        07 WS-SUB-DVZ     PIC 9(03).
      *        07 WS-SUB-RC      PIC 9(02).
              07 SUB-OUT-ISLEM-TIPI    PIC 9(01).
              07 SUB-OUT-ID            PIC 9(05).
              07 SUB-OUT-DVZ           PIC 9(03).
              07 SUB-OUT-RETURN-CODE   PIC 9(02).
              07 SUB-OUT-ACIKLAMA      PIC X(30).
              07 SUB-OUT-FNAME-FROM    PIC X(15).
              07 SUB-OUT-FNAME-TO      PIC X(15).
              07 SUB-OUT-LNAME-FROM    PIC X(15).
              07 SUB-OUT-LNAME-TO      PIC X(15).
              07 SUB-INP-ISLEM-TIPI    PIC 9(01).
              07 SUB-INP-ID            PIC 9(5).
              07 SUB-INP-DVZ           PIC 9(3).
      *SUBDATANIN ALT ELEMANLARI OLARAK OUTUN TÜM ALT ELEMANLARINI EKLE
      *VSAMDA TÜM OUTLARI SUBDATA YAPCAZ

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
       H100-END. EXIT.

       H200-PROCCES.
      *input dosyasIndaki verileri ws subdaki verilere move'la
           MOVE INP-ISLEM-TIPI  TO SUB-INP-ISLEM-TIPI
           MOVE INP-ID          TO SUB-INP-ID
           MOVE INP-DVZ         TO SUB-INP-DVZ
      *ISLLEMTIPI 5 ISE FUCKDOWNAYAZ
      *ILK HANESI 1 SE SET TRUE READ ISLEMLERI YAP
           IF INP-ISLEM-TIPI < 6
              DISPLAY 'INP ID' INP-ID 
              DISPLAY 'INP SUB ID' SUB-INP-ID 
              CALL WS-SUBPROG USING WS-SUB-AREA
           ELSE
              DISPLAY 'YAPAMAZSIN'
           END-IF.
      * SUB SUB DATADAN GELEN SEYLERI OUT A AT
           MOVE SUB-OUT-ISLEM-TIPI          TO   OUT-ISLEM-TIPI
           MOVE SUB-OUT-ID                  TO   OUT-ID
           MOVE SUB-OUT-DVZ                 TO   OUT-DVZ
           MOVE SUB-OUT-RETURN-CODE         TO   OUT-RETURN-CODE
           MOVE SUB-OUT-ACIKLAMA            TO   OUT-ACIKLAMA
           MOVE SUB-OUT-FNAME-FROM          TO   OUT-FNAME-FROM
           MOVE SUB-OUT-FNAME-TO            TO   OUT-FNAME-TO
           MOVE SUB-OUT-LNAME-FROM          TO   OUT-LNAME-FROM
           MOVE SUB-OUT-LNAME-TO            TO   OUT-LNAME-TO
           MOVE SUB-INP-ISLEM-TIPI          TO   INP-ISLEM-TIPI
           MOVE SUB-INP-ID                  TO   INP-ID
           MOVE SUB-INP-DVZ                 TO   INP-DVZ
           WRITE OUT-REC.
              READ INP-FILE.
       H200-END. EXIT.

       H999-PROGRAM-EXIT.
           CLOSE INP-FILE.
           CLOSE OUT-FILE.
           STOP RUN.
      *
