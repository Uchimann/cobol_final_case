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
           05 OUT-PRCSS-TYPE           PIC X(01).
           05 OUT-ID                   PIC 9(05).
           05 OUT-DVZ                  PIC 9(03).
           05 OUT-SPACE                PIC X(05).
           05 OUT-RETURN-CODE          PIC 9(02).
           05 FILLER                   PIC X(01).
           05 OUT-DESCRIPTION          PIC X(30).
           05 OUT-SPACE2               PIC X(01).
           05 OUT-FNAME-FROM           PIC X(15).
           05 OUT-FNAME-TO             PIC X(15).
           05 OUT-LNAME-FROM           PIC X(15).
           05 OUT-LNAME-TO             PIC X(15).
       FD  INP-FILE RECORDING MODE F.
         01  INP-REC.
           07 INP-PRCSS-TYPE           PIC X(01).
           07 INP-ID                   PIC 9(5).
           07 INP-DVZ                  PIC 9(3).
       WORKING-STORAGE SECTION.
         01  WS-WORK-AREA.
           05 WS-SUBPROG               PIC X(08)  VALUE 'SUBPROG'.
           05 INP-ST                   PIC 9(02).
              88 INP-EOF                          VALUE 10.
              88 INP-SUCCES                       VALUE 00 97.
           05 OUT-ST                   PIC 9(02).
              88 OUT-SUCCESS                      VALUE 00 97.
         01 WS-SUB-AREA.
              07 WS-PRCSS-TYPE         PIC X(01).
              07 WS-SUB-FUNC           PIC 9(01).
                 88 WS-FUNC-OPEN                  VALUE 1.
                 88 WS-FUNC-READ                  VALUE 2.
                 88 WS-FUNC-UPDATE                VALUE 3.
                 88 WS-FUNC-WRITE                 VALUE 4.
                 88 WS-FUNC-DELETE                VALUE 5.
                 88 WS-FUNC-CLOSE                 VALUE 9.
              07 SUB-OUT-PRCSS-TYPE    PIC X(01).
              07 SUB-OUT-ID            PIC 9(05).
              07 SUB-OUT-DVZ           PIC 9(03).
              07 SUB-OUT-RETURN-CODE   PIC 9(02).
              07 SUB-OUT-DESCRIPTION   PIC X(30).
              07 SUB-OUT-FNAME-FROM    PIC X(15).
              07 SUB-OUT-FNAME-TO      PIC X(15).
              07 SUB-OUT-LNAME-FROM    PIC X(15).
              07 SUB-OUT-LNAME-TO      PIC X(15).
              07 SUB-INP-PRCSS-TYPE    PIC X(01).
              07 SUB-INP-ID            PIC 9(5).
              07 SUB-INP-DVZ           PIC 9(3).
       PROCEDURE DIVISION.
      *-----------------------------------------------------------------
      *    MAIN: H100 ile input ve output dosyalarini aciyor.Basari ile
      *    acildiginda bir kayit okuyarak H200 paragrafina gidiyor.
      *    Dosyanin sonuna kadar H200 paragrafini calistirir. En son
      *    H999 ile dosyalari kapatir ve programi sonlandirir. 
      *    H100 paragrafinda dosyalar acilmaz ise program devam etmez.
      *----------------------------------------------------------------- 
       0000-MAIN.
           PERFORM H100-OPEN-FILES
           PERFORM H200-PROCCES UNTIL INP-EOF
           PERFORM H999-PROGRAM-EXIT.
       0000-END. EXIT.


      *-----------------------------------------------------------------
      *    H100:Dosya acma islemlerini yapar ve hata durumunu kontrol
      *    eder. Sonrasinda bir kayit okur. WS-FUNC-OPEN True olarak 
      *    isaretleyip paragrafi sonlandirir.
      *-----------------------------------------------------------------
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


      *-----------------------------------------------------------------
      *    H200:Okunan inputu sub input alanlarina tasir. Input degeri
      *    R,D,W veya U ise WS-SUB-AREA ile sub programı cagirir.
      *    Sub programda veri alan sub out ve sub ınp degiskenlerini
      *    Out alanlarina atarak Out dosyasina yazma islemi yapar.
      *    Cikarken bir kayit daha okur ve cagrildigi alana(MAIN'de 
      *    H200 satiri) geri doner.
      *-----------------------------------------------------------------
       H200-PROCCES.
           MOVE INP-PRCSS-TYPE              TO SUB-INP-PRCSS-TYPE
           MOVE INP-ID                      TO SUB-INP-ID
           MOVE INP-DVZ                     TO SUB-INP-DVZ

              IF INP-PRCSS-TYPE = 'R'
                 CALL WS-SUBPROG USING WS-SUB-AREA
              ELSE IF INP-PRCSS-TYPE = 'D'
                 CALL WS-SUBPROG USING WS-SUB-AREA
              ELSE IF INP-PRCSS-TYPE = 'W'
                 CALL WS-SUBPROG USING WS-SUB-AREA
              ELSE IF INP-PRCSS-TYPE = 'U'
                 CALL WS-SUBPROG USING WS-SUB-AREA
              ELSE
                 DISPLAY 'GECERSIZ ISLEM TIPI' INP-PRCSS-TYPE
              END-IF.

           MOVE SUB-OUT-PRCSS-TYPE          TO   OUT-PRCSS-TYPE
           MOVE SUB-OUT-ID                  TO   OUT-ID
           MOVE SUB-OUT-DVZ                 TO   OUT-DVZ
           MOVE SUB-OUT-RETURN-CODE         TO   OUT-RETURN-CODE
           MOVE SUB-OUT-DESCRIPTION         TO   OUT-DESCRIPTION
           MOVE SUB-OUT-FNAME-FROM          TO   OUT-FNAME-FROM
           MOVE SUB-OUT-FNAME-TO            TO   OUT-FNAME-TO
           MOVE SUB-OUT-LNAME-FROM          TO   OUT-LNAME-FROM
           MOVE SUB-OUT-LNAME-TO            TO   OUT-LNAME-TO
           MOVE SUB-INP-PRCSS-TYPE          TO   INP-PRCSS-TYPE
           MOVE SUB-INP-ID                  TO   INP-ID
           MOVE SUB-INP-DVZ                 TO   INP-DVZ
           MOVE ' RC: '                     TO   OUT-SPACE
           MOVE SPACE                       TO   OUT-SPACE2
           WRITE OUT-REC.
              READ INP-FILE.
       H200-END. EXIT.

      
      *-----------------------------------------------------------------
      *    H999: Input ve output dosyalarini kapatma islemi yapar. 
      *    Programi sonlandirir.
      *-----------------------------------------------------------------
       H999-PROGRAM-EXIT.
           CLOSE INP-FILE.
           CLOSE OUT-FILE.
           STOP RUN.
      *
