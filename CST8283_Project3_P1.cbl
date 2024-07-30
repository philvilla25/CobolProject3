       IDENTIFICATION DIVISION.
           PROGRAM-ID. SEQUENTIAL-TO-INDEXED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-PORTFOLIO
           ASSIGN TO '../PORTFOLIO.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-PORTFOLIO
           ASSIGN TO '../PORTFOLIO.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS STOCK-SYMBOL.
       DATA DIVISION.
       FILE SECTION.
       FD INPUT-PORTFOLIO.
       01 READ-PORTFOLIO.
           02 SSYMBOL PIC X(7).
           02 NSHARES PIC 9(5).
           02 ACOST PIC 9(4)V99.
       FD OUTPUT-PORTFOLIO.
       01 PORTFOLIO-RECORD.
           02 STOCK-SYMBOL PIC X(7).
           02 NUMBER-OF-SHARES PIC 9(5).
           02 AVERAGE-COST PIC 9(4)V99.
       WORKING-STORAGE SECTION.
       01 EOF PIC A.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM OPEN-FILES.
           PERFORM WRITE-INDEXED-FILE UNTIL EOF = 'Y'.
           PERFORM CLOSE-FILES.
           STOP RUN.
       OPEN-FILES.
           OPEN INPUT INPUT-PORTFOLIO.
           OPEN OUTPUT OUTPUT-PORTFOLIO.
       WRITE-INDEXED-FILE.
           READ INPUT-PORTFOLIO
               AT END MOVE 'Y' TO EOF
               NOT AT END
                   MOVE SSYMBOL TO STOCK-SYMBOL
                   MOVE NSHARES TO NUMBER-OF-SHARES
                   MOVE ACOST TO AVERAGE-COST
                   WRITE PORTFOLIO-RECORD
           END-READ.
       CLOSE-FILES.
           CLOSE INPUT-PORTFOLIO OUTPUT-PORTFOLIO.
       END PROGRAM SEQUENTIAL-TO-INDEXED.