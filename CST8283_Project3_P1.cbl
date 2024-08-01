       IDENTIFICATION DIVISION.
           PROGRAM-ID. SEQUENTIAL-TO-INDEXED.     *> Program identifier and name

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-PORTFOLIO                 *> Selecting the input portfolio file
           ASSIGN TO '../PORTFOLIO.txt'           *> Assigning file path
           ORGANIZATION IS LINE SEQUENTIAL.       *> Defining the organization as line sequential

           SELECT OUTPUT-PORTFOLIO                *> Selecting the output portfolio file
           ASSIGN TO '../PORTFOLIO.dat'           *> Assigning file path
           ORGANIZATION IS INDEXED                *> Defining the organization as indexed
           ACCESS MODE IS SEQUENTIAL              *> Accessing the file sequentially
           RECORD KEY IS STOCK-SYMBOL.            *> Record key for indexing

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-PORTFOLIO.
       01 READ-PORTFOLIO.
           02 SSYMBOL PIC X(7).                   *> Stock symbol in the input file
           02 NSHARES PIC 9(5).                   *> Number of shares in the input file
           02 ACOST PIC 9(4)V99.                  *> Average cost in the input file

       FD OUTPUT-PORTFOLIO.
       01 PORTFOLIO-RECORD.
           02 STOCK-SYMBOL PIC X(7).              *> Stock symbol in the output file
           02 NUMBER-OF-SHARES PIC 9(5).          *> Number of shares in the output file
           02 AVERAGE-COST PIC 9(4)V99.           *> Average cost in the output file

       WORKING-STORAGE SECTION.
       01 EOF PIC A.                              *> End of file indicator

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM OPEN-FILES.                    *> Open files
           PERFORM WRITE-INDEXED-FILE UNTIL EOF = 'Y'. *> Write records to indexed file until EOF
           PERFORM CLOSE-FILES.                   *> Close files
           STOP RUN.                              *> End program

       OPEN-FILES.
           OPEN INPUT INPUT-PORTFOLIO.            *> Open input file
           OPEN OUTPUT OUTPUT-PORTFOLIO.          *> Open output file

       WRITE-INDEXED-FILE.
           READ INPUT-PORTFOLIO
               AT END MOVE 'Y' TO EOF             *> Set EOF when end of file is reached
               NOT AT END
                   MOVE SSYMBOL TO STOCK-SYMBOL   *> Move stock symbol to output record
                   MOVE NSHARES TO NUMBER-OF-SHARES *> Move number of shares to output record
                   MOVE ACOST TO AVERAGE-COST     *> Move average cost to output record
                   WRITE PORTFOLIO-RECORD         *> Write output record
           END-READ.

       CLOSE-FILES.
           CLOSE INPUT-PORTFOLIO OUTPUT-PORTFOLIO. *> Close all files

       END PROGRAM SEQUENTIAL-TO-INDEXED.
