       IDENTIFICATION DIVISION.
           PROGRAM-ID. PROJECT-2-UPDATED.       *> Program identifier and name

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PORTFOLIO-FILE                  *> Selecting the portfolio file
           ASSIGN TO '../PORTFOLIO.dat'           *> Assigning file path
           ORGANIZATION IS INDEXED                *> Defining the organization as indexed
           ACCESS MODE IS SEQUENTIAL              *> Accessing the file sequentially
           RECORD KEY IS PORTFOLIO-STOCK-SYMBOL.  *> Record key for indexing

           SELECT STOCKS-FILE                     *> Selecting the stocks file
           ASSIGN TO '../STOCKS.txt'              *> Assigning file path
           ORGANIZATION IS LINE SEQUENTIAL.       *> Defining the organization as line sequential

           SELECT REPORT-FILE                     *> Selecting the report file
           ASSIGN TO '../REPORT.txt'              *> Assigning file path
           ORGANIZATION IS LINE SEQUENTIAL.       *> Defining the organization as line sequential

       DATA DIVISION.
       FILE SECTION.
       FD PORTFOLIO-FILE.
       01 PORTFOLIO-RECORD.
           02 PORTFOLIO-STOCK-SYMBOL PIC X(7).    *> Stock symbol in the portfolio
           02 NUMBER-OF-SHARES PIC 9(5).          *> Number of shares in the portfolio
           02 AVERAGE-COST PIC 9(4)V99.           *> Average cost per share

       FD STOCKS-FILE.
       01 STOCKS-RECORD.
           02 SSYMBOL PIC X(7).                   *> Stock symbol
           02 SNAME PIC X(25).                    *> Stock name
           02 CPRICE PIC 9(4)V99.                 *> Closing price of the stock

       FD REPORT-FILE.
       01 REPORT-LINE PIC X(99).                  *> Report line structure

       WORKING-STORAGE SECTION.
       01 TABLE-INDEX PIC 9(2).                   *> Index for table operations
       01 EOF PIC A(1).                           *> End of file indicator
       COPY '../STOCKS-TABLE.dat'.                *> Copy statement for stock table data

       01 EQUAL-SIGNS PIC X(98) VALUE ALL '='.    *> Line of equal signs for report formatting
       01 REPORT-HEADER.
           02 STOCK-NAME-HEADER PIC X(10) VALUE 'STOCK NAME'. *> Header for stock name
           02 FILLER PIC X(17) VALUE SPACES.      *> Spacer
           02 SHARES PIC X(7) VALUE '#SHARES'.    *> Header for number of shares
           02 FILLER PIC X(3) VALUE SPACES.       *> Spacer
           02 UNIT-COST PIC X(9) VALUE 'UNIT COST'. *> Header for unit cost
           02 FILLER PIC X(2) VALUE SPACES.       *> Spacer
           02 AT-CLOSING PIC X(10) VALUE 'AT CLOSING'. *> Header for closing price
           02 FILLER PIC X(4) VALUE SPACES.       *> Spacer
           02 COST-BASE PIC X(9) VALUE 'COST BASE'. *> Header for cost base
           02 FILLER PIC X(2) VALUE SPACES.       *> Spacer
           02 MARKET-VALUE-HEADER PIC X(12) VALUE 'MARKET VALUE'. *> Header for market value
           02 FILLER PIC X(4) VALUE SPACES.       *> Spacer
           02 GAIN-OR-LOSS PIC X(9) VALUE 'GAIN/LOSS'. *> Header for gain or loss

       01 READ-COUNTER PIC 9(2).                  *> Counter for records read
       01 WRITE-COUNTER PIC 9(2).                 *> Counter for records written
       01 FOUND-FLAG PIC A(1).                    *> Flag to indicate if stock is found
       01 ADJUSTED-COST-BASE PIC 9(5)V99.         *> Adjusted cost base calculation
       01 MARKET-VALUE PIC 9(5)V99.               *> Market value calculation
       01 TOTAL-GAIN-OR-LOSS PIC 9(5)V99-.        *> Total gain or loss calculation
       01 REPORT-RECORD.
           02 REPORT-STOCK-NAME PIC X(25).        *> Stock name in the report
           02 FILLER PIC X(3) VALUE SPACES.       *> Spacer
           02 REPORT-NUMBER-OF-SHARES PIC ZZ,ZZ9. *> Number of shares in the report
           02 FILLER PIC X(3) VALUE SPACES.       *> Spacer
           02 REPORT-AVERAGE-COST PIC $$,$$9.99.  *> Average cost in the report
           02 FILLER PIC X(3) VALUE SPACES.       *> Spacer
           02 REPORT-CLOSING-PRICE PIC $$,$$9.99. *> Closing price in the report
           02 FILLER PIC X(3) VALUE SPACES.       *> Spacer
           02 REPORT-ADJUSTED-COST-BASE PIC $$$,$$9.99. *> Adjusted cost base in the report
           02 FILLER PIC X(4) VALUE SPACES.       *> Spacer
           02 REPORT-MARKET-VALUE PIC $$$,$$9.99. *> Market value in the report
           02 FILLER PIC X(2) VALUE SPACES.       *> Spacer
           02 REPORT-TOTAL-GAIN-OR-LOSS PIC $$$$,$$9.99-. *> Total gain or loss in the report

       01 REPORT-COUNTERS.
           02 RECORDS-READ PIC X(14) VALUE 'Records read: '. *> Label for records read
           02 REPORT-READ-COUNTER PIC Z9.         *> Counter for records read
           02 FILLER PIC X(3) VALUE SPACES.       *> Spacer
           02 RECORDS-WRITTEN PIC X(17) VALUE 'Records written: '. *> Label for records written
           02 REPORT-WRITE-COUNTER PIC Z9.        *> Counter for records written

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZATION-RTN.            *> Perform initialization routine
           PERFORM PROCESS-RTN UNTIL EOF = 'Y'.   *> Process records until EOF
           PERFORM CLOSE-RTN.                     *> Perform close routine
           STOP RUN.                              *> End program

       INITIALIZATION-RTN.
           PERFORM OPEN-FILES.                    *> Open files
           PERFORM LOAD-TABLE VARYING TABLE-INDEX FROM 1 BY 1
               UNTIL EOF = 'Y' OR TABLE-INDEX > 20. *> Load stock data into table
           PERFORM SET-EOF.                       *> Set EOF indicator
           PERFORM WRITE-REPORT-HEADER.           *> Write report header

       OPEN-FILES.
           OPEN INPUT PORTFOLIO-FILE STOCKS-FILE. *> Open input files
           OPEN OUTPUT REPORT-FILE.               *> Open output file

       LOAD-TABLE.
           PERFORM READ-STOCKS-FILE.              *> Read stocks file
           PERFORM LOAD-DATA.                     *> Load data into table

       READ-STOCKS-FILE.
           READ STOCKS-FILE
               AT END MOVE 'Y' TO EOF             *> Set EOF when end of file is reached
           END-READ.

       LOAD-DATA.
           IF EOF NOT = 'Y'
               MOVE SSYMBOL TO STOCK-SYMBOL(TABLE-INDEX) *> Load stock symbol
               MOVE SNAME TO STOCK-NAME(TABLE-INDEX)     *> Load stock name
               MOVE CPRICE TO CLOSING-PRICE(TABLE-INDEX) *> Load closing price
           END-IF.

       SET-EOF.
           MOVE 'N' TO EOF.                       *> Reset EOF indicator

       WRITE-REPORT-HEADER.
           MOVE EQUAL-SIGNS TO REPORT-LINE.       *> Write line of equal signs
           WRITE REPORT-LINE.
           MOVE REPORT-HEADER TO REPORT-LINE.     *> Write report header
           WRITE REPORT-LINE.
           MOVE EQUAL-SIGNS TO REPORT-LINE.       *> Write another line of equal signs
           WRITE REPORT-LINE.

       PROCESS-RTN.
           PERFORM READ-PORTFOLIO-FILE.           *> Read portfolio file
           IF EOF NOT = 'Y'
               PERFORM SET-FOUND-FLAG             *> Set found flag
               PERFORM SEARCH-RTN VARYING TABLE-INDEX FROM 1 BY 1
                   UNTIL FOUND-FLAG = 'Y' OR TABLE-INDEX > 20 *> Search for stock in table
           END-IF.

       READ-PORTFOLIO-FILE.
           READ PORTFOLIO-FILE
               AT END MOVE 'Y' TO EOF             *> Set EOF when end of file is reached
               NOT AT END ADD 1 TO READ-COUNTER   *> Increment read counter
           END-READ.

       SET-FOUND-FLAG.
           MOVE 'N' TO FOUND-FLAG.                *> Reset found flag

       SEARCH-RTN.
           IF PORTFOLIO-STOCK-SYMBOL = STOCK-SYMBOL(TABLE-INDEX)
               MOVE 'Y' TO FOUND-FLAG                  *> Set found flag
               MOVE STOCK-NAME(TABLE-INDEX) TO REPORT-STOCK-NAME *> Move stock name to report
               MOVE NUMBER-OF-SHARES TO REPORT-NUMBER-OF-SHARES *> Move number of shares to report
               MOVE AVERAGE-COST TO REPORT-AVERAGE-COST *> Move average cost to report
               MOVE CLOSING-PRICE(TABLE-INDEX) TO REPORT-CLOSING-PRICE
               *> Move closing price to report
               MULTIPLY NUMBER-OF-SHARES BY AVERAGE-COST
                   GIVING ADJUSTED-COST-BASE      *> Calculate adjusted cost base
               MOVE ADJUSTED-COST-BASE TO REPORT-ADJUSTED-COST-BASE *> Move adjusted cost base to report
               CALL 'CALCULATE' USING
                 BY CONTENT NUMBER-OF-SHARES CLOSING-PRICE(TABLE-INDEX)
                 ADJUSTED-COST-BASE BY REFERENCE MARKET-VALUE
                 TOTAL-GAIN-OR-LOSS               *> Call calculate subroutine
               MOVE MARKET-VALUE TO REPORT-MARKET-VALUE *> Move market value to report
               MOVE TOTAL-GAIN-OR-LOSS TO REPORT-TOTAL-GAIN-OR-LOSS *> Move total gain/loss to report
               MOVE REPORT-RECORD TO REPORT-LINE
               WRITE REPORT-LINE                   *> Write report line
               ADD 1 TO WRITE-COUNTER             *> Increment write counter
           END-IF.

       CLOSE-RTN.
           PERFORM WRITE-REPORT-COUNTERS.         *> Write report counters
           PERFORM CLOSE-FILES.                   *> Close files

       WRITE-REPORT-COUNTERS.
           MOVE EQUAL-SIGNS TO REPORT-LINE.       *> Write line of equal signs
           WRITE REPORT-LINE.
           MOVE READ-COUNTER TO REPORT-READ-COUNTER. *> Move read counter to report
           MOVE WRITE-COUNTER TO REPORT-WRITE-COUNTER. *> Move write counter to report
           MOVE REPORT-COUNTERS TO REPORT-LINE.   *> Write report counters
           WRITE REPORT-LINE.

       CLOSE-FILES.
           CLOSE PORTFOLIO-FILE STOCKS-FILE REPORT-FILE. *> Close all files
       END PROGRAM PROJECT-2-UPDATED.
