       IDENTIFICATION DIVISION.
           PROGRAM-ID. BUY-STOCKS.
           *> This program manages the buying of stocks and updates the portfolio
           
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PORTFOLIO-FILE
           ASSIGN TO '../PORTFOLIO.dat'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS PORTFOLIO-STOCK-SYMBOL.
           
           SELECT STOCKS-FILE
           ASSIGN TO '../STOCKS.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
       FD PORTFOLIO-FILE.
       01 PORTFOLIO-RECORD.
           *> Structure of the portfolio record.
           02 PORTFOLIO-STOCK-SYMBOL PIC X(7).
           02 NUMBER-OF-SHARES PIC 9(5).
           02 AVERAGE-COST PIC 9(4)V99.
           
           *> Structure of the stocks record.
       FD STOCKS-FILE.
       01 STOCKS-RECORD.
           02 SSYMBOL PIC X(7).
           02 SNAME PIC X(25).
           02 CPRICE PIC 9(4)V99.
           
       WORKING-STORAGE SECTION.
       *> Variables for processing and calculations.
       
       01 LS-NUMBER-OF-SHARES PIC 9(5).
       01 LS-CLOSING-PRICE PIC 9(4)V99.
       01 LS-ADJUSTED-COST-BASE PIC 9(5)V99.
       01 LS-MARKET-VALUE PIC 9(5)V99.
       01 LS-TOTAL-GAIN-OR-LOSS PIC 9(5)V99-.
       01 WRITE-COUNTER PIC 9(2).
       01 TABLE-INDEX PIC 9(2).
       01 EOF PIC A.
       COPY '../STOCKS-TABLE.dat'. *> Loading the structure for the stocks table from the COPY member.  
      * 01 STOCKS-TABLE OCCURS 20 TIMES.
      *     02 STOCK-SYMBOL PIC X(7).
      *     02 STOCK-NAME PIC X(25).
      *     02 CLOSING-PRICE PIC 9(4)V99.

       01 REPORT-RECORD.
           *> Defining the structure of the report record
           
           02 REPORT-STOCK-NAME PIC X(25).
           02 FILLER PIC X(3) VALUE SPACES.
           02 REPORT-NUMBER-OF-SHARES PIC ZZ,ZZ9.
           02 FILLER PIC X(3) VALUE SPACES.
           02 REPORT-AVERAGE-COST PIC $$,$$9.99.
           02 FILLER PIC X(3) VALUE SPACES.
           02 REPORT-CLOSING-PRICE PIC $$,$$9.99.
           02 FILLER PIC X(3) VALUE SPACES.
           02 REPORT-ADJUSTED-COST-BASE PIC $$$,$$9.99.
           02 FILLER PIC X(4) VALUE SPACES.
           02 REPORT-MARKET-VALUE PIC $$$,$$9.99.
           02 FILLER PIC X(2) VALUE SPACES.
           02 TOTAL-GAIN-OR-LOSS PIC $$$$,$$9.99-.


       01 INPUT-STOCK-SYMBOL PIC X(7).
       01 INPUT-NUMBER-OF-SHARES PIC 9(5).
       01 FOUND-FLAG PIC A.
       01 OUTPUT-NUMBER-OF-SHARES PIC 9(5).
       01 OUTPUT-AVERAGE-COST PIC 9(4)V99.
       01 ADJUSTED-COST-BASE PIC 9(5)V99.
       01 NEW-RECORD PIC A.
       
       SCREEN SECTION.
       01 INPUT-SCREEN.
           *> Defining the input screen layout
           
           02 LINE 2 COL 10 VALUE 'Buy Stocks'.
           02 LINE 4.
               03 COL 3 VALUE 'Enter a Stock Symbol: '.
               03 COL 25 PIC X(7) TO INPUT-STOCK-SYMBOL.
           02 LINE 5.
               03 COL 3 VALUE 'Enter the Number of Shares: '.
               03 COL 31 PIC X(5) TO INPUT-NUMBER-OF-SHARES.
       01 OUTPUT-SCREEN.
           *> Defining the output screen layout.
           
           02 LINE 7 COL 10 VALUE 'Updated Portfolio Record'.
           02 LINE 9.
               03 COL 3 VALUE 'Stock Symbol: '.
               03 COL 17 PIC X(7) FROM PORTFOLIO-STOCK-SYMBOL.
           02 LINE 10.
               03 COL 3 VALUE 'Number of Shares: '.
               03 COL 21 PIC ZZ,ZZ9 FROM OUTPUT-NUMBER-OF-SHARES.
           02 LINE 11.
               03 COL 3 VALUE 'Average Cost: '.
               03 COL 17 PIC $$,$$9.99 FROM OUTPUT-AVERAGE-COST.
           02 LINE 13.
               03 COL 3 VALUE 'Buy more stocks? (Y/N) '.
               03 COL 26 PIC A TO NEW-RECORD.
               
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            *> Main procedure to initialize, process, and close files.
           
           PERFORM INITIALIZATION-RTN.
           PERFORM PROCESS-RTN UNTIL NEW-RECORD = 'N'.
           PERFORM CLOSE-FILES.
           STOP RUN.
           
       INITIALIZATION-RTN.
           *> Routine to open files and load the stocks table.
           
           PERFORM OPEN-FILES.
           PERFORM LOAD-TABLE VARYING TABLE-INDEX FROM 1 BY 1
               UNTIL EOF = 'Y' OR TABLE-INDEX > 20.
               
       OPEN-FILES.
            *> Open the portfolio and stocks files.
           
           OPEN I-O PORTFOLIO-FILE.
           OPEN INPUT STOCKS-FILE.
           
       LOAD-TABLE.
           *> Load data from the stocks file into the stocks table.
           
           READ STOCKS-FILE
               AT END MOVE 'Y' TO EOF
               NOT AT END
                   MOVE SSYMBOL TO STOCK-SYMBOL(TABLE-INDEX)
                   MOVE SNAME TO STOCK-NAME(TABLE-INDEX)
                   MOVE CPRICE TO CLOSING-PRICE(TABLE-INDEX)
           END-READ.
               
       PROCESS-RTN.
           *> Routine to get user input, read/update portfolio, and display output.
           
           PERFORM GET-INPUT.
           PERFORM READ-PORTFOLIO-FILE.
           PERFORM DISPLAY-OUTPUT.
           
       GET-INPUT.
            *> Accept user input for stock symbol and number of shares.
           
           ACCEPT INPUT-SCREEN.
           MOVE INPUT-STOCK-SYMBOL TO PORTFOLIO-STOCK-SYMBOL.
           
       READ-PORTFOLIO-FILE.
           *> Read the portfolio file. If record exists, update it; otherwise, add new record.
           
           READ PORTFOLIO-FILE
               INVALID KEY PERFORM ADD-RECORD
               NOT INVALID KEY PERFORM UPDATE-RECORD
           END-READ.
               
       ADD-RECORD.
            *> Routine to add a new record to the portfolio.
           
           PERFORM SET-PORTFOLIO-RECORD.
           PERFORM CALCULATIONS-RTN.
           PERFORM WRITE-RECORD.
           
       UPDATE-RECORD.
           *> Routine to update an existing portfolio record.
           
           PERFORM CALCULATIONS-RTN.
           PERFORM REWRITE-RECORD.
           
       SET-PORTFOLIO-RECORD.
           *> Initialize new portfolio record fields.
           
           MOVE 0 TO NUMBER-OF-SHARES.
           MOVE 0 TO AVERAGE-COST.
           
       CALCULATIONS-RTN.
           *> Routine to perform calculations for adding or updating records.
           
           PERFORM SET-FOUND-FLAG.
           PERFORM CALCULATIONS VARYING TABLE-INDEX FROM 1 BY 1
               UNTIL FOUND-FLAG = 'Y' OR TABLE-INDEX > 20.
               
       SET-FOUND-FLAG.
           *> Initialize the found flag to 'N'.
           
           MOVE 'N' TO FOUND-FLAG.
           
       CALCULATIONS.
           *> Perform calculations to update average cost, number of shares, etc.
           
           IF PORTFOLIO-STOCK-SYMBOL = STOCK-SYMBOL(TABLE-INDEX)
               MOVE 'Y' TO FOUND-FLAG
               COMPUTE AVERAGE-COST = (NUMBER-OF-SHARES * AVERAGE-COST +
                   INPUT-NUMBER-OF-SHARES * CLOSING-PRICE(TABLE-INDEX))
                   / (NUMBER-OF-SHARES + INPUT-NUMBER-OF-SHARES)



               MOVE AVERAGE-COST TO OUTPUT-AVERAGE-COST
               ADD INPUT-NUMBER-OF-SHARES TO NUMBER-OF-SHARES
               MOVE NUMBER-OF-SHARES TO OUTPUT-NUMBER-OF-SHARES
               MULTIPLY NUMBER-OF-SHARES BY AVERAGE-COST
                   GIVING ADJUSTED-COST-BASE
               MOVE ADJUSTED-COST-BASE TO LS-ADJUSTED-COST-BASE
               MOVE NUMBER-OF-SHARES TO LS-NUMBER-OF-SHARES
               MOVE CLOSING-PRICE(TABLE-INDEX) TO LS-CLOSING-PRICE

               *> Call an external program to calculate market value and total gain/loss.
               CALL 'CALCULATE' USING LS-NUMBER-OF-SHARES
                                   LS-CLOSING-PRICE
                                   LS-ADJUSTED-COST-BASE
                                   LS-MARKET-VALUE
                                   LS-TOTAL-GAIN-OR-LOSS
               MOVE LS-MARKET-VALUE TO REPORT-MARKET-VALUE
               MOVE LS-TOTAL-GAIN-OR-LOSS TO TOTAL-GAIN-OR-LOSS

                ADD 1 TO WRITE-COUNTER
           END-IF.
               
       WRITE-RECORD.
           *> Write the new record to the portfolio file.
           
           WRITE PORTFOLIO-RECORD.
           
       REWRITE-RECORD.
           *> Rewrite the updated record in the portfolio file.
           
           REWRITE PORTFOLIO-RECORD.
           
       DISPLAY-OUTPUT.
            *> Display the updated portfolio record.
           
           ACCEPT OUTPUT-SCREEN.
           
       CLOSE-FILES.
           *> Close the portfolio and stocks files.
           
           CLOSE PORTFOLIO-FILE STOCKS-FILE.
       END PROGRAM BUY-STOCKS.
