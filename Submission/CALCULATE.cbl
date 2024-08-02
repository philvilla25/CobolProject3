       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATE.                     *> Program identifier and name

       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-NUMBER-OF-SHARES PIC 9(5).           *> Number of shares passed to subroutine
       01 LS-CLOSING-PRICE PIC 9(4)V99.           *> Closing price passed to subroutine
       01 LS-ADJUSTED-COST-BASE PIC 9(5)V99.      *> Adjusted cost base passed to subroutine
       01 LS-MARKET-VALUE PIC 9(5)V99.            *> Market value calculated in subroutine
       01 LS-TOTAL-GAIN-OR-LOSS PIC 9(5)V99-.     *> Total gain or loss calculated in subroutine

       PROCEDURE DIVISION USING LS-NUMBER-OF-SHARES LS-CLOSING-PRICE
            LS-ADJUSTED-COST-BASE LS-MARKET-VALUE LS-TOTAL-GAIN-OR-LOSS.

           COMPUTE LS-MARKET-VALUE =              *> Calculate market value
            LS-NUMBER-OF-SHARES * LS-CLOSING-PRICE.

           SUBTRACT LS-ADJUSTED-COST-BASE FROM LS-MARKET-VALUE
           GIVING LS-TOTAL-GAIN-OR-LOSS.          *> Calculate total gain or loss

           EXIT PROGRAM.                          *> Exit subroutine

       END PROGRAM CALCULATE.
