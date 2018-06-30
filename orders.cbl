      ****************************
      IDENTIFICATION DIVISION
      ****************************
      PROGRAM-ID.      ORDERS.
      AUTHOR.          CHRISTOPHER WATKIN.
      DATE-WRITTEN.    A LONG TIME AGO
      DATE-WRITTEN.    JUNE 21, 2018- RE-KEYED ON VSCODE FOR GIT
      ******************************************************************
      * THIS PROGRAM WILL PROCESS ORDERS FOR ITEMS IN STOCK.  FOR ITEMS
      * WHICH NEED TO BE BACKORDERED, A ROUTINE WILL PROCESS A BACKORDER
      * RECORD.  ALL ITEMS WILL BE CHECKED TO SEE IF ANY NEED TO BE
      * REORDERED.
      * THIS PROGRAM WAS THE FINAL PROJECT FOR THE COBOL TRACK AT 
      * CHUBB INSTITUTE.
      * BELOW ARE THE ORIGINAL EDIT DATES AND WHY.
      *    DATE            INITIALS        DESCRIPTION
      *   -----            --------        -----------
      *    10/16/XX        CW      INITIAL WRITING OF PROGRAM.
      *    10/21/XX        CW      FIRST COMPILATION
      *    10/23/XX        CW      FIRST RUN, MINOR CORRECTIONS.
      *    10/27/XX        CW      FIRST PRINT REPORT.
      *    10/28/XX        CW      CORRECTION IN B/O ROUTINE (BACKORDER)
      * WITH THIS PROJECT THE LAST 'ERROR'- THE PRINT REPORT WAS 
      * NOT PRINTING IN THE CORRECT FORMAT- IT WAS SHIFTED IN 
      * ONE SECTION; IT TOOK A WHILE TO FIND THAT A LINE WAS ONE 
      * BYTE OFF! 
      * THIS IS NOT COMPILED NOR DO I HAVE THE JCL, COPYBOOKS OR 
      * DB2 FILES- THESE WERE LOST TO TIME- I PROBABLY TOSSED THE 
      * ACTUAL PRINTOUT OF ALL PROGRAMS & OUTPUT-
      * THIS HAPPENS TO BE JUST THE MAIN PROGRAM I SAVED AS HARDCOPY.
      ******************************************************************                                                                                             
       ENVIRONMENT DIVISION.
      ******************************************************************
      CONFIGURATION SECTION.
      SOURCE-COMPUTER.             IBM-370.
      OBJECT-COMPUTER.             IBM-370.
      INPUT-OUTPUT SECTION.
      FILE-CONTROL.
           SELECT MASTER-FILE      ASSIGN MASTFILE
                ORGANIZATION IS INDEXED
                ACCESS IS RANDOM
                RECORD KEY IS M-ITEMNO
                FILE STATUS IS VSAM-STAT.
            SELECT TRANSACTION-FILE    ASSIGN TRANFILE
                ORGANIZATION IS INDEXED
                ACCESS IS SEQUENTIAL
                RECORD KEY IS T-ORDERS
                FILE STATUS IS VSAM-STAT.
            SELECT INVORD-FIRE         ASSIGN INVFILE.
            SELECT BACKORDS-FILE       ASSIGN BKORDS.
            SELECT ERROR-FILE          ASSIGN ERRFILE.
            SELECT REORDERS-FILE       ASSIGN REORDS.
      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
       FD  MASTER-FILE.
       01  VMAST-REC.
           05 M-ITEMNO                 PIC 9(5).
           05 FILLER                   PIC X(65).
       FD TRANSACTION-FILE
               LABEL RECORDS ARE STANDARD.
       01   VTRANS-REC.
           05  T-ORDERS.
               10 T-CODE                   PIC 99.
               10 T-ITEMNO                 PIC 9(5).
               10 T-CUSTNO                 PIC 9(6).
           05  FILLER                  PIC X(67).
       FD INVORD--FILE
               LABEL RECORDS ARE STANDARD
               BLOCK CONTAINS 10 RECORDS.
       01 INV-ORD-REC                  PIC X(47).
       FD  BACKORDS-FILE
               LABEL RECORDS ARE STANDARD
               BLOCK CONTAINS 10 RECORDS.
       01  BACKORDS-REC                PIC X(40).
       FD  ERROR-FILE
               LABEL RECORDS ARE STANDARD
               BLOCK CONTAINS 10 RECORDS.
       01  ERR-REC                     PIC X(120).
       FD  REORDER-FILE
               LABEL RECORDS ARE STANDARD
               BLOCK CONTAINS 10 RECORDS.
       01  REORD-REC                   PIC X(133).
       WORKING-STORAGE SECTION.
       01  SWITCHES.
           05 EOF-SW                   PIC X       VALUE 'N'.
               88 TRANSEOF                         VALUE 'Y'.
           05 MATCH-SW                 PIC X       VALUE 'N'.
       01  COUNTERS-ACCUMULATORS.
           05  PG-CTR                  PIC 999     VALUE 0.
           05  LINE-CTR                PIC 99      VALUE 56.
           05  TOT-CTR                 PIC 9999    VALUE 0.
           05  REORD-CTR               PIC 999     VALUE 0.
           05  CC                      PIC 9       VALUE 1.
       01  WORK-AREA.
           05  ORDAMT                  PIC 9(4).
           05  VSAM-STAT               PIC XX.
           05  CURRDATE
               10  C-YY                PIC 99.
               10  C-MM                PIC 99.
               10  C-DD                PIC 99.
       01  WS-MAST-REC.
           05  WS-M-ITEMNO             PIC 9(5).
           05  WS-M-DESCRIP            PIC X(20).
           05  WS-M-QOH                PIC 9(4).
           05  WS-M-PURCHPRC           PIC 9(3)V99.
           05  WS-M-SALESPRC           PIC 9(3)V99.
           05  WS-M-REORDPT            PIC 9(4).
           05  WS-M-MINREORD           PIC 9(4).
           05  WS-M-BOQUAN             PIC 9(4).
           05  WS-M-QTYONORD           PIC 9(4).
           05  WS-M-REPLNO             PIC 9(5).
           05  WS-M-REPLDATE           PIC 9(6).
           05  WS-M-YTDSALES           PIC 9(7)    COMP-3.
       01  WS-TRANS-REC.
           05  WS-T-CODE               PIC 99.
           05  WS-T-ITEMNO             PIC 9(5).
           05  WS-T-CUSTNO             PIC 9(6).
           05  WS-T-QUANONORD          PIC 9(4).
           05  WS-T-BOIND              PIC X.
           05  WS-T-PAY                PIC 9.
           05  WS-T-ORDDATE            PIC 9(6).
           05  FILLER                  PIC X(55).
       01  WS-BACKORDS-REC.
           05  WS-B-ITEMNO             PIC 9(5).
           05  WS-B-DESCRIP            PIC X(20).
           05  WS-B-CUSTNO             PIC 9(6).
           05  WS-B-BOQUAN             PIC 9(4).
           05  WS-B-SALESPRC           PIC 9(3)V99.
       01  WS-INVORD-REC.
           05  WS-I-ITEMNO             PIC 9(5).
           05  WS-I-DESCRIP            PIC X(20).
           05  WS-I-CUSTNO             PIC 9(6).
           05  WS-I-QUANSHIP           PIC 9(4).
           05  WS-I-SALESPRC           PIC 9(3)V99.
           05  WS-I-DATESHIP.
               10  WS-I-MM             PIC Z9.
               10  WS-I-DD             PIC Z9.
               10  WS-I-YY             PIC 99.
           05  WS-I-PAY                PIC 9.
       01  WS-ERR-REC.
           05  ERRMSG                  PIC X(40).
           05  ERRRECIMAGE             PIC X(80).
       01  HEADER1.
           05  FILLER                 PIC X(5)    VALUE SPACES.
           05  FILLER                 PIC X(14)   VALUE 'REPORT DATE: '.
           05  DATE-OUT               PIC X(20).
           05  FILLER                 PIC X(14)   VALUE SPACES.
           05  FILLER                 PIC X(24)
                   VALUE 'OFFICE SUPPLY DEPARTMENT'.
           05  FILLER                 PIC X(33)       VALUE SPACES.
           05  FILLER                 PIC X(5)        VALUE 'PAGE '.
           05  PAGE-OUT               PIC ZZ9.
           05  FILLER                 PIC X(15)       VALUE SPACES.
       01  HEADER2.
       01  SUBHDR1.
       01  SUBHDR2.
       01  DETAIL-LINE.
       01  TRAILER.
       LINKAGE SECTION.
       01  PARM-DATA.
           05  FILLER                  PIC XX.
           05  P-MONTH                 PIC Z9.
           05  FILLER                  PIC X       VALUE '/'.
           05  P-DAY                   PIC Z9.
           05  FILLER                  PIC X       VALUE '/'.
           05  P-YEAR                  PIC 99.
      ******************************************************************
       PROCEDURE DIVISION USING PARM-DATA.
      ******************************************************************
       MAINLINE SECTION.
           PERFORM 0100-INIT THRU 0100-INIT-EXIT.
           PERFORM 0200-FIRST-TRANSREAD THRU 0200-FIRST-TRANSREAD-EXIT.
           PERFORM 0300-UPDATE THRU 0300-UPDATE-EXIT
               UNTIL EOF-SW = 'Y'.
           PERFORM 999-EOJ THRU 999-EOJ-EXIT.
           STOP RUN.
       SUBROUTINE SECTION.
      ******************************************************************
      *  OPEN ALL FILES. TRANSACTION FILE WILL BE READ STARTING AT
      *  THE FIRST RECORD WITH A TRANSACTION CODE OF 30. 
      ******************************************************************
       0100-INIT.
               ACCEPT CURRDATE FROM DATE.
               MOVE P-MONTH TO RUN-MONTH.
               MOVE P-DAY TO RUN-DAY.
               MOVE P-YEAR TO RUN-YEAR.
               MOVE MONTH-TITLE(RUN-MONTH) TO RUN-MONTH-OUT.
               MOVE RUN-DAY TO RUN-DAY-OUT.
               MOVE RUN-YEAR TO RUN-YEAR-OUT.
               MOVE RUN-DATE-OUT TO DATE-OUT.
           OPEN I-O TRANSACTION-FILE
               EVALUATE TRUE
                   WHEN VSAM-STAT = '00'
                     CONTINUE
                  WHEN OTHER
                    DISPLAY 'FAILURE TO OPEN TRANSACTION FILE'
                    CLOSE TRANSACTION-FILE
                    STOP RUN.      
            OPEN I-O MASTER-FILE
               EVALUATE TRUE
                   WHEN VSAM-STAT = '00'
                     CONTINUE
                  WHEN OTHER
                    DISPLAY 'FAILURE TO OPEN MASTER FILE'
                    CLOSE MASTERFILE
                    STOP RUN.   
            OPEN OUTPUT    INVORD-FILE
                           BACKORDS-FILE 
                           ERROR-FILE
                           REORDERS-FILE.
           0150 START-TRANSFILE.
               MOVE 30 TO T-CODE.
               START TRANSACTION-FILE KEY = T-CODE.
                   IF VSAM-STAT NOT = '00'
                    DISPLAY 'START NOT SUCCESSFUL FOR TRANSACTION FILE'
                           'VSAM STATUS CODE ' VSAM-STAT
                       PERFORM 999-EOJ THRU 999-EOJ-EXIT.
       0100-INIT-EXIT.    
           EXIT.
       0200-FIRST-TRANSREAD.
           READ TRANSACTION-FILE INTO WS-TRANS-REC.
               IF TRANS-EOF
                   MOVE 'Y' TO EOF-SW
                ELSE
                   IF T-CODE = 30
                       PERFORM 0400-READ-MAST THRU 0400-READ-MAST-EXIT
                           UNTIL MATCH-EOF = 'Y' OR TRANSEOF
                    ELSE
                       MOVE 'Y' TO EOF-SW.
       0200-FIRST-TRANSREC-EXIT.
           EXIT.
      ******************************************************************
      * MASTER FILE WILL BE READ INTO WORKING STORAGE AND MATCHED
      * AGAINST TRANSACTION RECORDS. 
      ******************************************************************
       0400-READ-MAST.
           MOVE WS-T-ITEMNO TO M-ITEMNO.
           READ MASTER-FILE INTO WS-MAST-REC.
               EVALUATE TRUE
                   WHEN VSAM-STAT = '00'
                     MOVE 'Y' TO MATCH-SW
                  WHEN VSAM-STAT = '23'
                    PERFORM 0500-CANCEL THRU 0500-CANCEL-EXIT.
                    PERFORM 0600-READ-TRANS THRU 0600-READ-TRANS-EXIT.
                  WHEN OTHER
                    DISPLAY 'FATAL ERROR IN READING MASTER FILE'
                          'VSAM CODE '  VSAM-STAT
                    PERFORM 999-EOJ THRU 999-EOJ-EXIT
                    STOP RUN.
       0400-READ-MAST-EXIT.
           EXIT.
      ******************************************************************
      *  EACH TRANSACTION RECORD WILL BE CHECKED FOR THE AMOUNT ORDERED
      *  IT WILL BE CHECKED AGAINST THE MASTER RECORD TO DETERMINE WHAT
      *  CAN BE DELIVERED AND WHAT NEEDS TO BE BACKORDERED. 
      ******************************************************************
       0300-UPDATE.
           IF WS-T-ITEMNO = WS-M-ITEMNO
               PERFORM 0700-ORDERS THRU 0700-ORDERS-EXIT
               PERFORM 0600-READ-TRANS THRU O600-READ-TRANS-EXIT
           ELSE
               PERFORM 0800-REORDERS THRU 0800-REORDERS-EXIT
               MOVE 'N' TO MATCH-SW
               PERFORM 0400-READ-MAST THRU O400-READ-MAST-EXIT
                   UNTIL MATCH-SW = 'Y' OR TRANSEOF.
       0300-UPDATE-EXIT.
           EXIT.
      ******************************************************************     
      *  CANCEL ANY TRANSACTION RECORD THAT DOES NOT MATCH UP WITH A
      *  TRANSACTION RECORD. 
      ****************************************************************** 
       0500-CANCEL.
           MOVE 'RECORD NOT FOUND ' TO ERRMSG.
           MOVE WS-TRANS-REC TO ERRRECIMAGE.
           WRITE ERR-REC FROM WS-ERR-REC.
           PERFORM 0900-DELETE THRU 0900-DELETE-EXIT.
       0500-CANCEL-EXIT.
           EXIT.
      ******************************************************************
      *  READ THE NEXT SEQUENTIAL TRANSACTION RECORD.
      ******************************************************************
       0600-READ-TRANS.
           READ TRANSACTION-FILE INTO WS-TRANS-REC.
               IF TRANSEOF
                   MOVE 'Y' TO EOF-SW
               ELSE
                   IF T-CODE NOT = 30
                       MOVE 'Y' TO EOF-SW.
       0600-READ-TRANS-EXIT.
           EXIT.
      ******************************************************************
      *  DETERMINE THE STATUS OF TRANSACTION RECORD QUANTITY ORDERED.
      *  ROUTINE WILL DETERMINE IF ANY ITEMS NEED TO BE BACKORDERED
      *  OR NOT.
      ******************************************************************    
       0700-ORDERS.
           EVALUATE TRUE
               WHEN WS-T-QUANORD < WS-M-QOH OR = WS-M-QOH
                   ADD WS-T-QUANORD TO WS-M-YTDSALES
                   SUBTRACT WS-T-QUANORD FROM WS-M-QOH
                   PERFORM 1000-INVOICE THRU 1000-INVOICE-EXIT
                   PERFORM 0900-DELETE THRU 0900-DELETE-EXIT
               WHEN WS-M-QOH > 0
                   ADD WS-M-QOH TO WS-M-YTDSALES
                   COMPUTE ORDAMT = WS-T-QUANORD - WS-M-QOH
                   MOVE WS-T-QOH TO WS-T-QUANORD
                   PERFORM 1000-INVOICE THRU 1000-INVOICE-EXIT
                   MOVE ORDAMT TO WS-T-QUANORD
                   PERFORM 1100-BACKORDER THRU 1100-BACKORDER-EXIT
                WHEN OTHER
                   PERFORM 1100-BACKORDER THRU 1100-BACKORDER-EXIT.
       0700-ORDERS-EXIT.
           EXIT.
      ******************************************************************
      *  ROUTINE WILL DETERMINE IF ANY ITEMS NEED TO BE REORDERED.
      ******************************************************************
       0800-REORDERS.
           IF WS-M-QOH NOT > WS-M-REORDPT
               PERFORM 1200-FORMAT THRU 1200-FORMAT-EXIT
               PERFORM 1300-PRINT THRU 1300-PRINT-EXIT
               ADD 1 TO REORD-CTR.
               REWRITE MAST-REC FROM WS-MAST-REC
       0800-REORDERS-EXIT.
           EXIT.
      ******************************************************************
      *  ROUTINE WILL DELETE ANY RECORDS THAT COULD NOT BE MATCHED
      *  TO THE MASTER RECORD.  ANY TRANSACTION RECORD ORDER THAT 
      *  COULD BE COMPLETELY FILLED WILL ALSO BE DELETED.
      ******************************************************************
       0900-DELETE.
           MOVE WS-T-ITEMNO TO M-ITEMNO
               EVALUATE TRUE
                  WHEN VSAM-STAT = '23'
                     DELETE TRANSACTION-FILE
                  WHEN VSAM-STAT = '00'
                    DELETE TRANSACTION-FILE
                  WHEN VSAM-STAT NOT '00'
                    DISPLAY 'FATAL ERROR DELETE ROUTINE ' VSAM-STAT
                    PERFORM 9999-EOJ THRU 999-EOJ-EXIT
                    STOP RUN.
       0900-DELETE-EXIT.
           EXIT.
      ******************************************************************
      *  ROUTINE WILL FILL A COMPLETE OR PARTIAL ORDER.
      ******************************************************************
       1000-INVOICE.
            ADD WS-T-QUANORD TO TOT-CTR.
            MOVE WS-T-QUANORD TO WS-I-QUANSHIP.
            MOVE WS-T-ITEMNO TO WS-I-ITEMNO.
            MOVE WS-T-CUSTNO TO WS-I-CUSTNO.
            MOVE WS-M-DESCRIP TO WS-I-DESCRIP.
            MOVE WS-M-SALESPRC TO WS-I-SALESPRC.
            MOVE C-MM TO WS-I-MM.
            MOVE C-DD TO WS-I-DD.
            MOVE C-YY TO WS-I-YY.
            MOVE WS-T-PAY TO WS-I-PAY.
            WRITE INVORD-REC FROM WS-INVORD-REC.
       1000-INVOICE-EXIT.
           EXIT.
      ******************************************************************
      *  ROUTINE WILL BACKORDER ALL OR PART OF AN ORDER.
      ******************************************************************
       1100-BACKORDER.
           IF WS-T-BOIND = 'N'
             MOVE 'Y' TO WS-T-BOIND
             ADD WS-T-QUAN TO WS-M-BOQUAN.
               MOVE WS-T-ITEMNO TO WS-B-ITEMNO.
               MOVE WS-M-DESCRIP TO WS-B-DESCRIP.
               MOVE WS-T-CUSTNO TO WS-B-CUSTNO.
               MOVE WS-T-QUAN TO WS-B-BOQUAN.
               MOVE WS-M-SALESPRC TO WS-B-SALES-PRC.
               WRITE BCKORDS-REC FROM WS-BACKORDS-REC.
               REWRITE VTRANS-REC FROM WS-TRANS-REC.
       1100-BACKORDER-EXIT.
           EXIT.
      ******************************************************************
      *  FORMAT REORDERS PRINT FILE.
      ******************************************************************
       1200-FORMAT.
           MOVE WS-M-ITEMNO TO ITEMNO-OUT.
           MOVE WS-M-DESCRIPT TO DESCRIP-OUT.
           MOVE WS-M-QOH TO QOH-OUT.
           MOVE WS-M-PURCHPRC TO PURCHPRC-OUT.
           MOVE WS-M-SALESPRC TO SALESPRC-OUT.
           MOVE WS-M-REORDPT TO REORDPT-OUT.
           MOVE WS-M-MINREORD TO MINREORD-OUT.
           MOVE WS-M-BOQUAN TO BOQUAN-OUT.
           MOVE WS-M-QTYONORD TO QTYONORD-OUT.
           MOVE WS-M-REPLNO TO REPLNO-OUT.
           MOVE WS-M-YTDSALES TO YTDSALES.
       1200-FORMAT-EXIT.
           EXIT.
      ******************************************************************
      *  DETAIL LINES FOR REORDERS REPORT.
      ******************************************************************
       1300-PRINT.
           IF LINE-CTR > 50
               ADD 1 TO PG-CTR
               MOVE PG-CTR TO PG-OUT
               WRITE REORD-REC FROM HEADER1
                   AFTER ADVANCING PAGE
               WRITE REORD-REC FROM HEADER2
                   AFTER ADVANCING 1 LINE
               WRITE REORD-REC FROM SUBHDR1
                   AFTER ADVANCING 2 LINES
               WRITE REORD-REC FROM SUBHDR2
                   AFTER ADVANCING 1 LINE
               MOVE 5 TO LINE-CTR
               MOVE 2 TO CC.
               WRITE REORD-REC FROM DETAIL-LINE
                   AFTER ADVANCING CC LINES.
               ADD CC TO LINE-CTR.
               MOVE 1 TO CC.   
       1300-PRINT-EXIT.
           EXIT.
      ******************************************************************
      *  TRAILER WILL SHOW TOTAL NUMBER OF ITEMS THAT NEED TO BE 
      *  REORDERED AS WELL AS DISPLAY A MESSAGE SHOWING HOW MANY ITEMS
      *  WERE SKIPPED ON THIS RUN.
      ******************************************************************
       1400-TRAILER.
           IF LINE-CTR > 50
               ADD 1 TO PG-CTR
               MOVE PG-CTR TO PG-OUT
               WRITE REORD0-REC FROM HEADER1
                   AFTER ADVANCING PAGE
               WRITE REORD-REC FROM HEADER2
                   AFTER ADVANCING 1 LINE.
               MOVE REORD-CTR TO REORDCTR-OUT
               WRITE REORD-REC FROM TRAILER
                   AFTER ADVANCING 3 LINES.
       1400-TRAILER-EXIT.
           EXIT.
      ******************************************************************
      *  CLOSE ALL FILES.
      ******************************************************************
       999-EOJ.
           PERFORM 0800-REORDERS THRU 0800-REORDERS-EXIT.
           PERFORM 1400-TRAILER THRU 1400-TRAILER-EXIT.
           DISPLAY TOT-CTR ' WILL BE SHIPPED ON THIS RUN.'.
           CLOSE   BACKORDER-FILE
                   ERROR-FILE
                   INVORD-FILE
                   REORDERS-FILE.
           CLOSE   MASTER-FILE
               EVALUATE TRUE
                   WHEN VSAM-STAT = '00'
                     CONTINUE
                   WHEN OTHER
                       DISPLAY 'FAILURE TO CLOSE MASTER FILE.'.
            CLOSE  TRANSACTION-FILE
                EVALUATE TRUE
                   WHEN VSAM-STAT = '00'
                     CONTINUE
                   WHEN OTHER
                       DISPLAY 'FAILURE TO CLOSE TRANSACTION FILE.'.
       9999-EOJ-EXIT.
           EXIT.