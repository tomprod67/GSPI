
----+-*--1-!--+----2----+----3----+----4----+----5----+----6----+----7-!--+----8
      *    INITIALISIATION

           MOVE 'TTTTNNNB'                  TO PGCTB-PROGRAM-NAME
           SET PGCTB-OK                     TO TRUE
           SET DB-OK                        TO TRUE

      *    LIT PARAM Du fichier .param
           CALL "read_params"         USING PGCTB-PROGRAM-NAME
                                            SQLCA-HOST
                                            SQLCA-USER
                                            SQLCA-PASSWD
                                            SQLCA-DBNAME
                                            SQLCA-PORT
                                            SQLCA-SOCKET
           END-CALL
      *
           INSPECT SQLCA-HOST REPLACING ALL LOW-VALUE BY SPACE
           INSPECT SQLCA-USER REPLACING ALL LOW-VALUE BY SPACE
           INSPECT SQLCA-PASSWD REPLACING ALL LOW-VALUE BY SPACE
           INSPECT SQLCA-DBNAME REPLACING ALL LOW-VALUE BY SPACE
           INSPECT SQLCA-PORT REPLACING ALL LOW-VALUE BY SPACE
           INSPECT SQLCA-SOCKET REPLACING ALL LOW-VALUE BY SPACE
      *
           SET PGCTB-SQLCA-PTR              TO ADDRESS OF SQLCA
      *
      * All cursors are closed at the beginning
           MOVE ALL '0'                     TO SQLCA-CURSOR-CTRL-GRP
      *

      *
      *
      * Initialize the database connection
           EXEC SQL
              INIT DB
           END-EXEC.
           EVALUATE TRUE
           WHEN DB-OK
              DISPLAY "bdd trouver"
              CONTINUE
           WHEN DB-NOT-FOUND
              DISPLAY "bdd pas trouver"
              SET DB-OK              TO TRUE
           WHEN OTHER
              PERFORM DB-STATUS
           END-EVALUATE

           EXEC SQL
              CONNECT DB
           END-EXEC.
           EVALUATE TRUE
           WHEN DB-OK
              CONTINUE
           WHEN DB-NOT-FOUND
              SET DB-OK              TO TRUE
           WHEN OTHER
              PERFORM DB-STATUS
           END-EVALUATE
      *
      * Now execute the user's code
           PERFORM PGCTB-ACTION
      *    DISPLAY 'After PGCTB-ACTION (batch program)'
      *
      * Any errors?
           PERFORM DB-STATUS
      *    DISPLAY 'After DB-STATUS (batch program)'
      *    DISPLAY 'SQLCODE=' SQLCODE
      *
      * Commit the work
      *    DISPLAY 'Before COMMIT (batch program)'
           EXEC SQL
              COMMIT
           END-EXEC.
      *    DISPLAY 'After COMMIT (batch program)'
      *
      * We're done, now close the database and stop the program
           EXEC SQL
               CLOSE DB
           END-EXEC.
      *    DISPLAY 'After CLOSE DB (batch program)'
           PERFORM DB-STATUS
      *

      *
           MOVE 0                       TO RETURN-CODE
           .
       PGCTB-MAIN-EXIT.
           STOP RUN.
      /
      *************************************************************************
       PGCTB-STATUS SECTION.
           IF PGCTB-ERROR
              IF PGCTB-ERROR-MESSAGE = SPACES
                 STRING PGCTB-PROGRAM-NAME    DELIMITED BY SIZE
                        ': PGCTB-STATUS-FLD ' DELIMITED BY SIZE
                         PGCTB-STATUS-FLD     DELIMITED BY SIZE
                         ' is set!'         DELIMITED BY SIZE
                                   INTO PGCTB-ERROR-MESSAGE
              END-IF
				         PERFORM PGCTB-DISPLAY-ERROR
      *
      * Rollback the work
              EXEC SQL
                 ROLLBACK
              END-EXEC.
              MOVE 2                       TO RETURN-CODE
              STOP RUN
           END-IF
           .
       PGCTB-STATUS-EXIT.
           EXIT.
      *************************************************************************
       PGCTB-DISPLAY-ERROR SECTION.
           DISPLAY '*******************************************'
                   '******************************'
           DISPLAY '* E R R O R * E R R O R * E R R O R * E R R'
                   ' O R * E R R O R * E R R O R *'
           DISPLAY '*******************************************'
                   '******************************'
           DISPLAY '***                                        '
                   '                           ***'
           DISPLAY '** ' PGCTB-ERROR-MESSAGE ' **'
           DISPLAY '***                                        '
                   '                           ***'
           DISPLAY '*******************************************'
                   '******************************'
           DISPLAY '* E R R O R * E R R O R * E R R O R * E R R'
                   ' O R * E R R O R * E R R O R *'
           DISPLAY '*******************************************'
                   '******************************'
           DISPLAY '*      D A T A B A S E   W O R K   U N I T '
                   '  R O L L E D    B A C K     *'
           DISPLAY '*******************************************'
                   '******************************'
           .
       PGCTB-DISPLAY-ERROR-EXIT.
           EXIT.
      *************************************************************************
       DB-STATUS SECTION.
      *    DISPLAY 'in DB-STATUS, SQLCODE = ' SQLCODE
           IF SQLCODE NOT = 0
              CALL "MySQL_errno" USING PGCTB-ERRNO
              END-CALL
              DISPLAY 'ERRNO: ' PGCTB-ERRNO
              CALL "MySQL_error" USING PGCTB-ERROR-MESSAGE
              END-CALL
              DISPLAY PGCTB-ERROR-MESSAGE
              MOVE SPACES                      TO PGCTB-ERROR-MESSAGE
              STRING  'DB-STATUS: Program '     DELIMITED BY SIZE
                      PGCTB-PROGRAM-NAME         DELIMITED BY SIZE
                      ' SQLCODE='             DELIMITED BY SIZE
                      SQLCODE                  DELIMITED BY SIZE
                      '   SQLCA-SEQUENCE='     DELIMITED BY SIZE
                      SQLCA-SEQUENCE           DELIMITED BY SIZE
                      ' '                      DELIMITED BY SIZE
                                               INTO PGCTB-ERROR-MESSAGE
              PERFORM PGCTB-DISPLAY-ERROR
      *
      * Rollback the work
              EXEC SQL
                 ROLLBACK
              END-EXEC.
              MOVE 3                       TO RETURN-CODE
              STOP RUN
           END-IF
           .
       DB-STATUS-EXIT.
           EXIT.
