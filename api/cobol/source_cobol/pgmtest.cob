      **************************************************************************
      *I D E N T I F I C A T I O N   D I V I S I O N                         *
      **************************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 pgmtest.
       AUTHOR.                     Thomas.

      **************************************************************************
      *D A T A    D I V I S I O N                                            *
      **************************************************************************
       DATA DIVISION.

       FILE SECTION.
      **************************************************************************
      *W O R K I N G   S T O R A G E   S E C T I O N                         *
      **************************************************************************
       WORKING-STORAGE SECTION.
       01 PrenomB Pic X(15).
       01 NomB Pic X(15).

       COPY CPYTOM OF cobol.
      ******************************************************************
      *P R O C E D U R E   D I V I S I O N
      ******************************************************************
       PROCEDURE DIVISION.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      *****                    FRAMEWORK PGCTBBAT                  *****
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************

       PGCTB-MAIN.
      ***** Inititialisation du framework
      * IMPORTANT CHANGER 'pgmtest' par votre nom de programme

           display "recuperation infos connection".
           MOVE 'pgmtest' TO PGCTB-PROGRAM-NAME.

           SET PGCTB-OK TO TRUE.
           SET DB-OK TO TRUE.

      *    Recuperation paramètre de connexion à la BDD
           CALL "read_params"         USING PGCTB-PROGRAM-NAME
                                            SQLCA-HOST
                                            SQLCA-USER
                                            SQLCA-PASSWD
                                            SQLCA-DBNAME
                                            SQLCA-PORT
                                            SQLCA-SOCKET
           END-CALL.

           INSPECT SQLCA-HOST   REPLACING ALL LOW-VALUE BY SPACE.
           INSPECT SQLCA-USER   REPLACING ALL LOW-VALUE BY SPACE.
           INSPECT SQLCA-PASSWD REPLACING ALL LOW-VALUE BY SPACE.
           INSPECT SQLCA-DBNAME REPLACING ALL LOW-VALUE BY SPACE.
           INSPECT SQLCA-PORT   REPLACING ALL LOW-VALUE BY SPACE.
           INSPECT SQLCA-SOCKET REPLACING ALL LOW-VALUE BY SPACE.

      ***** Initialize the database connection *****

           display "Initialisation connection BDD".
           CALL "MySQL_init"  USING SQLCA-CID
           END-CALL.
           MOVE RETURN-CODE TO SQLCODE.

      ***** Conection à la BDD *****
           display "connection BDD ".
           CALL "MySQL_real_connect" USING
                                   SQLCA-HOST
                                   SQLCA-USER
                                   SQLCA-PASSWD
                                   SQLCA-DBNAME
                                   SQLCA-PORT
                                   SQLCA-SOCKET
           END-CALL.
           MOVE RETURN-CODE TO SQLCODE.
           if sqlcode equal 0 then
               display "bon okok pppour copy"
               display sqlca-cid
           end-if.
           if sqlcode not equal 0 then
            display "pas bon  pour copy"
               end-if.

      ***** EXECUTE Le code de l'user dans PGCTB-ACTION *****
           PERFORM PGCTB-ACTION THRU PGCTB-ACTION-FIN.

      *    **** Close BDD *****

           display "close mysql" .
           CALL "MySQL_close"
           END-CALL.
           Perform userResponse.

       PGCTB-MAIN-EXIT.
           STOP RUN.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      *****                FIN DU FRAMEWORK PGCTBBAT               *****
      ******************************************************************
      ******************************************************************
      ******************************************************************

       PGCTB-ACTION.
           display " Debut requete thomas".
           Move "tototo" to PrenomB.
           Move "tatata" to NomB.
      *    INSPECT PrenomB  REPLACING ALL SPACES by LOW-VALUE .
      *    INSPECT NomB REPLACING ALL SPACES BY SPACE.

           MOVE LOW-VALUES TO SQLCA-STATEMENT.
           STRING 'INSERT ' DELIMITED SIZE
                  'INTO ' DELIMITED SIZE
                  'Client' DELIMITED SIZE
                  '( '    DELIMITED SIZE
                  'Nom, ' DELIMITED SIZE
                  'Prenom ' DELIMITED SIZE
                  ') '    DELIMITED SIZE
                  'VALUES ' DELIMITED SIZE
                  '("'    DELIMITED SIZE
                  NomB DELIMITED SIZE
                  '","' DELIMITED SIZE
                  PrenomB DELIMITED SIZE
                  '") ' DELIMITED SIZE
               INTO SQLCA-STATEMENT
           END-STRING.
           CALL 'MySQL_query' USING SQLCA-STATEMENT
           END-CALL.
           MOVE RETURN-CODE TO SQLCODE.
           display "requete Thomas -terminé".


       PGCTB-ACTION-FIN.
           EXIT.

       UserResponse.

           Display "Reponse du programme :"
           IF SQLCODE IS NOT EQUAL 0 THEN
              DISPLAY "ERROR"
           ELSE DISPLAY "C BON 2"
           END-IF.
           EXIT.
