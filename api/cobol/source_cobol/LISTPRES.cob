      **************************************************************************
      *I D E N T I F I C A T I O N   D I V I S I O N                         *
      **************************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 LISTPRES.
       AUTHOR.                     Thomas.


      **************************************************************************
      *E N V I R O N M E N T    D I V I S I O N                              *
      **************************************************************************
       ENVIRONMENT DIVISION.
       configuration section.
       special-names.
           Decimal-Point is Comma.
       input-output section.

       file-control.

           select F-Response
           assign to "/home/thomas/dev/projet_git/cobol-stage1/api/data_
      -              "txt/liste_prestation_response.txt"
           organization is line sequential access sequential.
      **************************************************************************
      *D A T A    D I V I S I O N                                            *
      **************************************************************************
       DATA DIVISION.

       FILE SECTION.
       FD F-Response record varying from 0 to 1400.
       01 E-Response pic x(1400).
      **************************************************************************
      *W O R K I N G   S T O R A G E   S E C T I O N                         *
      **************************************************************************
       WORKING-STORAGE SECTION.

       01 IdPresta pic 9(4).
       01 IdSinistre pic 9(4).
       01 StatusPresta pic 9.


       01 indexPresta pic 99.
       01 DELIMITEUR pic X.
       


       01 DATA-RESPONSE pic X(1000).
       01 MESSAGE-RESPONSE pic X(150).
       01 STATUT-RESPONSE pic X(6).
       01 PRESTA-STRING.
           10 ONE-PRESTA OCCURS 80 PIC X(110).
       01 COMPLETE-RESPONSE pic X(1300).


       COPY CPYTOM OF "cobol/source_cobol".
      ******************************************************************
      *P R O C E D U R E   D I V I S I O N
      ******************************************************************
       PROCEDURE DIVISION.

       GSPI.
           perform GSPI-Init.
           perform GSPI-Trt.
           perform GSPI-Fin.

       GSPI-Init.
           CONTINUE.
       GSPI-Trt.
               perform List-Presta-For-Validation.
               perform Write-Response-File.
       GSPI-Fin.
           stop run.

      ******************************************************************
      *****                    CHECK-IF-EXIST                      *****
      ******************************************************************
       List-Presta-For-Validation.
           perform List-Presta-For-Validation-Init.
           perform List-Presta-For-Validation-Trt.
           perform List-Presta-For-Validation-Fin.

       List-Presta-For-Validation-Init.
           perform Initialisation-connexion-BDD.
           perform Connexion-BDD.

           MOVE 0 TO SQLCODE.
           MOVE 0 TO indexPresta.


           IF SQLCA-CURSOR-CTRL (1) = 1
              SET DB-CURSOR-ALREADY-OPEN TO TRUE
           END-IF.

           MOVE 1 TO SQLCA-CURSOR-CTRL (1).


       List-Presta-For-Validation-Trt.
           perform Generate-ListPresta-SQLCA-STATEMENT.
           CALL 'MySQL_query' USING SQLCA-STATEMENT

           END-CALL.
           MOVE RETURN-CODE TO SQLCODE.
           IF DB-OK
              CALL 'MySQL_use_result' USING SQLCA-RESULT (1)
              END-CALL
              IF SQLCA-RESULT (1) = NULL
                 MOVE 100 TO SQLCODE
              ELSE
                 MOVE 0 TO SQLCODE
              END-IF
           END-IF.

           if SQLCODE equal 0 and DB-OK
               display sqlcode
               PERFORM UNTIL NOT DB-OK
                   add 1 to indexPresta
                   IF SQLCA-CURSOR-CTRL (1) = 0
                      SET DB-CURSOR-NOT-OPEN TO TRUE
                   END-IF
                   CALL 'MySQL_fetch_row' USING SQLCA-RESULT (1)
                                            IdPresta
                                            IdSinistre
                                            StatusPresta

                   END-CALL
                   IF SQLCA-RESULT (1) = NULL
                      MOVE 100 TO SQLCODE
                   ELSE
                       MOVE 0 TO SQLCODE
                   END-IF
                   if indexPresta equal 1 then
                        move '' to DELIMITEUR
                    else move ',' to DELIMITEUR
                    end-if
                   EVALUATE TRUE
                   WHEN DB-OK
                       STRING DELIMITEUR DELIMITED SIZE 
                           '"prestation_' DELIMITED SIZE
                           indexPresta DELIMITED SIZE
                           '" :' DELIMITED SIZE
                           '{' DELIMITED SIZE
                           '"idPresta" : ' DELIMITED SIZE
                           '"' DELIMITED SIZE
                           IdPresta DELIMITED SIZE
                           '"' DELIMITED SIZE
                           ',' DELIMITED SIZE
                           '"idSin" : ' DELIMITED SIZE
                           '"' DELIMITED SIZE
                           IdSinistre DELIMITED SIZE
                           '"' DELIMITED SIZE
                           ',' DELIMITED SIZE
                           '"status" : ' DELIMITED SIZE
                           '"' DELIMITED SIZE
                           StatusPresta DELIMITED SIZE
                           '"' DELIMITED SIZE
                           '}' DELIMITED SIZE
                           INTO ONE-PRESTA (indexPresta)
                       END-STRING
                   WHEN DB-NOT-FOUND
                       continue

                   END-EVALUATE
               END-PERFORM
           END-IF.

           MOVE "SUCCES = VOILA LA LISTE DES PRESTATIONS EN COURS"
           TO MESSAGE-RESPONSE.
           MOVE "SUCCES" TO STATUT-RESPONSE.


       List-Presta-For-Validation-Fin.
           perform close-BDD.
           EXIT.

       Write-Response-File.
           open output F-Response.
           STRING '{"prestation" :{' DELIMITED SIZE
                   PRESTA-STRING DELIMITED SIZE
                   '}' DELIMITED SIZE
           INTO DATA-RESPONSE
           END-STRING.

           STRING '{' DELIMITED SIZE
                   '"statut" : ' DELIMITED SIZE
                   '"' DELIMITED SIZE
                   STATUT-RESPONSE DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ',' DELIMITED SIZE
                   '"message" : ' DELIMITED SIZE
                   '"' DELIMITED SIZE
                   MESSAGE-RESPONSE DELIMITED SIZE
                   '"' DELIMITED SIZE
                   ',' DELIMITED SIZE
                   '"data" : ' DELIMITED SIZE
                   DATA-RESPONSE DELIMITED SIZE
                   '}}}' DELIMITED SIZE
           INTO COMPLETE-RESPONSE
           END-STRING.

           write E-Response from COMPLETE-RESPONSE.
           close F-Response.
           EXIT.
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      *****                GENERATE SQLCA SATTEMENT                *****
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************

      ******************************************************************
      *****             GENERATE-IFEXISTE-SQLCA-STATEMENT          *****
      ******************************************************************
       Generate-ListPresta-SQLCA-STATEMENT.
           MOVE LOW-VALUES TO SQLCA-STATEMENT.
               STRING 'SELECT ' DELIMITED SIZE
                   'IDPRESTATION, ' DELIMITED SIZE
                   'SINISTREID, ' DELIMITED SIZE
                   'STATUS ' DELIMITED SIZE
                   'FROM '    DELIMITED SIZE
                   'PRESTATION '    DELIMITED SIZE
                   'WHERE '    DELIMITED SIZE
                   'STATUS '    DELIMITED SIZE
                   '= "' DELIMITED SIZE
                   '0' DELIMITED SIZE
                   '"' DELIMITED SIZE
               INTO SQLCA-STATEMENT
               END-STRING.
       Generate-ListPresta-SQLCA-STATEMENT-Fin.
           EXIT.


      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************
      *****                      UTILITAIRES                       *****
      ******************************************************************
      ******************************************************************
      ******************************************************************
      ******************************************************************

      ******************************************************************
      *****              INITIALISATION-CONNEXION-BDD              *****
      ******************************************************************
       Initialisation-connexion-BDD.
           PERFORM Initialisation-connexion-BDD-Init.
           PERFORM Initialisation-connexion-BDD-Trt.
           PERFORM Initialisation-connexion-BDD-Fin.

       Initialisation-connexion-BDD-Init.
           MOVE 'LISTPRES' TO PGCTB-PROGRAM-NAME.

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

       Initialisation-connexion-BDD-Trt.
      ***** Initialize the database connection *****

           CALL "MySQL_init"  USING SQLCA-CID
           END-CALL.
           MOVE RETURN-CODE TO SQLCODE.
       Initialisation-connexion-BDD-Fin.
           EXIT.

      ******************************************************************
      *****                    CONNEXION-DBD                       *****
      ******************************************************************
       Connexion-BDD.
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
       Connexion-BDD-Fin.
           EXIT.

      ******************************************************************
      *****                    CLOSE-BDD                           *****
      ******************************************************************
       Close-BDD.
           CALL "MySQL_close"
           END-CALL.
           EXIT.
