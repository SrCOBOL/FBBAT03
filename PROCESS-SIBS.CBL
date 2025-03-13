      *-------------------------------------------------------------*  
      *  DESCRIPTION:                                               *                                    
      * DEVELOPER: FRANCISCO BORGES                                 *  
      * DATE-WRITTEN.  MARCH 2025.                                  *  
      *-------------------------------------------------------------*  
     
       IDENTIFICATION DIVISION.  
       PROGRAM-ID. PROCESS-SIBS.
    
       ENVIRONMENT DIVISION.                                       
       CONFIGURATION SECTION.                                     
       SPECIAL-NAMES.                                              
           DECIMAL-POINT IS COMMA.                                 
       INPUT-OUTPUT SECTION.                                         
       FILE-CONTROL.                                                    
           SELECT SIBS-FILE ASSIGN TO '/home/kikos/ficheiros/ENT002'   
              ORGANIZATION IS LINE SEQUENTIAL.                        
      *        
       DATA DIVISION.                                           
       FILE SECTION.                                            
       FD  SIBS-FILE.                                           
       01  SIBS-RECORD.                                         
           05 TRANSACTION-ID         PIC X(06).                 
           05 TRANSACTION-DATE       PIC X(08).                 
           05 TRANSACTION-TIME       PIC X(04).                  
           05 CARD-NUMBER            PIC X(16).                   
           05 TRANSACTION-AMOUNT     PIC S9(07)V99.                
           05 TRANSACTION-CODE       PIC X(04).                    
           05 REGION-CODE            PIC X(01).                    
           05 TRANSACTION-STATUS     PIC X(10).                      
           05 FILLER                 PIC X(20).                     
      *     
       WORKING-STORAGE SECTION.      
      *                                                            
       01  CAMPOS-DE-TRABALHO.
           05 WS-END-OF-FILE         PIC X(01)    VALUE 'N'.
           05 WS-REGION-DESC         PIC X(20).
           05 WS-FS-SIBS             PIC 9(02) VALUE ZEROS.
      *
           01  WS-DUMP-X             PIC X(01) VALUE SPACES.      
           01  WS-DUMP                    REDEFINES WS-DUMP-X     
                                     PIC S9(01) COMP-3.                 
      *                                                             
       PROCEDURE DIVISION.                                             
       A000-PRINCIPAL.
                                                                    
           PERFORM A100-INICIO                                   
              THRU A100-INICIO-FIM.                                   
      *                                                      
           PERFORM A200-PROCESSAMENTO                     
              THRU A200-PROCESSAMENTO-FIM 
                UNTIL WS-END-OF-FILE = 'S'                             
      *                                                                 
           PERFORM A300-FIM
              THRU A300-FIM-FIM.
      *        
       A100-INICIO. 
      *                                                     
           OPEN INPUT SIBS-FILE.
           IF WS-FS-SIBS NOT EQUAL ZEROS
              DISPLAY 'ERROR OPENING SIBS-FILE ' WS-FS-SIBS
              ADD WS-DUMP TO WS-DUMP. 
      *        
           PERFORM Q100-READ-SIBS-FILE.
      *  
       A100-INICIO-FIM.                                         
           EXIT.                                                
                                                                  
       A200-PROCESSAMENTO.                                    
      *                                                           
           DISPLAY 'TRANSACTION ID: ' TRANSACTION-ID.
           DISPLAY 'DATE: ' TRANSACTION-DATE.
           DISPLAY 'TIME: ' TRANSACTION-TIME.
           DISPLAY 'CARD NUMBER: ' CARD-NUMBER.
           DISPLAY 'AMOUNT: ' TRANSACTION-AMOUNT '€'.
           DISPLAY 'TYPE: ' TRANSACTION-CODE.
      *     
           PERFORM Q200-DESCRIPTON-CODE.
           DISPLAY 'REGION: ' WS-REGION-DESC.
      *
           DISPLAY 'STATUS: ' TRANSACTION-STATUS.
           DISPLAY '----------------------------------'.
      *     
           PERFORM Q100-READ-SIBS-FILE.
      *                                                  
       A200-PROCESSAMENTO-FIM.                           
           EXIT.
      *              
       A300-FIM.                                         
           CLOSE SIBS-FILE.
      *
           IF WS-FS-SIBS > ZEROS                                        
              DISPLAY 'CANCELADO POR ERROS NO SIBS-FILE ' UPON SYSOUT   
              ADD WS-DUMP    TO  WS-DUMP                               
           ELSE                                                        
              DISPLAY 'FIM NORMAL' UPON SYSOUT.                         
      *
       A300-FIM-FIM.
           EXIT.
      *
       Q100-READ-SIBS-FILE.
           READ SIBS-FILE INTO SIBS-RECORD
              AT END 
                  MOVE 'S' TO WS-END-OF-FILE.
      *
       Q100-READ-SIBS-FILE-FIM.
           EXIT.
      *
       Q200-DESCRIPTON-CODE.
           IF REGION-CODE = 'C'
              MOVE 'PORTUGAL CONTINENTAL' TO WS-REGION-DESC 
           ELSE
              IF REGION-CODE = 'A'
                MOVE 'AÇORES' TO WS-REGION-DESC
              ELSE
                 IF REGION-CODE = 'M'
                    MOVE 'MADEIRA' TO WS-REGION-DESC
                 ELSE
                    MOVE 'REGION UNKNOWN' TO WS-REGION-DESC.
      *
       Q200-DESCRIPTON-CODE-FIM.
           EXIT.
