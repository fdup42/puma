       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.         SCA19P.                                             
      ******************************************************************        
      *                                                                *        
      *        S C A 1 9 P  -  C A L C U L  REV / ASS / COT            *
      *                                                                *        
      *        E X E R C I C E    2 0 1 9  -  P U M A                  *
      *                                                                *
      *                                                                *        
      ******************************************************************
      * Ce sous-programme est spécifique pour l'exercice 2019. Il a    *
      * été inspiré des programmes frontaliers suisses (FS) et il sera *
      * dupliqué et adapté pour chaque exercice sur le même principe.  *
      * (si les règles de calcul changent d'une année à l'autre)       *
      ****************************************************************** 
      * Redmine 409636 - 10/2019 - Modification des formules de calcul.*
      * Modification des formules SP et AR.                            *
      * Utilisation de calculs intermédiares pourRevenus AR et SP      *
      ******************************************************************                                                                                
      * Entrée:                                                        *
      *--------                                                        *
      *     OPTION DE TRAITEMENT : CSCAFIP-OPTION                      *
      *     NUMERO COMPTE INTERNE: CSCAFIP-NO-CPT-INT                  *        
      *     ANNEE DES REVENUS    : CSCAFIP-ANNEE                       *
      *     CODE ORGANISME       : CSCAFIP-ORG                         *
      *                                                                *
      * Sortie:                                                        *
      *--------                                                        *
      *     MT REVENUS AR: CSCAFIP-MT-REV-AR                           *
      *     MT REVENUS RF: CSCAFIP-MT-REV-RF                           *
      *     MT REVENUS SP: CSCAFIP-MT-REV-SP                           *
      *     MT ASSIETTE  : CSCAFIP-MT-ASS-SOC                          *
      *     MT COTISATION: CSCAFIP-MT-COT                              *
      *     MT PASS      : CSCAFIP-MT-PASS                             *
      *                                                                *
      ******************************************************************
      * JPO - Redmine 477758 - 07/2020 - Création.
      * Nouveautés:                                                    *
      * - Nouvelles formules de calcul des revenus SP et AR            *
      ******************************************************************
       AUTHOR.             SAP-MONTPELLIER.                             
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION       SECTION.                                             
       SOURCE-COMPUTER.                                                         
U                          UNIX.                                                
       OBJECT-COMPUTER.                                                         
U                          UNIX.                                                
       DATA DIVISION.                                                           
       WORKING-STORAGE     SECTION.
      *                                                                         
      *****************************************************************
      * GESTION DES CODES ERREUR:                                     *
      *****************************************************************
       01  W-ERR-SCA19P.
           05  ERR-OPTION          PICTURE         X(10)       
                                   VALUE           "SCA19P1001".            
           05  ERR-NO-CPT-INT      PICTURE         X(10)       
                                   VALUE           "SCA19P1002".           
           05  ERR-ANNEE           PICTURE         X(10)       
                                   VALUE           "SCA19P1003".         
           05  ERR-ORG             PICTURE         X(10)       
                                   VALUE           "SCA19P1004".         
           05  ERR-XBASE-LECT      PICTURE         X(10)       
                                   VALUE           "SCA19P1005".               
           05  ERR-APPEL-SBAR01    PICTURE         X(10)       
                                   VALUE           "SCA19P1006".               
           05  ERR-NBPLDE          PICTURE         X(10)       
                                   VALUE           "SCA19P1007".               
           05  ERR-DECL            PICTURE         X(10)       
                                   VALUE           "SCA19P1008".               
           05  ERR-REVENU-SP       PICTURE         X(10)       
                                   VALUE           "SCA19P1009".
           05  ERR-FISC-VIDE       PICTURE         X(10)       
                                   VALUE           "SCA19P1010".
           05  ERR-REVENU-AR       PICTURE         X(10)       
                                   VALUE           "SCA19P1011".       
           05  ERR-INDREG          PICTURE         X(10)       
                                   VALUE           "SCA19P1012".               
      *****************************************************************       
      * MONTANTS pour CALCUL DES ASSIETTES                            *     
      *****************************************************************
      * (386 rubriques référencées)
       01  W-MT-RUB. 
           03  CIRVCA              PICTURE         S9(15).
           03  CIRVVA              PICTURE         S9(15).    
           03  MNRVKH              PICTURE         S9(15).
           03  NBPLDE              PICTURE         S9(15).    
           03  DECL                PICTURE         S9(15).
           03  INDREG              PICTURE         S9(15).
           03  1AA                 PICTURE         S9(15).
           03  1AJ                 PICTURE         S9(15).
           03  1AL                 PICTURE         S9(15).
           03  1AM                 PICTURE         S9(15).
           03  1AP                 PICTURE         S9(15).
           03  1AQ                 PICTURE         S9(15).
           03  1AS                 PICTURE         S9(15).
           03  1AT                 PICTURE         S9(15).
           03  1AW                 PICTURE         S9(15).
           03  1AZ                 PICTURE         S9(15).
           03  1BA                 PICTURE         S9(15).
           03  1BJ                 PICTURE         S9(15).
           03  1BL                 PICTURE         S9(15).
           03  1BM                 PICTURE         S9(15).
           03  1BP                 PICTURE         S9(15).
           03  1BQ                 PICTURE         S9(15).
           03  1BS                 PICTURE         S9(15).
           03  1BT                 PICTURE         S9(15).
           03  1BW                 PICTURE         S9(15).
           03  1BZ                 PICTURE         S9(15).
           03  1CW                 PICTURE         S9(15).
           03  1DN                 PICTURE         S9(15).
           03  1DW                 PICTURE         S9(15).
           03  1DY                 PICTURE         S9(15).
           03  1EY                 PICTURE         S9(15).
           03  1GB                 PICTURE         S9(15).
           03  1GF                 PICTURE         S9(15).
           03  1GG                 PICTURE         S9(15).
           03  1HB                 PICTURE         S9(15).
           03  1HF                 PICTURE         S9(15).
           03  1HG                 PICTURE         S9(15).
           03  1PM                 PICTURE         S9(15).
           03  1QM                 PICTURE         S9(15).
           03  1SM                 PICTURE         S9(15).
           03  1TP                 PICTURE         S9(15).
           03  1TT                 PICTURE         S9(15).
           03  1TX                 PICTURE         S9(15).
           03  1TZ                 PICTURE         S9(15).
           03  1UP                 PICTURE         S9(15).
           03  1UT                 PICTURE         S9(15).
           03  2BH                 PICTURE         S9(15).
           03  2CA                 PICTURE         S9(15).
           03  2CH                 PICTURE         S9(15).
           03  2DC                 PICTURE         S9(15).
           03  2DF                 PICTURE         S9(15).
           03  2DH                 PICTURE         S9(15).
           03  2DM                 PICTURE         S9(15).
           03  2EE                 PICTURE         S9(15).
           03  2FA                 PICTURE         S9(15).
           03  2FU                 PICTURE         S9(15).
           03  2GO                 PICTURE         S9(15).
           03  2RA                 PICTURE         S9(15).
           03  2RB                 PICTURE         S9(15).
           03  2RC                 PICTURE         S9(15).
           03  2RD                 PICTURE         S9(15).
           03  2TQ                 PICTURE         S9(15).
           03  2TR                 PICTURE         S9(15).
           03  2TS                 PICTURE         S9(15).
           03  2TT                 PICTURE         S9(15).
           03  2VM                 PICTURE         S9(15).
           03  2VN                 PICTURE         S9(15).
           03  2VO                 PICTURE         S9(15).
           03  2VP                 PICTURE         S9(15).
           03  2VV                 PICTURE         S9(15).
           03  2WW                 PICTURE         S9(15).
           03  2XX                 PICTURE         S9(15).
           03  2YY                 PICTURE         S9(15).
           03  2ZZ                 PICTURE         S9(15).
           03  3AN                 PICTURE         S9(15).
           03  3PI                 PICTURE         S9(15).
           03  3SA                 PICTURE         S9(15).
           03  3SE                 PICTURE         S9(15).
           03  3SG                 PICTURE         S9(15).
           03  3SJ                 PICTURE         S9(15).
           03  3SK                 PICTURE         S9(15).
           03  3SL                 PICTURE         S9(15).
           03  3SZ                 PICTURE         S9(15).
           03  3TJ                 PICTURE         S9(15).
           03  3TK                 PICTURE         S9(15).
           03  3TZ                 PICTURE         S9(15).
           03  3UA                 PICTURE         S9(15).
           03  3VA                 PICTURE         S9(15).
           03  3VC                 PICTURE         S9(15).
           03  3VD                 PICTURE         S9(15).
           03  3VF                 PICTURE         S9(15).
           03  3VG                 PICTURE         S9(15).
           03  3VH                 PICTURE         S9(15).
           03  3VI                 PICTURE         S9(15).
           03  3VJ                 PICTURE         S9(15).
           03  3VK                 PICTURE         S9(15).
           03  3VM                 PICTURE         S9(15).
           03  3VN                 PICTURE         S9(15).
           03  3VQ                 PICTURE         S9(15).
           03  3VR                 PICTURE         S9(15).
           03  3VT                 PICTURE         S9(15).
           03  3VZ                 PICTURE         S9(15).
           03  3WD                 PICTURE         S9(15).
           03  3WE                 PICTURE         S9(15).
           03  3WG                 PICTURE         S9(15).
           03  3WH                 PICTURE         S9(15).
           03  3WI                 PICTURE         S9(15).
           03  3WJ                 PICTURE         S9(15).
           03  3WN                 PICTURE         S9(15).
           03  3WP                 PICTURE         S9(15).
           03  4BA                 PICTURE         S9(15).
           03  4BB                 PICTURE         S9(15).
           03  4BC                 PICTURE         S9(15).
           03  4BD                 PICTURE         S9(15).
           03  4BE                 PICTURE         S9(15).
           03  4BF                 PICTURE         S9(15).
           03  5EY                 PICTURE         S9(15).
           03  5EZ                 PICTURE         S9(15).
           03  5FY                 PICTURE         S9(15).
           03  5FZ                 PICTURE         S9(15).
           03  5GA                 PICTURE         S9(15).
           03  5GB                 PICTURE         S9(15).
           03  5GC                 PICTURE         S9(15).
           03  5GD                 PICTURE         S9(15).
           03  5GE                 PICTURE         S9(15).
           03  5GF                 PICTURE         S9(15).
           03  5GG                 PICTURE         S9(15).
           03  5GH                 PICTURE         S9(15).
           03  5GI                 PICTURE         S9(15).
           03  5GJ                 PICTURE         S9(15).
           03  5HA                 PICTURE         S9(15).
           03  5HB                 PICTURE         S9(15).
           03  5HC                 PICTURE         S9(15).
           03  5HD                 PICTURE         S9(15).
           03  5HE                 PICTURE         S9(15).
           03  5HH                 PICTURE         S9(15).
           03  5HI                 PICTURE         S9(15).
           03  5HK                 PICTURE         S9(15).
           03  5HM                 PICTURE         S9(15).
           03  5HP                 PICTURE         S9(15).
           03  5HQ                 PICTURE         S9(15).
           03  5HR                 PICTURE         S9(15).
           03  5HS                 PICTURE         S9(15).
           03  5HT                 PICTURE         S9(15).
           03  5HV                 PICTURE         S9(15).
           03  5HW                 PICTURE         S9(15).
           03  5HX                 PICTURE         S9(15).
           03  5HY                 PICTURE         S9(15).
           03  5HZ                 PICTURE         S9(15).
           03  5IA                 PICTURE         S9(15).
           03  5IB                 PICTURE         S9(15).
           03  5IC                 PICTURE         S9(15).
           03  5ID                 PICTURE         S9(15).
           03  5IE                 PICTURE         S9(15).
           03  5IH                 PICTURE         S9(15).
           03  5II                 PICTURE         S9(15).
           03  5IK                 PICTURE         S9(15).
           03  5IM                 PICTURE         S9(15).
           03  5IP                 PICTURE         S9(15).
           03  5IQ                 PICTURE         S9(15).
           03  5IR                 PICTURE         S9(15).
           03  5IS                 PICTURE         S9(15).
           03  5IT                 PICTURE         S9(15).
           03  5IU                 PICTURE         S9(15).
           03  5IV                 PICTURE         S9(15).
           03  5IW                 PICTURE         S9(15).
           03  5IX                 PICTURE         S9(15).
           03  5IY                 PICTURE         S9(15).
           03  5IZ                 PICTURE         S9(15).
           03  5JG                 PICTURE         S9(15).
           03  5JJ                 PICTURE         S9(15).
           03  5JK                 PICTURE         S9(15).
           03  5JT                 PICTURE         S9(15).
           03  5JU                 PICTURE         S9(15).
           03  5KB                 PICTURE         S9(15).
           03  5KC                 PICTURE         S9(15).
           03  5KE                 PICTURE         S9(15).
           03  5KH                 PICTURE         S9(15).
           03  5KI                 PICTURE         S9(15).
           03  5KJ                 PICTURE         S9(15).
           03  5KK                 PICTURE         S9(15).
           03  5KM                 PICTURE         S9(15).
           03  5KN                 PICTURE         S9(15).
           03  5KO                 PICTURE         S9(15).
           03  5KP                 PICTURE         S9(15).
           03  5KQ                 PICTURE         S9(15).
           03  5KR                 PICTURE         S9(15).
           03  5KS                 PICTURE         S9(15).
           03  5KT                 PICTURE         S9(15).
           03  5KU                 PICTURE         S9(15).
           03  5KV                 PICTURE         S9(15).
           03  5KW                 PICTURE         S9(15).
           03  5KX                 PICTURE         S9(15).
           03  5KY                 PICTURE         S9(15).
           03  5KZ                 PICTURE         S9(15).
           03  5LA                 PICTURE         S9(15).
           03  5LB                 PICTURE         S9(15).
           03  5LC                 PICTURE         S9(15).
           03  5LD                 PICTURE         S9(15).
           03  5LE                 PICTURE         S9(15).
           03  5LH                 PICTURE         S9(15).
           03  5LI                 PICTURE         S9(15).
           03  5LJ                 PICTURE         S9(15).
           03  5LM                 PICTURE         S9(15).
           03  5LN                 PICTURE         S9(15).
           03  5LO                 PICTURE         S9(15).
           03  5LP                 PICTURE         S9(15).
           03  5LQ                 PICTURE         S9(15).
           03  5LR                 PICTURE         S9(15).
           03  5LS                 PICTURE         S9(15).
           03  5LT                 PICTURE         S9(15).
           03  5LU                 PICTURE         S9(15).
           03  5LV                 PICTURE         S9(15).
           03  5LW                 PICTURE         S9(15).
           03  5LX                 PICTURE         S9(15).
           03  5LY                 PICTURE         S9(15).
           03  5LZ                 PICTURE         S9(15).
           03  5MT                 PICTURE         S9(15).
           03  5NA                 PICTURE         S9(15).
           03  5NB                 PICTURE         S9(15).
           03  5NC                 PICTURE         S9(15).
           03  5ND                 PICTURE         S9(15).
           03  5NE                 PICTURE         S9(15).
           03  5NF                 PICTURE         S9(15).
           03  5NG                 PICTURE         S9(15).
           03  5NH                 PICTURE         S9(15).
           03  5NI                 PICTURE         S9(15).
           03  5NJ                 PICTURE         S9(15).
           03  5NK                 PICTURE         S9(15).
           03  5NL                 PICTURE         S9(15).
           03  5NM                 PICTURE         S9(15).
           03  5NN                 PICTURE         S9(15).
           03  5NO                 PICTURE         S9(15).
           03  5NP                 PICTURE         S9(15).
           03  5NQ                 PICTURE         S9(15).
           03  5NR                 PICTURE         S9(15).
           03  5NS                 PICTURE         S9(15).
           03  5NT                 PICTURE         S9(15).
           03  5NU                 PICTURE         S9(15).
           03  5NW                 PICTURE         S9(15).
           03  5NX                 PICTURE         S9(15).
           03  5NY                 PICTURE         S9(15).
           03  5NZ                 PICTURE         S9(15).
           03  5OA                 PICTURE         S9(15).
           03  5OB                 PICTURE         S9(15).
           03  5OC                 PICTURE         S9(15).
           03  5OD                 PICTURE         S9(15).
           03  5OE                 PICTURE         S9(15).
           03  5OF                 PICTURE         S9(15).
           03  5OG                 PICTURE         S9(15).
           03  5OH                 PICTURE         S9(15).
           03  5OI                 PICTURE         S9(15).
           03  5OJ                 PICTURE         S9(15).
           03  5OK                 PICTURE         S9(15).
           03  5OL                 PICTURE         S9(15).
           03  5OM                 PICTURE         S9(15).
           03  5ON                 PICTURE         S9(15).
           03  5OO                 PICTURE         S9(15).
           03  5OP                 PICTURE         S9(15).
           03  5OQ                 PICTURE         S9(15).
           03  5OR                 PICTURE         S9(15).
           03  5OW                 PICTURE         S9(15).
           03  5OX                 PICTURE         S9(15).
           03  5OY                 PICTURE         S9(15).
           03  5OZ                 PICTURE         S9(15).
           03  5QA                 PICTURE         S9(15).
           03  5QB                 PICTURE         S9(15).
           03  5QC                 PICTURE         S9(15).
           03  5QD                 PICTURE         S9(15).
           03  5QH                 PICTURE         S9(15).
           03  5QI                 PICTURE         S9(15).
           03  5QJ                 PICTURE         S9(15).
           03  5QL                 PICTURE         S9(15).
           03  5QM                 PICTURE         S9(15).
           03  5RA                 PICTURE         S9(15).
           03  5RB                 PICTURE         S9(15).
           03  5RC                 PICTURE         S9(15).
           03  5RD                 PICTURE         S9(15).
           03  5RF                 PICTURE         S9(15).
           03  5RG                 PICTURE         S9(15).
           03  5RH                 PICTURE         S9(15).
           03  5RI                 PICTURE         S9(15).
           03  5RJ                 PICTURE         S9(15).
           03  5RL                 PICTURE         S9(15).
           03  5RM                 PICTURE         S9(15).
           03  5RN                 PICTURE         S9(15).
           03  5RO                 PICTURE         S9(15).
           03  5RP                 PICTURE         S9(15).
           03  5RQ                 PICTURE         S9(15).
           03  5RR                 PICTURE         S9(15).
           03  5RW                 PICTURE         S9(15).
           03  5RZ                 PICTURE         S9(15).
           03  5SN                 PICTURE         S9(15).
           03  5SO                 PICTURE         S9(15).
           03  5SP                 PICTURE         S9(15).
           03  5SV                 PICTURE         S9(15).
           03  5SW                 PICTURE         S9(15).
           03  5TA                 PICTURE         S9(15).
           03  5TB                 PICTURE         S9(15).
           03  5TC                 PICTURE         S9(15).
           03  5TE                 PICTURE         S9(15).
           03  5TF                 PICTURE         S9(15).
           03  5TH                 PICTURE         S9(15).
           03  5TI                 PICTURE         S9(15).
           03  5UA                 PICTURE         S9(15).
           03  5UB                 PICTURE         S9(15).
           03  5UC                 PICTURE         S9(15).
           03  5UE                 PICTURE         S9(15).
           03  5UF                 PICTURE         S9(15).
           03  5UH                 PICTURE         S9(15).
           03  5UI                 PICTURE         S9(15).
           03  5VI                 PICTURE         S9(15).
           03  5XA                 PICTURE         S9(15).
           03  5XB                 PICTURE         S9(15).
           03  5XN                 PICTURE         S9(15).
           03  5XO                 PICTURE         S9(15).
           03  5XT                 PICTURE         S9(15).
           03  5XU                 PICTURE         S9(15).
           03  5XV                 PICTURE         S9(15).
           03  5XW                 PICTURE         S9(15).
           03  5YA                 PICTURE         S9(15).
           03  5YB                 PICTURE         S9(15).
           03  5YN                 PICTURE         S9(15).
           03  5YO                 PICTURE         S9(15).
           03  6DE                 PICTURE         S9(15).
           03  8BY                 PICTURE         S9(15).
           03  8CY                 PICTURE         S9(15).
           03  8FV                 PICTURE         S9(15).
           03  CHC                 PICTURE         S9(15).
           03  CHI                 PICTURE         S9(15).
           03  CIC                 PICTURE         S9(15).
           03  CII                 PICTURE         S9(15).
           03  DAJ                 PICTURE         S9(15).
           03  DBJ                 PICTURE         S9(15).
           03  EAJ                 PICTURE         S9(15).
           03  EBJ                 PICTURE         S9(15).
           03  FAS                 PICTURE         S9(15).
           03  FBS                 PICTURE         S9(15).
           03  NAJ                 PICTURE         S9(15).
           03  NAP                 PICTURE         S9(15).
           03  NAS                 PICTURE         S9(15).
           03  NAW                 PICTURE         S9(15).
           03  NAZ                 PICTURE         S9(15).
           03  NBA                 PICTURE         S9(15).
           03  NBJ                 PICTURE         S9(15).
           03  NBP                 PICTURE         S9(15).
           03  NBS                 PICTURE         S9(15).
           03  NBW                 PICTURE         S9(15).
           03  NBZ                 PICTURE         S9(15).
           03  NCH                 PICTURE         S9(15).
           03  NCW                 PICTURE         S9(15).
           03  NDC                 PICTURE         S9(15).
           03  NDW                 PICTURE         S9(15).
           03  NFU                 PICTURE         S9(15).
           03  NGO                 PICTURE         S9(15).
           03  NTR                 PICTURE         S9(15).
           03  NTS                 PICTURE         S9(15).
           03  NUA                 PICTURE         S9(15).
           03  NVG                 PICTURE         S9(15).
           03  RAJ                 PICTURE         S9(15).
           03  RAL                 PICTURE         S9(15).
           03  RAM                 PICTURE         S9(15).
           03  RAP                 PICTURE         S9(15).
           03  RAS                 PICTURE         S9(15).
           03  RAW                 PICTURE         S9(15).
           03  RAZ                 PICTURE         S9(15).
           03  RBA                 PICTURE         S9(15).
           03  RBJ                 PICTURE         S9(15).
           03  RBL                 PICTURE         S9(15).
           03  RBM                 PICTURE         S9(15).
           03  RBP                 PICTURE         S9(15).
           03  RBS                 PICTURE         S9(15).
           03  RBW                 PICTURE         S9(15).
           03  RBZ                 PICTURE         S9(15).
           03  RCH                 PICTURE         S9(15).
           03  RCW                 PICTURE         S9(15).
           03  RDC                 PICTURE         S9(15).
           03  RDW                 PICTURE         S9(15).
           03  RFU                 PICTURE         S9(15).
           03  RGO                 PICTURE         S9(15).
           03  RTR                 PICTURE         S9(15).
           03  RTS                 PICTURE         S9(15).
           03  RUA                 PICTURE         S9(15).
           03  RVG                 PICTURE         S9(15).
           03  SBA                 PICTURE         S9(15).
           03  TBA                 PICTURE         S9(15).
      *
      ***************************************************************** 
      *  MONTANTS temporaires pour les différents calculs             *
      *****************************************************************
       01  W-CALCUL.
           03  W-REV-SP            PICTURE         S9(15).  
           03  W-REV-AR            PICTURE         S9(15). 
           03  W-REV-RR-1          PICTURE         S9(15). 
           03  W-REV-RR-2          PICTURE         S9(15). 
           03  W-ASS-SOC           PICTURE         S9(15). 
           03  W-ASS-SOC-2         PICTURE         S9(15). 
           03  W-COT               PICTURE         S9(15). 
           03  W-PASS              PICTURE         9(7).
           03  W-ABATTEMENT        PICTURE         9(7).
           03  W-PLAFOND           PICTURE         9(7).
           03  W-ASS-MIN           PICTURE         S9(15). 
           
      * elements calcul internediaire Revenus AR 
       01  W-CALCUL-INT-AR.
           03 W-REV-AR-INT                          PICTURE S9(15)V99. 
           03 V-1AW-RAW-NAW                         PICTURE S9(15)V99.
           03 V-1BW-RBW-NBW                         PICTURE S9(15)V99.
           03 V-1CW-RCW-NCW                         PICTURE S9(15)V99.
           03 V-1DW-RDW-NDW                         PICTURE S9(15)V99.
           03 V-5NA-5NK-5NM-5KM-5NY-5NZ             PICTURE S9(15)V99.
           03 V-5KQ-5KR-5NX-5IU-5NQ-5NR             PICTURE S9(15)V99.
           03 V-5HR-5HS                             PICTURE S9(15)V99.
           03 V-5KY-5JU                             PICTURE S9(15)V99.
           03 V-5KV-5KW                             PICTURE S9(15)V99.
           03 V-RDC-NDC                             PICTURE S9(15)V99.
           03 V-RFU-NFU                             PICTURE S9(15)V99.
           03 V-2RB-2RC-2RD                         PICTURE S9(15)V99.
           03 V-RTS-NTS                             PICTURE S9(15)V99.
           03 V-RTR-NTR                             PICTURE S9(15)V99.
           03 V-RGO-NGO                             PICTURE S9(15)V99.
           03 V-RVG-NVG                             PICTURE S9(15)V99.
           03 V-RUA-NUA                             PICTURE S9(15)V99.
           03 V-3TJ-3TK                             PICTURE S9(15)V99.
           03 V-RBA-NBA                             PICTURE S9(15)V99.
           03 V-SBA-TBA                             PICTURE S9(15)V99.
           03 V-3UA-3VG-3VQ-RVG-NVG-RUA-NUA-3VR     PICTURE S9(15)V99.
           03 V-4BE-4BA-RBA-NBA-SBA-TBA-4BB-4BC-4BD PICTURE S9(15)V99.
           03 V-5OA-5OK-5OM-5LM-5OY-5OZ             PICTURE S9(15)V99.
           03 V-5LQ-5LR-5OX-5RZ-5OQ-5OR             PICTURE S9(15)V99.
           03 V-5LY-5LD-5LV-5LW                     PICTURE S9(15)V99.
           03 V-5IR-5IS                             PICTURE S9(15)V99.
           03 V-COEF                                PICTURE 9(1).

      * elements calcul internediaire Revenus AR 
      *
       01  W-CALCUL-INT-SP.     
           03 V-RAJ-NAJ                             PICTURE S9(15)V99. 
           03 V-5HX-5XN-5HE                         PICTURE S9(15)V99.
           03 V-5KN-5KO-5KP-5KX-5KJ                 PICTURE S9(15)V99.
           03 V-5HP-5HQ-5HV-5KZ                     PICTURE S9(15)V99.
           03 V-RBJ-NBJ                             PICTURE S9(15)V99.
           03 V-5IX-5YN-5IE                         PICTURE S9(15)V99.
           03 V-5LN-5LO-5LP-5LX-5LJ                 PICTURE S9(15)V99.
           03 V-5IP-5IQ-5IV-5LZ                     PICTURE S9(15)V99.
      *     
      * Flags de contrôle ou trace    
       01  W-FLAG.
           05  FLAG-MNRVKH         PICTURE         9(1).
           05  FLAG-FISC-VIDE      PICTURE         9(1).
               88 FISC-VIDE        VALUE 0.
               88 FISC-NON-VIDE    VALUE 1.
      *
      *****************************************************************
      * COPY des constantes:                                          *
      *****************************************************************
           COPY    CCONST.
      *
      *****************************************************************       
      * COPY table FISC                                               *
      *****************************************************************       
           COPY    V2FISC.
      *                                                                       
      *****************************************************************       
      * COPY XBASE                                                    *
      *****************************************************************       
           COPY    V2TOTAL.                                                     
           05  TOT-AREA            PICTURE         X(400)                       
                                   VALUE           SPACE.                       
      *     
      *****************************************************************        
      * COPY du sous programme SBAR01 pour récupérer le PASS          *
      *****************************************************************        
           COPY    CBAR01.                                              
      *                                                                         
       LINKAGE SECTION.                                                         
      *                    
      *****************************************************************        
      *    INTERFACE DU SOUS PGM SCA19P                               *  
      *****************************************************************        
           COPY    CSCAFIP.                                                    
           COPY    DTFLIEN.                                                     
      *===============================================================         
       PROCEDURE DIVISION USING WSS-CSCAFIP DTFLNK                             
           DTFINP DTFWRK DTFCOM.
      *===============================================================
      *---------------------------------------------------------------*
      * Initialisation des variables                                  *
      *---------------------------------------------------------------*
        PERFORM   INITIALISATION
      *
      *---------------------------------------------------------------*
      * Contrôle des zones passées par le programme appelant          *
      *---------------------------------------------------------------*
        PERFORM   CONTROLE-PARAM
      *
      *---------------------------------------------------------------*
      * Recherche du PASS utilisé pour les calculs                    *
      *---------------------------------------------------------------*
        IF CSCAFIP-CD-RET = CCONST-TRAITEMENT-OK      
           PERFORM   RECHERCHE-PASS
        END-IF
      *
      *---------------------------------------------------------------*
      * Lecture table FISC pour récupération de l'ensemble des        *
      * rubriques et montants du compte traité pour l'année traitée   *
      *---------------------------------------------------------------*
        IF CSCAFIP-CD-RET = CCONST-TRAITEMENT-OK
           PERFORM PREP-LECTURE-FISC
      *
           PERFORM LECTURE-FISC
      *
           PERFORM UNTIL TOT-STAT  = "MRNF"
                      OR TOT-REFER = "END."
      *       Chargement des valeurs de donnée lues dans la 
      *       variable de working correspondante
              PERFORM   CHARGEMENT-MONTANTS
      *
      *       Lecture suivante de la table FISC
              PERFORM  LECTURE-FISC
           END-PERFORM
        END-IF
      *
      *---------------------------------------------------------------*
      * Controle des données lues dans la table FISC                  *
      *---------------------------------------------------------------*
        IF CSCAFIP-CD-RET = CCONST-TRAITEMENT-OK
           PERFORM CONTROLE-DONNEES-FISC
        END-IF
      *
      *---------------------------------------------------------------*
      * Calcul du revenu AR (autres revenus)
      *---------------------------------------------------------------*
        IF CSCAFIP-CD-RET = CCONST-TRAITEMENT-OK
           PERFORM CALCUL-REV-AR
        END-IF
      *
      *---------------------------------------------------------------*
      * Calcul du revenu SP (Salaire et pensions)
      *---------------------------------------------------------------*
        IF CSCAFIP-CD-RET = CCONST-TRAITEMENT-OK
           PERFORM CALCUL-REV-SP
        END-IF
      *
      *---------------------------------------------------------------*
      * Calcul du revenu RR (Revenu de remplacement)
      *---------------------------------------------------------------*
        IF CSCAFIP-CD-RET = CCONST-TRAITEMENT-OK
           PERFORM CALCUL-REV-RR
        END-IF
      *
      *---------------------------------------------------------------*
      * Calcul de l'assiette minimum de cotisation
      *---------------------------------------------------------------*
        IF CSCAFIP-CD-RET = CCONST-TRAITEMENT-OK
           PERFORM CALCUL-ASSIETTE-MINIMUM
        END-IF
      *
      *---------------------------------------------------------------*
      * Calcul de l'assiette de cotisation
      *---------------------------------------------------------------*
        IF CSCAFIP-CD-RET = CCONST-TRAITEMENT-OK
           PERFORM CALCUL-ASSIETTE
        END-IF
      *
      *---------------------------------------------------------------*
      * Calcul de la cotisation annuelle   
      *---------------------------------------------------------------*
        IF CSCAFIP-CD-RET = CCONST-TRAITEMENT-OK
           PERFORM   CALCUL-COTISATIONS
        END-IF
      *  
      *---------------------------------------------------------------*
      * Transfert des calculs en linkage
      *---------------------------------------------------------------*
        IF CSCAFIP-CD-RET = CCONST-TRAITEMENT-OK
           PERFORM   TRANSFERT-RESULTATS
        END-IF
            
        EXIT PROGRAM
        .
      *****************************************************************
      * Initialisation des variables                                  *
      *****************************************************************
       INITIALISATION. 
      *---------------      
        INITIALIZE W-MT-RUB W-CALCUL W-FLAG   
        INITIALIZE CSCAFIP-SORTIE                               
        MOVE CCONST-TRAITEMENT-OK TO CSCAFIP-CD-RET
        .
      *****************************************************************
      * Controle des zones passées par le programme appelant          *
      *****************************************************************
        CONTROLE-PARAM. 
      * --------------
      *
      * Contrôle OPTION de traitement:                 
        IF NOT CSCAFIP-CALCUL   
           MOVE ERR-OPTION          TO CSCAFIP-CD-RET    
           MOVE CSCAFIP-OPTION      TO CSCAFIP-PARAM1
        END-IF                                                      
      *                                                              
      * Contrôle COMPTE:                                             
        IF CSCAFIP-CD-RET = CCONST-TRAITEMENT-OK
        AND ( CSCAFIP-NO-CPT-INT NOT NUMERIC
           OR CSCAFIP-NO-CPT-INT NOT > 0 
            )
           MOVE ERR-NO-CPT-INT      TO CSCAFIP-CD-RET                
           MOVE CSCAFIP-NO-CPT-INT  TO CSCAFIP-PARAM1
        END-IF                                                      
      *                                                              
      * Contrôle ANNEE:                                              
        IF CSCAFIP-CD-RET = CCONST-TRAITEMENT-OK                  
        AND ( CSCAFIP-ANNEE NOT NUMERIC   
           OR CSCAFIP-ANNEE < 2018 
            )
           MOVE ERR-ANNEE           TO CSCAFIP-CD-RET                
           MOVE CSCAFIP-ANNEE       TO CSCAFIP-PARAM1
        END-IF  
      *                                                              
      * Contrôle ORGANISME:                                          
        IF CSCAFIP-CD-RET = CCONST-TRAITEMENT-OK                  
        AND ( CSCAFIP-ORG NOT NUMERIC   
           OR CSCAFIP-ORG NOT > 0  
            )
           MOVE ERR-ORG             TO CSCAFIP-CD-RET                
           MOVE CSCAFIP-ORG         TO CSCAFIP-PARAM1
        END-IF
        .
      *****************************************************************        
      * Recherche PASS via le sous programme existant SBAR01          *
      * (nb: PASS identique entre TI/FS/PUMA)                         *
      *****************************************************************        
        RECHERCHE-PASS.                                                         
      * --------------      
        INITIALIZE WSS-CBAR01
        MOVE CSCAFIP-ORG       TO CBAR01-E-ORG
        MOVE CSCAFIP-ANNEE     TO CBAR01-E-ANNEE
      *
        CALL    "SBAR01"  USING   WSS-CBAR01
                                  DTFLNK
                                  DTFINP
                                  DTFWRK
                                  DTFCOM
      *                              
        IF CBAR01-S-CDRET NOT = 0 
          MOVE ERR-APPEL-SBAR01 TO CSCAFIP-CD-RET
          MOVE CBAR01-S-CDRET   TO CSCAFIP-PARAM1
        ELSE
          MOVE CBAR01-S-PLAF-SECU (1) TO W-PASS
        END-IF
        .
      *****************************************************************        
      * INITIALISATION DE LA LECTURE DE LA TABLE FISC                 *      
      *****************************************************************        
       PREP-LECTURE-FISC.                                                            
      *-----------------      
        MOVE SPACE              TO V2TOTAL TOT-MINI6 
        MOVE CSCAFIP-NO-CPT-INT TO TOT-KEY-ASS
        MOVE CSCAFIP-ANNEE      TO TOT-KEY
        .
      *****************************************************************        
      * LECTURE TABLE FISC                                            *      
      *****************************************************************        
       LECTURE-FISC.                                                            
      *------------      
      * Lecture séquentielle
        MOVE "CPTE-CLE"         TO TOT-PRP-ASS.                              
        MOVE "AA-REV"           TO TOT-PRP.
        MOVE "V"                TO TOT-SENS.                                 
        MOVE "FISC"             TO TOT-FILE.                                 
        MOVE "READS"            TO TOT-FUNC.                                 
        MOVE "S"                TO TOT-AIG-X.                                
        MOVE SPACE              TO TOT-LIKE.                                 
        MOVE "MRNF"             TO TOT-STAT1.                                
        MOVE "RLSE"             TO TOT-ENDP. 
      *                                                                 
                    PERFORM     APPEL-XBASE                                  
      *
      * Test code lecture OK (car sortie possible sur "MNRF" ou "END")
        IF TOT-STAT = "****"
           MOVE TOT-AREA           TO V2FISC   
      *
      * Si au moins une lecture alors table non vide
      * pour le couple compte/année
           SET FISC-NON-VIDE TO TRUE
      *
        END-IF
       .
      *****************************************************************        
      * CONTROLE DES DONNEES LUES DANS FISC                           *      
      *****************************************************************        
       CONTROLE-DONNEES-FISC.                                                            
      *---------------------      
      *---------------------------------------------------------------*
      * Contrôle si la table FISC est vide. Si oui alors sortie en    *
      * erreur (non bloquante). Le compte sera créé avec un code      *
      * particularité 4 dans ELEF ('Affiliation en attente').         *
      *---------------------------------------------------------------*
        IF CSCAFIP-CD-RET = CCONST-TRAITEMENT-OK
      *
           IF FISC-VIDE
              MOVE ERR-FISC-VIDE      TO CSCAFIP-CD-RET
              MOVE CSCAFIP-NO-CPT-INT TO CSCAFIP-PARAM1         
              MOVE CSCAFIP-ANNEE      TO CSCAFIP-PARAM2
           END-IF
      *
        END-IF
      *---------------------------------------------------------------*
      * Contrôle valeur NBPLDE (qui conditionne les calculs ci-après) * 
      * NBPLDE doit etre 1 ou 2 sinon erreur. Idem pour DECL.         *
      * Remarque: à priori ces contrôles sont faits en amont lors de  *
      * l'intégration des rubriques (via SMCMFI). Ajouter ici en      *
      * prévention si besoin de faire appel via autre canal ...       *
      *---------------------------------------------------------------*
        IF CSCAFIP-CD-RET = CCONST-TRAITEMENT-OK
      *
           IF NBPLDE NOT = 1 AND NBPLDE NOT = 2 
              MOVE ERR-NBPLDE         TO CSCAFIP-CD-RET
              MOVE CSCAFIP-NO-CPT-INT TO CSCAFIP-PARAM1         
              MOVE CSCAFIP-ANNEE      TO CSCAFIP-PARAM2
           END-IF
      *
        END-IF
      *  
        IF CSCAFIP-CD-RET = CCONST-TRAITEMENT-OK
      *
           IF DECL NOT = 1 AND DECL NOT = 2 
              MOVE ERR-DECL           TO CSCAFIP-CD-RET
              MOVE CSCAFIP-NO-CPT-INT TO CSCAFIP-PARAM1         
              MOVE CSCAFIP-ANNEE      TO CSCAFIP-PARAM2
           END-IF
      *
        END-IF
      *---------------------------------------------------------------*
      * Contrôle valeur INDREG indicateur régime BIC, BNC, ...        * 
      * INDREG doit etre égal à 0,1,2,3,4                             *
      *---------------------------------------------------------------*
        IF CSCAFIP-CD-RET = CCONST-TRAITEMENT-OK
      *
           IF  INDREG NOT = 0 AND INDREG NOT = 1 
           AND INDREG NOT = 2 AND INDREG NOT = 3 AND INDREG NOT = 4 
              MOVE ERR-INDREG         TO CSCAFIP-CD-RET
              MOVE CSCAFIP-NO-CPT-INT TO CSCAFIP-PARAM1         
              MOVE CSCAFIP-ANNEE      TO CSCAFIP-PARAM2
           END-IF
        END-IF
        .
      *****************************************************************
      * Calcul du revenu AR (Autres Revenus) --> DSF_OBDE_F_001552    *
      *                                      --> DSF_ODBE_F_001994    *
      *****************************************************************
       CALCUL-REV-AR.
      *-------------
      *
        PERFORM CALCUL-ELEMENTS-REV-AR

        IF DECL = 1
           COMPUTE W-REV-AR  ROUNDED = 
                               3VJ
                             + 5HY
                             + 5ND*(1 - 0.50) 
                             + 5NW*(1 - 0.50)
                             + 5NG*(1 - 0.71) 
                             + 5NJ*(1 - 0.71)
                             + V-5NA-5NK-5NM-5KM-5NY-5NZ
                             + V-5KQ-5KR-5NX-5IU-5NQ-5NR 
                             + 5NE 
                             + 5TF
                             + V-5KY-5JU
                             + V-5KV-5KW 
                             + 5TC 
                             + 5QJ
                             + 5SO
                             + 5SV   
                             + V-5HR-5HS
        END-IF
      
        IF DECL = 2
           COMPUTE W-REV-AR  ROUNDED = 
                               3VK
                             + 5IY
                             + 5OD*(1 - 0.50) 
                             + 5OW*(1 - 0.50) 
                             + 5OG*(1 - 0.71) 
                             + 5OJ*(1 - 0.71) 
                             + V-5OA-5OK-5OM-5LM-5OY-5OZ
                             + V-5LQ-5LR-5OX-5RZ-5OQ-5OR
                             + 5OE 
                             + 5UF
                             + V-5LY-5LD-5LV-5LW 
                             + 5UC 
                             + 5RJ 
                             + 5NT 
                             + 5SW 
                             + V-5IR-5IS 
        END-IF
      * Ajout de la partie commune divisée par le nombre de decl.
        COMPUTE W-REV-AR  ROUNDED = 
                            W-REV-AR
                          + 
                          ( V-1AW-RAW-NAW
                          + V-1BW-RBW-NBW
                          + V-1CW-RCW-NCW
                          + V-1DW-RDW-NDW
                          + 2DH + 2EE + 2XX + 2VM + 2RA + 2DC
                          + V-RDC-NDC
                          + 2FU 
                          + V-RFU-NFU
                          + V-2RB-2RC-2RD
                          + W-REV-AR-INT
                          + 2TS 
                          + V-RTS-NTS
                          + 2TR 
                          + V-RTR-NTR 
                          + 2GO 
                          + V-RGO-NGO
                          + 2DM + 2TT + 2TQ + 2YY + 2ZZ 
                         + 2VN + 2VO + 2VP
                          + V-3UA-3VG-3VQ-RVG-NVG-RUA-NUA-3VR
                          + 3VD + 3VI + 3VF + 3PI + 3WH + 3WI                  
                          + 3WJ + 3WP + 3SJ
                          + V-3TJ-3TK 
                          + 3SK + 3VT + 3VC + 3SE + 3WE + 3AN + 3VZ  
                          + V-4BE-4BA-RBA-NBA-SBA-TBA-4BB-4BC-4BD
                          - 2DF * 0.068
                          - 6DE
                          ) / NBPLDE
                          
      * Si Revenu AR <= PASS * 0.50 --> non éligible PUMA 
      * DSF_ODBE_F_004983 
      *----------------------------------------------------------------
        IF W-REV-AR <= W-PASS * 0.50
          MOVE ERR-REVENU-AR TO CSCAFIP-CD-RET    
        END-IF             
      *
      * Pas de montant négatif
      *----------------------------------------------------------------
        IF W-REV-AR < 0
          MOVE 0 TO W-REV-AR
        END-IF
        .
      *****************************************************************
      * Calcul du revenu SP (Salaires/Pensions) --> DSF_OBDE_F_001551 *
      *                                         --> DSF_ODBE_F_001993 *
      *****************************************************************
       CALCUL-REV-SP.
      *--------------
        PERFORM CALCUL-ELEMENTS-REV-SP
       
        IF DECL = 1
           COMPUTE W-REV-SP  ROUNDED = 1AJ 
                                     + V-RAJ-NAJ 
                                     + DAJ / 4 
                                     + EAJ / 4 
                                     + 1GB + 1AA + 1GF + 1GG
                                     + 1TP + 1PM + 1AQ 
                                     + 1TT + 1TZ + 1DY + 1SM
                                     + 5TA * (1 - 0.71) 
                                     + 5TB * (1 - 0.50) 
                                     + 5TE * (1 - 0.34) 
                                     + 5HD  
                                     + (5XA + 5XB) * 0.13
                                     + 5HW - 5XO 
                                     + 5HH + 5HB + 5HC 
                                     + 5HI + 5HM + 5HZ
                                     + 5XT + 5XV 
                                     + V-5HX-5XN-5HE
                                     + 5HA
                                     + V-5KN-5KO-5KP-5KX-5KJ
                                     + 5KB + 5KC + 5KH + 5KI
                                     + 5UI + 5KE
                                     + V-5HP-5HQ-5HV-5KZ
                                     + 5QB + 5QC + 5QH 
                                     + 5QI + 5QL + 5QM
                                     + 5QA + 5QD
        END-IF
        IF DECL = 2
           COMPUTE W-REV-SP  ROUNDED = 1BJ 
                                     + V-RBJ-NBJ 
                                     + DBJ / 4 
                                     + EBJ / 4 
                                     + 1HB + 1BA + 1HF + 1HG
                                     + 1UP + 1QM + 1BQ
                                     + 1UT + 1EY + 1DN 
                                     + 5UA * (1 - 0.71)
                                     + 5UB * (1 - 0.5) 
                                     + 5UE * (1 - 0.34) 
                                     + 5ID
                                     + (5YA + 5YB) * 0.13                                  
                                     + 5IW - 5YO  
                                     + 5IB + 5IC + 5IH 
                                     + 5II + 5IM + 5IZ 
                                     + 5XU + 5XW
                                     + V-5IX-5YN-5IE
                                     + 5IA
                                     + V-5LN-5LO-5LP-5LX-5LJ
                                     + 5LB + 5LC + 5LH 
                                     + 5LI + 5LE + 5VI
                                     + V-5IP-5IQ-5IV-5LZ
                                     + 5RB + 5RC + 5RH 
                                     + 5RI + 5RL + 5RM
                                     + 5RD + 5RA
        END-IF

      * Si Revenu SP > PASS * 20% --> non éligible PUMA 
      * DSF_ODBE_F_001995 
      *----------------------------------------------------------------
        IF W-REV-SP > W-PASS * 0.20
          MOVE ERR-REVENU-SP TO CSCAFIP-CD-RET    
        END-IF
      *
      * Pas de montant négatif
      *----------------------------------------------------------------
        IF W-REV-SP < 0
          MOVE 0 TO W-REV-SP
        END-IF
        .
      *****************************************************************
      * Calcul du revenu RR (remplacement) --> DSF_OBDE_F_002934      *
      *                                    --> DSF_ODBE_F_002935      *
      *****************************************************************
       CALCUL-REV-RR.
      *-------------
      * - Calcul du revenu RR de chaque déclarant
      *
      * DSF_OBDE_F_002934
        COMPUTE W-REV-RR-1 ROUNDED = 
                    1AP + RAP + 1AS + RAS + FAS + 1AL
                  + 1AM + RAL + RAM + 1AT + 1AZ + RAZ + 8FV
       
      * DSF_ODBE_F_002935
        COMPUTE W-REV-RR-2 ROUNDED =
                    1BP + RBP + 1BS + RBS + FBS + 1BL
                  + 1BM + RBL + RBM + 1BT + 1BZ + RBZ
      *
      * Pas de montant négatif
      *----------------------------------------------------------------
        IF W-REV-RR-1 < 0
          MOVE 0 TO W-REV-RR-1
        END-IF
        IF W-REV-RR-2 < 0
          MOVE 0 TO W-REV-RR-2
        END-IF
        .
      *****************************************************************
      * Calcul de l'assiette minimum selon l'indicateur BIC, BNC      *
      *****************************************************************
       CALCUL-ASSIETTE-MINIMUM.
      *-----------------------
        EVALUATE INDREG
           WHEN 1
           WHEN 2
           WHEN 3
           WHEN 4
              COMPUTE W-ASS-MIN ROUNDED = W-PASS * 0.115
           WHEN 0
              COMPUTE W-ASS-MIN ROUNDED = W-PASS * 0.20
        END-EVALUATE
      *
      * Pas de montant négatif
      *----------------------------------------------------------------
        IF W-ASS-SOC < 0
          MOVE 0 TO W-ASS-SOC
        END-IF
        .
      *****************************************************************
      * Calcul de l'assiette : revenus AR - abattement de 50% du PASS *
      *****************************************************************
       CALCUL-ASSIETTE.
      *---------------
        COMPUTE W-ABATTEMENT ROUNDED = W-PASS * 0.5
        COMPUTE W-PLAFOND    ROUNDED = W-PASS * 8
        IF W-REV-AR < W-PLAFOND
           COMPUTE W-ASS-SOC    = W-REV-AR  - W-ABATTEMENT
        ELSE
           COMPUTE W-ASS-SOC    = W-PLAFOND - W-ABATTEMENT
        END-IF
      *
      * Application d'un taux dégressif
        IF INDREG = 1 OR 2 OR 3 OR 4
           IF W-REV-SP > W-ASS-MIN
              COMPUTE W-ASS-SOC-2  = W-ASS-SOC
                                   * (1 - W-REV-SP  / (0.2 * W-PASS))
           ELSE
              COMPUTE W-ASS-SOC-2  = W-ASS-SOC
                                   * (1 - W-ASS-MIN / (0.2 * W-PASS))
           END-IF
        END-IF
        IF INDREG = 0
              COMPUTE W-ASS-SOC-2  = W-ASS-SOC
                                   * (1 - W-REV-SP  / (0.2 * W-PASS))
        END-IF
      *
      * Pas de montant négatif
      *----------------------------------------------------------------
        IF W-ASS-SOC   < 0
          MOVE 0 TO W-ASS-SOC
        END-IF
        IF W-ASS-SOC-2 < 0
          MOVE 0 TO W-ASS-SOC-2
        END-IF
        .
      *****************************************************************
      * Calcul cotisation annuelle maladie -->                        *
      * DSF_OBDE_F_001559 DSF_OBDE_F_002934 DSF_OBDE_F_002935         *
      *****************************************************************
       CALCUL-COTISATIONS.
      *------------------     
        COMPUTE W-COT ROUNDED = W-ASS-SOC-2 * 0.065
      *
      * Pas de cotisation si revenu de remplacement supérieur à 0          
      *----------------------------------------------------------------
        IF  NBPLDE = 1
        AND W-REV-RR-1 > 0
           MOVE 0      TO W-COT
        END-IF
      *
        IF  NBPLDE = 2
        AND (W-REV-RR-1 > 0 OR W-REV-RR-2 > 0)
           MOVE 0      TO W-COT
        END-IF
      *
      * Pas de montant négatif
      *----------------------------------------------------------------
      *
        IF W-COT < 0
           MOVE 0      TO W-COT
        END-IF
        .
      *****************************************************************
      * Chargement de la zone de communication avec les résultats de  *
      * calcul                                                        *
      *****************************************************************
       TRANSFERT-RESULTATS.
      * NB: le revenu fiscal RF est simplement = à MNRVKH
      * (si MNRVKH absent de la table FISC alors le montant sera égal
      * à NULL en base, pour différencier d'un revenu vraiment = 0 )
        IF FLAG-MNRVKH = 1
           MOVE MNRVKH    TO CSCAFIP-MT-REV-RF
        ELSE
           MOVE SPACE     TO CSCAFIP-MT-REV-RF-X
        END-IF
      *
        MOVE INDREG      TO CSCAFIP-INDREG
        MOVE W-ASS-MIN   TO CSCAFIP-MT-ASS-MIN
        MOVE W-REV-AR    TO CSCAFIP-MT-REV-AR
        MOVE W-REV-SP    TO CSCAFIP-MT-REV-SP      
        MOVE W-ASS-SOC   TO CSCAFIP-MT-ASS-SOC
        MOVE W-ASS-SOC-2 TO CSCAFIP-MT-ASS-SOC-2
        MOVE W-PASS      TO CSCAFIP-MT-PASS
        MOVE W-COT       TO CSCAFIP-MT-COT
        .
      *****************************************************************        
      * Chargement des montants des rubriques fiscales pour calcul    *
      * des assiettes et des revenus                                  *
      *****************************************************************        
       CHARGEMENT-MONTANTS.                                                     
      *-------------------
      *     
        EVALUATE FISC-TYP-MT-FISC
           WHEN "CIRVCA"    MOVE FISC-MT-FISC TO CIRVCA
           WHEN "CIRVVA"    MOVE FISC-MT-FISC TO CIRVVA
           WHEN "MNRVKH"    MOVE FISC-MT-FISC TO MNRVKH
                            MOVE 1            TO FLAG-MNRVKH
           WHEN "NBPLDE"    MOVE FISC-MT-FISC TO NBPLDE
           WHEN "DECL"      MOVE FISC-MT-FISC TO DECL
           WHEN "INDREG"    MOVE FISC-MT-FISC TO INDREG
           WHEN "1AA"       MOVE FISC-MT-FISC TO 1AA
           WHEN "1AJ"       MOVE FISC-MT-FISC TO 1AJ
           WHEN "1AL"       MOVE FISC-MT-FISC TO 1AL
           WHEN "1AM"       MOVE FISC-MT-FISC TO 1AM
           WHEN "1AP"       MOVE FISC-MT-FISC TO 1AP
           WHEN "1AQ"       MOVE FISC-MT-FISC TO 1AQ
           WHEN "1AS"       MOVE FISC-MT-FISC TO 1AS
           WHEN "1AT"       MOVE FISC-MT-FISC TO 1AT
           WHEN "1AW"       MOVE FISC-MT-FISC TO 1AW
           WHEN "1AZ"       MOVE FISC-MT-FISC TO 1AZ
           WHEN "1BA"       MOVE FISC-MT-FISC TO 1BA
           WHEN "1BJ"       MOVE FISC-MT-FISC TO 1BJ
           WHEN "1BL"       MOVE FISC-MT-FISC TO 1BL
           WHEN "1BM"       MOVE FISC-MT-FISC TO 1BM
           WHEN "1BP"       MOVE FISC-MT-FISC TO 1BP
           WHEN "1BQ"       MOVE FISC-MT-FISC TO 1BQ
           WHEN "1BS"       MOVE FISC-MT-FISC TO 1BS
           WHEN "1BT"       MOVE FISC-MT-FISC TO 1BT
           WHEN "1BW"       MOVE FISC-MT-FISC TO 1BW
           WHEN "1BZ"       MOVE FISC-MT-FISC TO 1BZ
           WHEN "1CW"       MOVE FISC-MT-FISC TO 1CW
           WHEN "1DN"       MOVE FISC-MT-FISC TO 1DN
           WHEN "1DW"       MOVE FISC-MT-FISC TO 1DW
           WHEN "1DY"       MOVE FISC-MT-FISC TO 1DY
           WHEN "1EY"       MOVE FISC-MT-FISC TO 1EY
           WHEN "1GB"       MOVE FISC-MT-FISC TO 1GB
           WHEN "1GF"       MOVE FISC-MT-FISC TO 1GF
           WHEN "1GG"       MOVE FISC-MT-FISC TO 1GG
           WHEN "1HB"       MOVE FISC-MT-FISC TO 1HB
           WHEN "1HF"       MOVE FISC-MT-FISC TO 1HF
           WHEN "1HG"       MOVE FISC-MT-FISC TO 1HG
           WHEN "1PM"       MOVE FISC-MT-FISC TO 1PM
           WHEN "1QM"       MOVE FISC-MT-FISC TO 1QM
           WHEN "1SM"       MOVE FISC-MT-FISC TO 1SM
           WHEN "1TP"       MOVE FISC-MT-FISC TO 1TP
           WHEN "1TT"       MOVE FISC-MT-FISC TO 1TT
           WHEN "1TX"       MOVE FISC-MT-FISC TO 1TX
           WHEN "1TZ"       MOVE FISC-MT-FISC TO 1TZ
           WHEN "1UP"       MOVE FISC-MT-FISC TO 1UP
           WHEN "1UT"       MOVE FISC-MT-FISC TO 1UT
           WHEN "2BH"       MOVE FISC-MT-FISC TO 2BH
           WHEN "2CA"       MOVE FISC-MT-FISC TO 2CA
           WHEN "2CH"       MOVE FISC-MT-FISC TO 2CH
           WHEN "2DC"       MOVE FISC-MT-FISC TO 2DC
           WHEN "2DF"       MOVE FISC-MT-FISC TO 2DF
           WHEN "2DH"       MOVE FISC-MT-FISC TO 2DH
           WHEN "2DM"       MOVE FISC-MT-FISC TO 2DM
           WHEN "2EE"       MOVE FISC-MT-FISC TO 2EE
           WHEN "2FA"       MOVE FISC-MT-FISC TO 2FA
           WHEN "2FU"       MOVE FISC-MT-FISC TO 2FU
           WHEN "2GO"       MOVE FISC-MT-FISC TO 2GO
           WHEN "2RA"       MOVE FISC-MT-FISC TO 2RA
           WHEN "2RB"       MOVE FISC-MT-FISC TO 2RB
           WHEN "2RC"       MOVE FISC-MT-FISC TO 2RC
           WHEN "2RD"       MOVE FISC-MT-FISC TO 2RD
           WHEN "2TQ"       MOVE FISC-MT-FISC TO 2TQ
           WHEN "2TR"       MOVE FISC-MT-FISC TO 2TR
           WHEN "2TS"       MOVE FISC-MT-FISC TO 2TS
           WHEN "2TT"       MOVE FISC-MT-FISC TO 2TT
           WHEN "2VM"       MOVE FISC-MT-FISC TO 2VM
           WHEN "2VN"       MOVE FISC-MT-FISC TO 2VN
           WHEN "2VO"       MOVE FISC-MT-FISC TO 2VO
           WHEN "2VP"       MOVE FISC-MT-FISC TO 2VP
           WHEN "2VV"       MOVE FISC-MT-FISC TO 2VV
           WHEN "2WW"       MOVE FISC-MT-FISC TO 2WW
           WHEN "2XX"       MOVE FISC-MT-FISC TO 2XX
           WHEN "2YY"       MOVE FISC-MT-FISC TO 2YY
           WHEN "2ZZ"       MOVE FISC-MT-FISC TO 2ZZ
           WHEN "3AN"       MOVE FISC-MT-FISC TO 3AN
           WHEN "3PI"       MOVE FISC-MT-FISC TO 3PI
           WHEN "3SA"       MOVE FISC-MT-FISC TO 3SA
           WHEN "3SE"       MOVE FISC-MT-FISC TO 3SE
           WHEN "3SG"       MOVE FISC-MT-FISC TO 3SG
           WHEN "3SJ"       MOVE FISC-MT-FISC TO 3SJ
           WHEN "3SK"       MOVE FISC-MT-FISC TO 3SK
           WHEN "3SL"       MOVE FISC-MT-FISC TO 3SL
           WHEN "3SZ"       MOVE FISC-MT-FISC TO 3SZ
           WHEN "3TJ"       MOVE FISC-MT-FISC TO 3TJ
           WHEN "3TK"       MOVE FISC-MT-FISC TO 3TK
           WHEN "3TZ"       MOVE FISC-MT-FISC TO 3TZ
           WHEN "3UA"       MOVE FISC-MT-FISC TO 3UA
           WHEN "3VA"       MOVE FISC-MT-FISC TO 3VA
           WHEN "3VC"       MOVE FISC-MT-FISC TO 3VC
           WHEN "3VD"       MOVE FISC-MT-FISC TO 3VD
           WHEN "3VF"       MOVE FISC-MT-FISC TO 3VF
           WHEN "3VG"       MOVE FISC-MT-FISC TO 3VG
           WHEN "3VH"       MOVE FISC-MT-FISC TO 3VH
           WHEN "3VI"       MOVE FISC-MT-FISC TO 3VI
           WHEN "3VJ"       MOVE FISC-MT-FISC TO 3VJ
           WHEN "3VK"       MOVE FISC-MT-FISC TO 3VK
           WHEN "3VM"       MOVE FISC-MT-FISC TO 3VM
           WHEN "3VN"       MOVE FISC-MT-FISC TO 3VN
           WHEN "3VQ"       MOVE FISC-MT-FISC TO 3VQ
           WHEN "3VR"       MOVE FISC-MT-FISC TO 3VR
           WHEN "3VT"       MOVE FISC-MT-FISC TO 3VT
           WHEN "3VZ"       MOVE FISC-MT-FISC TO 3VZ
           WHEN "3WD"       MOVE FISC-MT-FISC TO 3WD
           WHEN "3WE"       MOVE FISC-MT-FISC TO 3WE
           WHEN "3WG"       MOVE FISC-MT-FISC TO 3WG
           WHEN "3WH"       MOVE FISC-MT-FISC TO 3WH
           WHEN "3WI"       MOVE FISC-MT-FISC TO 3WI
           WHEN "3WJ"       MOVE FISC-MT-FISC TO 3WJ
           WHEN "3WN"       MOVE FISC-MT-FISC TO 3WN
           WHEN "3WP"       MOVE FISC-MT-FISC TO 3WP
           WHEN "4BA"       MOVE FISC-MT-FISC TO 4BA
           WHEN "4BB"       MOVE FISC-MT-FISC TO 4BB
           WHEN "4BC"       MOVE FISC-MT-FISC TO 4BC
           WHEN "4BD"       MOVE FISC-MT-FISC TO 4BD
           WHEN "4BE"       MOVE FISC-MT-FISC TO 4BE
           WHEN "4BF"       MOVE FISC-MT-FISC TO 4BF
           WHEN "5EY"       MOVE FISC-MT-FISC TO 5EY
           WHEN "5EZ"       MOVE FISC-MT-FISC TO 5EZ
           WHEN "5FY"       MOVE FISC-MT-FISC TO 5FY
           WHEN "5FZ"       MOVE FISC-MT-FISC TO 5FZ
           WHEN "5GA"       MOVE FISC-MT-FISC TO 5GA
           WHEN "5GB"       MOVE FISC-MT-FISC TO 5GB
           WHEN "5GC"       MOVE FISC-MT-FISC TO 5GC
           WHEN "5GD"       MOVE FISC-MT-FISC TO 5GD
           WHEN "5GE"       MOVE FISC-MT-FISC TO 5GE
           WHEN "5GF"       MOVE FISC-MT-FISC TO 5GF
           WHEN "5GG"       MOVE FISC-MT-FISC TO 5GG
           WHEN "5GH"       MOVE FISC-MT-FISC TO 5GH
           WHEN "5GI"       MOVE FISC-MT-FISC TO 5GI
           WHEN "5GJ"       MOVE FISC-MT-FISC TO 5GJ
           WHEN "5HA"       MOVE FISC-MT-FISC TO 5HA
           WHEN "5HB"       MOVE FISC-MT-FISC TO 5HB
           WHEN "5HC"       MOVE FISC-MT-FISC TO 5HC
           WHEN "5HD"       MOVE FISC-MT-FISC TO 5HD
           WHEN "5HE"       MOVE FISC-MT-FISC TO 5HE
           WHEN "5HH"       MOVE FISC-MT-FISC TO 5HH
           WHEN "5HI"       MOVE FISC-MT-FISC TO 5HI
           WHEN "5HK"       MOVE FISC-MT-FISC TO 5HK
           WHEN "5HM"       MOVE FISC-MT-FISC TO 5HM
           WHEN "5HP"       MOVE FISC-MT-FISC TO 5HP
           WHEN "5HQ"       MOVE FISC-MT-FISC TO 5HQ
           WHEN "5HR"       MOVE FISC-MT-FISC TO 5HR
           WHEN "5HS"       MOVE FISC-MT-FISC TO 5HS
           WHEN "5HT"       MOVE FISC-MT-FISC TO 5HT
           WHEN "5HV"       MOVE FISC-MT-FISC TO 5HV
           WHEN "5HW"       MOVE FISC-MT-FISC TO 5HW
           WHEN "5HX"       MOVE FISC-MT-FISC TO 5HX
           WHEN "5HY"       MOVE FISC-MT-FISC TO 5HY
           WHEN "5HZ"       MOVE FISC-MT-FISC TO 5HZ
           WHEN "5IA"       MOVE FISC-MT-FISC TO 5IA
           WHEN "5IB"       MOVE FISC-MT-FISC TO 5IB
           WHEN "5IC"       MOVE FISC-MT-FISC TO 5IC
           WHEN "5ID"       MOVE FISC-MT-FISC TO 5ID
           WHEN "5IE"       MOVE FISC-MT-FISC TO 5IE
           WHEN "5IH"       MOVE FISC-MT-FISC TO 5IH
           WHEN "5II"       MOVE FISC-MT-FISC TO 5II
           WHEN "5IK"       MOVE FISC-MT-FISC TO 5IK
           WHEN "5IM"       MOVE FISC-MT-FISC TO 5IM
           WHEN "5IP"       MOVE FISC-MT-FISC TO 5IP
           WHEN "5IQ"       MOVE FISC-MT-FISC TO 5IQ
           WHEN "5IR"       MOVE FISC-MT-FISC TO 5IR
           WHEN "5IS"       MOVE FISC-MT-FISC TO 5IS
           WHEN "5IT"       MOVE FISC-MT-FISC TO 5IT
           WHEN "5IU"       MOVE FISC-MT-FISC TO 5IU
           WHEN "5IV"       MOVE FISC-MT-FISC TO 5IV
           WHEN "5IW"       MOVE FISC-MT-FISC TO 5IW
           WHEN "5IX"       MOVE FISC-MT-FISC TO 5IX
           WHEN "5IY"       MOVE FISC-MT-FISC TO 5IY
           WHEN "5IZ"       MOVE FISC-MT-FISC TO 5IZ
           WHEN "5JG"       MOVE FISC-MT-FISC TO 5JG
           WHEN "5JJ"       MOVE FISC-MT-FISC TO 5JJ
           WHEN "5JK"       MOVE FISC-MT-FISC TO 5JK
           WHEN "5JT"       MOVE FISC-MT-FISC TO 5JT
           WHEN "5JU"       MOVE FISC-MT-FISC TO 5JU
           WHEN "5KB"       MOVE FISC-MT-FISC TO 5KB
           WHEN "5KC"       MOVE FISC-MT-FISC TO 5KC
           WHEN "5KE"       MOVE FISC-MT-FISC TO 5KE
           WHEN "5KH"       MOVE FISC-MT-FISC TO 5KH
           WHEN "5KI"       MOVE FISC-MT-FISC TO 5KI
           WHEN "5KJ"       MOVE FISC-MT-FISC TO 5KJ
           WHEN "5KK"       MOVE FISC-MT-FISC TO 5KK
           WHEN "5KM"       MOVE FISC-MT-FISC TO 5KM
           WHEN "5KN"       MOVE FISC-MT-FISC TO 5KN
           WHEN "5KO"       MOVE FISC-MT-FISC TO 5KO
           WHEN "5KP"       MOVE FISC-MT-FISC TO 5KP
           WHEN "5KQ"       MOVE FISC-MT-FISC TO 5KQ
           WHEN "5KR"       MOVE FISC-MT-FISC TO 5KR
           WHEN "5KS"       MOVE FISC-MT-FISC TO 5KS
           WHEN "5KT"       MOVE FISC-MT-FISC TO 5KT
           WHEN "5KU"       MOVE FISC-MT-FISC TO 5KU
           WHEN "5KV"       MOVE FISC-MT-FISC TO 5KV
           WHEN "5KW"       MOVE FISC-MT-FISC TO 5KW
           WHEN "5KX"       MOVE FISC-MT-FISC TO 5KX
           WHEN "5KY"       MOVE FISC-MT-FISC TO 5KY
           WHEN "5KZ"       MOVE FISC-MT-FISC TO 5KZ
           WHEN "5LA"       MOVE FISC-MT-FISC TO 5LA
           WHEN "5LB"       MOVE FISC-MT-FISC TO 5LB
           WHEN "5LC"       MOVE FISC-MT-FISC TO 5LC
           WHEN "5LD"       MOVE FISC-MT-FISC TO 5LD
           WHEN "5LE"       MOVE FISC-MT-FISC TO 5LE
           WHEN "5LH"       MOVE FISC-MT-FISC TO 5LH
           WHEN "5LI"       MOVE FISC-MT-FISC TO 5LI
           WHEN "5LJ"       MOVE FISC-MT-FISC TO 5LJ
           WHEN "5LM"       MOVE FISC-MT-FISC TO 5LM
           WHEN "5LN"       MOVE FISC-MT-FISC TO 5LN
           WHEN "5LO"       MOVE FISC-MT-FISC TO 5LO
           WHEN "5LP"       MOVE FISC-MT-FISC TO 5LP
           WHEN "5LQ"       MOVE FISC-MT-FISC TO 5LQ
           WHEN "5LR"       MOVE FISC-MT-FISC TO 5LR
           WHEN "5LS"       MOVE FISC-MT-FISC TO 5LS
           WHEN "5LT"       MOVE FISC-MT-FISC TO 5LT
           WHEN "5LU"       MOVE FISC-MT-FISC TO 5LU
           WHEN "5LV"       MOVE FISC-MT-FISC TO 5LV
           WHEN "5LW"       MOVE FISC-MT-FISC TO 5LW
           WHEN "5LX"       MOVE FISC-MT-FISC TO 5LX
           WHEN "5LY"       MOVE FISC-MT-FISC TO 5LY
           WHEN "5LZ"       MOVE FISC-MT-FISC TO 5LZ
           WHEN "5MT"       MOVE FISC-MT-FISC TO 5MT
           WHEN "5NA"       MOVE FISC-MT-FISC TO 5NA
           WHEN "5NB"       MOVE FISC-MT-FISC TO 5NB
           WHEN "5NC"       MOVE FISC-MT-FISC TO 5NC
           WHEN "5ND"       MOVE FISC-MT-FISC TO 5ND
           WHEN "5NE"       MOVE FISC-MT-FISC TO 5NE
           WHEN "5NF"       MOVE FISC-MT-FISC TO 5NF
           WHEN "5NG"       MOVE FISC-MT-FISC TO 5NG
           WHEN "5NH"       MOVE FISC-MT-FISC TO 5NH
           WHEN "5NI"       MOVE FISC-MT-FISC TO 5NI
           WHEN "5NJ"       MOVE FISC-MT-FISC TO 5NJ
           WHEN "5NK"       MOVE FISC-MT-FISC TO 5NK
           WHEN "5NL"       MOVE FISC-MT-FISC TO 5NL
           WHEN "5NM"       MOVE FISC-MT-FISC TO 5NM
           WHEN "5NN"       MOVE FISC-MT-FISC TO 5NN
           WHEN "5NO"       MOVE FISC-MT-FISC TO 5NO
           WHEN "5NP"       MOVE FISC-MT-FISC TO 5NP
           WHEN "5NQ"       MOVE FISC-MT-FISC TO 5NQ
           WHEN "5NR"       MOVE FISC-MT-FISC TO 5NR
           WHEN "5NS"       MOVE FISC-MT-FISC TO 5NS
           WHEN "5NT"       MOVE FISC-MT-FISC TO 5NT
           WHEN "5NU"       MOVE FISC-MT-FISC TO 5NU
           WHEN "5NW"       MOVE FISC-MT-FISC TO 5NW
           WHEN "5NX"       MOVE FISC-MT-FISC TO 5NX
           WHEN "5NY"       MOVE FISC-MT-FISC TO 5NY
           WHEN "5NZ"       MOVE FISC-MT-FISC TO 5NZ
           WHEN "5OA"       MOVE FISC-MT-FISC TO 5OA
           WHEN "5OB"       MOVE FISC-MT-FISC TO 5OB
           WHEN "5OC"       MOVE FISC-MT-FISC TO 5OC
           WHEN "5OD"       MOVE FISC-MT-FISC TO 5OD
           WHEN "5OE"       MOVE FISC-MT-FISC TO 5OE
           WHEN "5OF"       MOVE FISC-MT-FISC TO 5OF
           WHEN "5OG"       MOVE FISC-MT-FISC TO 5OG
           WHEN "5OH"       MOVE FISC-MT-FISC TO 5OH
           WHEN "5OI"       MOVE FISC-MT-FISC TO 5OI
           WHEN "5OJ"       MOVE FISC-MT-FISC TO 5OJ
           WHEN "5OK"       MOVE FISC-MT-FISC TO 5OK
           WHEN "5OL"       MOVE FISC-MT-FISC TO 5OL
           WHEN "5OM"       MOVE FISC-MT-FISC TO 5OM
           WHEN "5ON"       MOVE FISC-MT-FISC TO 5ON
           WHEN "5OO"       MOVE FISC-MT-FISC TO 5OO
           WHEN "5OP"       MOVE FISC-MT-FISC TO 5OP
           WHEN "5OQ"       MOVE FISC-MT-FISC TO 5OQ
           WHEN "5OR"       MOVE FISC-MT-FISC TO 5OR
           WHEN "5OW"       MOVE FISC-MT-FISC TO 5OW
           WHEN "5OX"       MOVE FISC-MT-FISC TO 5OX
           WHEN "5OY"       MOVE FISC-MT-FISC TO 5OY
           WHEN "5OZ"       MOVE FISC-MT-FISC TO 5OZ
           WHEN "5QA"       MOVE FISC-MT-FISC TO 5QA
           WHEN "5QB"       MOVE FISC-MT-FISC TO 5QB
           WHEN "5QC"       MOVE FISC-MT-FISC TO 5QC
           WHEN "5QD"       MOVE FISC-MT-FISC TO 5QD
           WHEN "5QH"       MOVE FISC-MT-FISC TO 5QH
           WHEN "5QI"       MOVE FISC-MT-FISC TO 5QI
           WHEN "5QJ"       MOVE FISC-MT-FISC TO 5QJ
           WHEN "5QL"       MOVE FISC-MT-FISC TO 5QL
           WHEN "5QM"       MOVE FISC-MT-FISC TO 5QM
           WHEN "5RA"       MOVE FISC-MT-FISC TO 5RA
           WHEN "5RB"       MOVE FISC-MT-FISC TO 5RB
           WHEN "5RC"       MOVE FISC-MT-FISC TO 5RC
           WHEN "5RD"       MOVE FISC-MT-FISC TO 5RD
           WHEN "5RF"       MOVE FISC-MT-FISC TO 5RF
           WHEN "5RG"       MOVE FISC-MT-FISC TO 5RG
           WHEN "5RH"       MOVE FISC-MT-FISC TO 5RH
           WHEN "5RI"       MOVE FISC-MT-FISC TO 5RI
           WHEN "5RJ"       MOVE FISC-MT-FISC TO 5RJ
           WHEN "5RL"       MOVE FISC-MT-FISC TO 5RL
           WHEN "5RM"       MOVE FISC-MT-FISC TO 5RM
           WHEN "5RN"       MOVE FISC-MT-FISC TO 5RN
           WHEN "5RO"       MOVE FISC-MT-FISC TO 5RO
           WHEN "5RP"       MOVE FISC-MT-FISC TO 5RP
           WHEN "5RQ"       MOVE FISC-MT-FISC TO 5RQ
           WHEN "5RR"       MOVE FISC-MT-FISC TO 5RR
           WHEN "5RW"       MOVE FISC-MT-FISC TO 5RW
           WHEN "5RZ"       MOVE FISC-MT-FISC TO 5RZ
           WHEN "5SN"       MOVE FISC-MT-FISC TO 5SN
           WHEN "5SO"       MOVE FISC-MT-FISC TO 5SO
           WHEN "5SP"       MOVE FISC-MT-FISC TO 5SP
           WHEN "5SV"       MOVE FISC-MT-FISC TO 5SV
           WHEN "5SW"       MOVE FISC-MT-FISC TO 5SW
           WHEN "5TA"       MOVE FISC-MT-FISC TO 5TA
           WHEN "5TB"       MOVE FISC-MT-FISC TO 5TB
           WHEN "5TC"       MOVE FISC-MT-FISC TO 5TC
           WHEN "5TE"       MOVE FISC-MT-FISC TO 5TE
           WHEN "5TF"       MOVE FISC-MT-FISC TO 5TF
           WHEN "5TH"       MOVE FISC-MT-FISC TO 5TH
           WHEN "5TI"       MOVE FISC-MT-FISC TO 5TI
           WHEN "5UA"       MOVE FISC-MT-FISC TO 5UA
           WHEN "5UB"       MOVE FISC-MT-FISC TO 5UB
           WHEN "5UC"       MOVE FISC-MT-FISC TO 5UC
           WHEN "5UE"       MOVE FISC-MT-FISC TO 5UE
           WHEN "5UF"       MOVE FISC-MT-FISC TO 5UF
           WHEN "5UH"       MOVE FISC-MT-FISC TO 5UH
           WHEN "5UI"       MOVE FISC-MT-FISC TO 5UI
           WHEN "5VI"       MOVE FISC-MT-FISC TO 5VI
           WHEN "5XA"       MOVE FISC-MT-FISC TO 5XA
           WHEN "5XB"       MOVE FISC-MT-FISC TO 5XB
           WHEN "5XN"       MOVE FISC-MT-FISC TO 5XN
           WHEN "5XO"       MOVE FISC-MT-FISC TO 5XO
           WHEN "5XT"       MOVE FISC-MT-FISC TO 5XT
           WHEN "5XU"       MOVE FISC-MT-FISC TO 5XU
           WHEN "5XV"       MOVE FISC-MT-FISC TO 5XV
           WHEN "5XW"       MOVE FISC-MT-FISC TO 5XW
           WHEN "5YA"       MOVE FISC-MT-FISC TO 5YA
           WHEN "5YB"       MOVE FISC-MT-FISC TO 5YB
           WHEN "5YN"       MOVE FISC-MT-FISC TO 5YN
           WHEN "5YO"       MOVE FISC-MT-FISC TO 5YO
           WHEN "6DE"       MOVE FISC-MT-FISC TO 6DE
           WHEN "8BY"       MOVE FISC-MT-FISC TO 8BY
           WHEN "8CY"       MOVE FISC-MT-FISC TO 8CY
           WHEN "8FV"       MOVE FISC-MT-FISC TO 8FV
           WHEN "CHC"       MOVE FISC-MT-FISC TO CHC
           WHEN "CHI"       MOVE FISC-MT-FISC TO CHI
           WHEN "CIC"       MOVE FISC-MT-FISC TO CIC
           WHEN "CII"       MOVE FISC-MT-FISC TO CII
           WHEN "DAJ"       MOVE FISC-MT-FISC TO DAJ
           WHEN "DBJ"       MOVE FISC-MT-FISC TO DBJ
           WHEN "EAJ"       MOVE FISC-MT-FISC TO EAJ
           WHEN "EBJ"       MOVE FISC-MT-FISC TO EBJ
           WHEN "FAS"       MOVE FISC-MT-FISC TO FAS
           WHEN "FBS"       MOVE FISC-MT-FISC TO FBS
           WHEN "NAJ"       MOVE FISC-MT-FISC TO NAJ
           WHEN "NAP"       MOVE FISC-MT-FISC TO NAP
           WHEN "NAS"       MOVE FISC-MT-FISC TO NAS
           WHEN "NAW"       MOVE FISC-MT-FISC TO NAW
           WHEN "NAZ"       MOVE FISC-MT-FISC TO NAZ
           WHEN "NBA"       MOVE FISC-MT-FISC TO NBA
           WHEN "NBJ"       MOVE FISC-MT-FISC TO NBJ
           WHEN "NBP"       MOVE FISC-MT-FISC TO NBP
           WHEN "NBS"       MOVE FISC-MT-FISC TO NBS
           WHEN "NBW"       MOVE FISC-MT-FISC TO NBW
           WHEN "NBZ"       MOVE FISC-MT-FISC TO NBZ
           WHEN "NCH"       MOVE FISC-MT-FISC TO NCH
           WHEN "NCW"       MOVE FISC-MT-FISC TO NCW
           WHEN "NDC"       MOVE FISC-MT-FISC TO NDC
           WHEN "NDW"       MOVE FISC-MT-FISC TO NDW
           WHEN "NFU"       MOVE FISC-MT-FISC TO NFU
           WHEN "NGO"       MOVE FISC-MT-FISC TO NGO
           WHEN "NTR"       MOVE FISC-MT-FISC TO NTR
           WHEN "NTS"       MOVE FISC-MT-FISC TO NTS
           WHEN "NUA"       MOVE FISC-MT-FISC TO NUA
           WHEN "NVG"       MOVE FISC-MT-FISC TO NVG
           WHEN "RAJ"       MOVE FISC-MT-FISC TO RAJ
           WHEN "RAL"       MOVE FISC-MT-FISC TO RAL
           WHEN "RAM"       MOVE FISC-MT-FISC TO RAM
           WHEN "RAP"       MOVE FISC-MT-FISC TO RAP
           WHEN "RAS"       MOVE FISC-MT-FISC TO RAS
           WHEN "RAW"       MOVE FISC-MT-FISC TO RAW
           WHEN "RAZ"       MOVE FISC-MT-FISC TO RAZ
           WHEN "RBA"       MOVE FISC-MT-FISC TO RBA
           WHEN "RBJ"       MOVE FISC-MT-FISC TO RBJ
           WHEN "RBL"       MOVE FISC-MT-FISC TO RBL
           WHEN "RBM"       MOVE FISC-MT-FISC TO RBM
           WHEN "RBP"       MOVE FISC-MT-FISC TO RBP
           WHEN "RBS"       MOVE FISC-MT-FISC TO RBS
           WHEN "RBW"       MOVE FISC-MT-FISC TO RBW
           WHEN "RBZ"       MOVE FISC-MT-FISC TO RBZ
           WHEN "RCH"       MOVE FISC-MT-FISC TO RCH
           WHEN "RCW"       MOVE FISC-MT-FISC TO RCW
           WHEN "RDC"       MOVE FISC-MT-FISC TO RDC
           WHEN "RDW"       MOVE FISC-MT-FISC TO RDW
           WHEN "RFU"       MOVE FISC-MT-FISC TO RFU
           WHEN "RGO"       MOVE FISC-MT-FISC TO RGO
           WHEN "RTR"       MOVE FISC-MT-FISC TO RTR
           WHEN "RTS"       MOVE FISC-MT-FISC TO RTS
           WHEN "RUA"       MOVE FISC-MT-FISC TO RUA
           WHEN "RVG"       MOVE FISC-MT-FISC TO RVG
           WHEN "SBA"       MOVE FISC-MT-FISC TO SBA
           WHEN "TBA"       MOVE FISC-MT-FISC TO TBA
        END-EVALUATE
        .
      * FGA - 10/2019 - PUMA - #409636 >> 
      *****************************************************************        
      * Calcul des éléments composant la formule de calcul 
      *  des revenus AR
      *****************************************************************        
       CALCUL-ELEMENTS-REV-AR.
      *
        INITIALIZE W-CALCUL-INT-AR
        
        MOVE 1 TO V-COEF
        IF (DECL = 1 AND NBPLDE = 2) OR (DECL = 2)
           MOVE 2 TO V-COEF
        END-IF
      * ========================================================
      * ----- Début des éléments du déclarant 1 ----------------   
      * ========================================================
      * --------------------------------------------------------
        COMPUTE V-5NA-5NK-5NM-5KM-5NY-5NZ  = 5NA + 5NK + 5NM
                                           + 5KM - 5NY - 5NZ
        IF V-5NA-5NK-5NM-5KM-5NY-5NZ < 0 
          MOVE 0 TO V-5NA-5NK-5NM-5KM-5NY-5NZ
        END-IF
        
      * --------------------------------------------------------
        COMPUTE V-5KQ-5KR-5NX-5IU-5NQ-5NR  = 5KQ - 5KR 
                                           + 5NX - 5IU 
                                           + 5NQ - 5NR
        IF V-5KQ-5KR-5NX-5IU-5NQ-5NR < 0                        
          MOVE 0 TO V-5KQ-5KR-5NX-5IU-5NQ-5NR                   
        END-IF
       
      * --------------------------------------------------------
        COMPUTE V-5HR-5HS  = 5HR - 5HS
        IF V-5HR-5HS < 0 
          MOVE 0 TO V-5HR-5HS
        END-IF
       
      * --------------------------------------------------------
        COMPUTE V-5KY-5JU  = 5KY - 5JU
        IF V-5KY-5JU < 0 
          MOVE 0 TO V-5KY-5JU
        END-IF
       
      * --------------------------------------------------------
        COMPUTE V-5KV-5KW  = 5KV - 5KW
        IF V-5KV-5KW < 0 
          MOVE 0 TO V-5KV-5KW
        END-IF
      * ========================================================
      * ----- Début des éléments du déclarant 2 ----------------   
      * ========================================================
      * --------------------------------------------------------
        COMPUTE V-5OA-5OK-5OM-5LM-5OY-5OZ = 5OA + 5OK
                                          + 5OM + 5LM
                                          - 5OY - 5OZ
        IF V-5OA-5OK-5OM-5LM-5OY-5OZ < 0 
           MOVE 0 TO V-5OA-5OK-5OM-5LM-5OY-5OZ
        END-IF

      * --------------------------------------------------------
        COMPUTE V-5LQ-5LR-5OX-5RZ-5OQ-5OR = 5LQ - 5LR
                                          + 5OX - 5RZ 
                                          + 5OQ - 5OR
        IF V-5LQ-5LR-5OX-5RZ-5OQ-5OR < 0                         
           MOVE 0 TO V-5LQ-5LR-5OX-5RZ-5OQ-5OR                   
        END-IF

      * --------------------------------------------------------
        COMPUTE V-5LY-5LD-5LV-5LW = 5LY - 5LD 
                                  + 5LV - 5LW
        IF V-5LY-5LD-5LV-5LW < 0 
           MOVE 0 TO V-5LY-5LD-5LV-5LW
        END-IF
        
      * --------------------------------------------------------
        COMPUTE V-5IR-5IS = 5IR - 5IS
        IF V-5IR-5IS < 0                         
           MOVE 0 TO V-5IR-5IS                   
        END-IF

      * ========================================================
      * ----- Début des éléments communs aux deux déclarants ---   
      * ========================================================
      * --------------------------------------------------------
        IF NAW <> 0 
          COMPUTE V-1AW-RAW-NAW  = 
             (1AW + RAW / NAW) * (1 - 0.3)
        ELSE
          COMPUTE V-1AW-RAW-NAW  = 
             1AW * (1 - 0.3)
        END-IF
        
      * --------------------------------------------------------
        IF NBW <> 0 
          COMPUTE V-1BW-RBW-NBW  =  
              (1BW + RBW / NBW) * (1 - 0.5)
        ELSE
          COMPUTE V-1BW-RBW-NBW  = 
             1BW * (1 - 0.5)
        END-IF
        
      * --------------------------------------------------------
        IF NCW <> 0 
            COMPUTE V-1CW-RCW-NCW  =  
             (1CW + RCW / NCW) * (1 - 0.6)                    
        ELSE
          COMPUTE V-1CW-RCW-NCW  = 
             1CW * (1 - 0.6)
        END-IF
        
      * --------------------------------------------------------
        IF NDW <> 0
          COMPUTE V-1DW-RDW-NDW  =  
             (1DW + RDW / NDW) * (1 - 0.7)                    
        ELSE
          COMPUTE V-1DW-RDW-NDW  = 
             1DW * (1 - 0.7)
        END-IF
        
      * --------------------------------------------------------
        IF NDC <> 0 
          COMPUTE V-RDC-NDC = RDC / NDC 
        END-IF
          
      * --------------------------------------------------------
        IF NFU <> 0 
          COMPUTE V-RFU-NFU = RFU / NFU
        END-IF

      * --------------------------------------------------------
        COMPUTE V-2RB-2RC-2RD = 2RB + 2RC + 2RD - V-COEF * 4600
        IF V-2RB-2RC-2RD < 0                         
           MOVE 0 TO V-2RB-2RC-2RD                   
        END-IF

      * --------------------------------------------------------
        IF NCH = 0
           IF 2CH + 2VV + 2WW + RCH = 0
              COMPUTE W-REV-AR-INT  = 2CH + 2VV + 2WW
           ELSE
              COMPUTE W-REV-AR-INT  = 
                  (2CH + 2VV + 2WW)                    
                  - V-COEF * 4600 * (2CH + 2VV + 2WW)
                  / (2CH + 2VV + 2WW + RCH)
           END-IF
        ELSE
           IF 2CH + 2VV + 2WW + RCH = 0
              COMPUTE W-REV-AR-INT  =  
                             (2CH + 2VV + 2WW)
                             + RCH / NCH
           ELSE
              COMPUTE W-REV-AR-INT  =  
                  (2CH + 2VV + 2WW)
                  - V-COEF * 4600 * ((2CH + 2VV + 2WW)   
                  / (2CH + 2VV + 2WW + RCH))
                  + (RCH             
                 - V-COEF * 4600 * RCH
                  / (2CH + 2VV + 2WW + RCH))
                  / NCH
           END-IF
        END-IF                          
        IF W-REV-AR-INT < 0 
           MOVE 0 TO W-REV-AR-INT 
        END-IF
         
      * --------------------------------------------------------
        IF NTS <> 0 
           COMPUTE V-RTS-NTS = RTS / NTS 
        END-IF
         
      * --------------------------------------------------------
        IF NTR <> 0 
           COMPUTE V-RTR-NTR = RTR / NTR 
        END-IF
         
      * --------------------------------------------------------
        IF NGO <> 0 
           COMPUTE V-RGO-NGO = RGO / NGO 
        END-IF
         
      * --------------------------------------------------------
        IF NVG <> 0 
           COMPUTE V-RVG-NVG = RVG / NVG 
        END-IF
        
        IF NUA <> 0 
           COMPUTE V-RUA-NUA = RUA / NUA 
        END-IF

        COMPUTE V-3UA-3VG-3VQ-RVG-NVG-RUA-NUA-3VR =
                    3UA + 3VG + 3VQ
                  + V-RVG-NVG
                  + V-RUA-NUA
                  - 3VR
        IF V-3UA-3VG-3VQ-RVG-NVG-RUA-NUA-3VR < 0 
           MOVE 0 TO V-3UA-3VG-3VQ-RVG-NVG-RUA-NUA-3VR
        END-IF
        
      * --------------------------------------------------------
        COMPUTE V-3TJ-3TK  = 3TJ - 3TK
        IF V-3TJ-3TK < 0 
          MOVE 0 TO V-3TJ-3TK
        END-IF
       
      * --------------------------------------------------------
        IF NBA <> 0 
           COMPUTE V-RBA-NBA = RBA / NBA
        END-IF
      
        IF TBA <> 0 
           COMPUTE V-SBA-TBA = SBA / TBA
        END-IF
        
        COMPUTE V-4BE-4BA-RBA-NBA-SBA-TBA-4BB-4BC-4BD =         
                        4BE * (1 - 0.3)
                      + 4BA
                      + V-RBA-NBA
                      + V-SBA-TBA
                      - 4BB - 4BC - 4BD         
        IF V-4BE-4BA-RBA-NBA-SBA-TBA-4BB-4BC-4BD < 0         
           MOVE 0 TO V-4BE-4BA-RBA-NBA-SBA-TBA-4BB-4BC-4BD
        END-IF
       .
      *****************************************************************        
      * Calcul des éléments composant la formule de calcul 
      *  des revenus SP
      *****************************************************************
       CALCUL-ELEMENTS-REV-SP.
          INITIALIZE W-CALCUL-INT-SP

      * ========================================================
      * ----- Début des éléments du déclarant 1 ----------------   
      * ========================================================
      * --------------------------------------------------------
          IF NAJ <> 0 
             COMPUTE V-RAJ-NAJ =  RAJ / NAJ 
          END-IF

      * --------------------------------------------------------
          COMPUTE V-5HX-5XN-5HE = 5HX - 5XN + 5HE
          IF V-5HX-5XN-5HE < 0                             
              MOVE 0 TO V-5HX-5XN-5HE                       
          END-IF

      * --------------------------------------------------------
          COMPUTE V-5KN-5KO-5KP-5KX-5KJ = 5KN 
                                        + 5KO * (1 - 0.71)
                                        + 5KP * (1 - 0.5) 
                                        + 5KX - 5KJ
          IF V-5KN-5KO-5KP-5KX-5KJ < 0                             
              MOVE 0 TO V-5KN-5KO-5KP-5KX-5KJ                       
          END-IF

      * --------------------------------------------------------
          COMPUTE V-5HP-5HQ-5HV-5KZ = 5HP 
                                    + 5HQ * (1 - 0.34)
                                    + 5HV - 5KZ
          IF V-5HP-5HQ-5HV-5KZ < 0                                      
              MOVE 0 TO V-5HP-5HQ-5HV-5KZ                               
          END-IF

      * ========================================================
      * ----- Début des éléments du déclarant 2 ----------------   
      * ========================================================
      * --------------------------------------------------------
          IF NBJ <> 0 
             COMPUTE V-RBJ-NBJ = RBJ / NBJ 
          END-IF

      * --------------------------------------------------------
          COMPUTE V-5IX-5YN-5IE = 5IX - 5YN + 5IE
          IF V-5IX-5YN-5IE < 0                             
              MOVE 0 TO V-5IX-5YN-5IE                       
          END-IF

      * --------------------------------------------------------
          COMPUTE V-5LN-5LO-5LP-5LX-5LJ = 5LN 
                                        + 5LO * (1 - 0.71)
                                        + 5LP * (1 - 0.5)
                                        + 5LX - 5LJ
          IF V-5LN-5LO-5LP-5LX-5LJ < 0                                  
              MOVE 0 TO V-5LN-5LO-5LP-5LX-5LJ                           
          END-IF.

      * --------------------------------------------------------
          COMPUTE V-5IP-5IQ-5IV-5LZ = 5IP 
                                     + 5IQ * (1 - 0.34)
                                     + 5IV - 5LZ
          IF V-5IP-5IQ-5IV-5LZ < 0                                      
              MOVE 0 TO V-5IP-5IQ-5IV-5LZ                               
          END-IF
       .
      *****************************************************************
      * Appel du programme XBASE de gestion des tables SQL            *
      *****************************************************************       
       APPEL-XBASE.                                                             
      *-----------                                                              
           CALL    "XBASE"     USING   V2TOTAL                                  
                                       TOT-MINI6                                
                                       DTFLNK                                   
                                       DTFINP                                   
                                       DTFWRK                                   
                                       DTFCOM                                  
       .
      *