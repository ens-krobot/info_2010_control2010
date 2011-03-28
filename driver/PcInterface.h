/**
 * @file PcInterface.h
 * Fichier commun avec le programme hôte sur la carte mère (USB PC).
 * Ce fichier définit de nombreuses constantes utilisées pour le protocole
 * de communication carte <-> PC.
*/

#ifndef PC_INTERFACE_H
#define PC_INTERFACE_H

#define USB_VID                      0x04D8        ///< Vendor ID commun aux différentes cartes USB
#define USB_PID_USB_DEV_BOARD        0x0001        ///< Product ID de la Carte d'essais
#define USB_PID_PROXIMITY_SENSOR     0x0002        ///< Product ID de la Carte capteurs
#define USB_PID_MOTOR_CONTROLLER     0x0003        ///< Product ID de la Carte d'asservissement
#define USB_PID_ROBOT_INTERFACE      0x0004        ///< Product ID de la Carte d'actionneur
#define USB_PID_BATTERY_MONITORING   0x0005        ///< Product ID de la carte Battery Monitoring
#define USB_PID_SENSOR_INTERFACE     0x0006        ///< Product ID de la carte Sensor Interface
#define USB_PID_BOOTLOADER           0x000b        ///< Product ID d'une carte en mode bootloader

// Protocole USB
typedef struct _UP {
    BYTE HSEQ;
    BYTE DSEQ;
    BYTE CMD;
    BYTE ERR;
    BYTE RES0;
    BYTE RES1;
    BYTE RES2;
    BYTE RES3;
    BYTE DATA[52];
    BYTE RES4;
    BYTE RES5;
    BYTE RES6;
    BYTE RES7;
} UP;

#define UP_HSEQ                           0        ///< Host sequence number
#define UP_DSEQ                           1        ///< Device sequence number
#define UP_CMD                            2
#define UP_ERR                            3
#define UP_RES0                           4
#define UP_RES1                           5
#define UP_RES2                           6
#define UP_RES3                           7
#define UP_RES4                          60
#define UP_RES5                          61
#define UP_RES6                          62
#define UP_RES7                          63
#define UP_DATA                           8

// Commande (premier bit)
#define CMD_RESET                         0        ///< Reset du PIC
#define CMD_BOOTLOADER                    1        ///< Reset du PIC en mode bootloader
#define CMD_GET                           2        ///< Obtenir une information
#define CMD_RESPOND                       3        ///< Réponse à une commande
#define CMD_ERR                           4        ///< Envoyer une erreur
#define CMD_SEND                          5        ///< Envoyer du texte
#define CMD_SET                           6        ///< Définir l'état d'un paramètre
#define CMD_TEST                          7        ///< Commande générique pour déclencher une action de test
#define CMD_CALIBRATE                     8        ///< Lance la calibration d'un télémètre [Carte capteurs]
#define CMD_AX12                          9
#define CMD_TRAJ                         10        ///< Transmet une trajetoire au Krobot
#define CMD_MOTOR                        11        ///< Gestion des moteurs
#define CMD_MOTOR_TOR                    12
#define CMD_LCD                          13        ///< Commande de l'afficheur LCD
#define CMD_USRF                         14        ///< Télémètres US I2C

// CMD_GET arguments
#define GET_RESET_SOURCE                  0        ///< Demande au PIC la source du Reset
#define GET_BOARD_INFO                    1        ///< Demande au PIC le nom de la carte et l'auteur
#define GET_FIRMWARE_BUILD                2        ///< Demande au PIC la date et l'heure de compilation du firmware
#define GET_PORTS_CONFIG                  3        ///< Demande la config TRIS des ports du PIC
#define GET_PORTS_STATE                   4        ///< Demande l'état des ports du PIC
#define GET_RANGEFINDER_STATE             5        ///< Demande au PIC les mesures des télémètres [Carte capteurs]
#define GET_ISENS                         6        ///< Demande au PIC les valeurs des courants moteurs mesurées [Carte d'asservissement]
#define GET_CURRENT_POS                   7        ///< Demande au PIC les positions actuelles des moteurs [Carte d'asservissement]
#define GET_RANGEFINDER_CALIBRATION       8        ///< Demande au PIC les valeurs de calibration des télémètres [Carte capteurs]
#define GET_TOR_STATE                     9        ///< Demande au PIC l'état des capteurs de contact [Carte capteurs]
#define GET_CMP03_DATA                   10        ///< Récupère les infos du compas électronique CMP03
#define GET_CELL_VOLTAGE                 11        ///< Tensions des cellules [Battery Monitoring]
#define GET_CURRENT                      12        ///< Valeur instantanée du courant débité par la batterie [Battery Monitoring]
#define GET_POWER_STATE                  13        ///< Etat de l'alimentation de puissance (On/Off) [Battery Monitoring]
#define GET_BATTERY_STATE                14        ///< Etat des batteries (Pleine charge/Charge moyenne/Charge faible) [Battery Monitoring]
#define GET_CURRENT_SPEED                15        ///< Demande au PIC la valeur actuelle de la vitesse d'un moteur [Carte d'asservissement]
#define GET_INTEGRATION_SUM              16        ///< Demande au PIC la valeur actuelle du terme integration de l'asservissement [Carte d'asservissement]
#define GET_DESIRED_SPEED                17        ///< Demande au PIC la valeur désirée de la vitesse d'un moteur [Carte d'asservissement]
#define GET_DESIRED_POS                  18        ///< Demande au PIC les positions désirées des moteurs [Carte d'asservissement]

// CMD_ERR arguments
#define ERR_UNKNOWN_CMD                   1        ///< Commande inconnue
#define ERR_UNKNOWN_GET                   2        ///< Demande inconnue
#define ERR_UNKNOWN_SET                   3        ///< Demande inconnue
#define ERR_INVALID_RESPONSE              4        ///< Réponse invalide
#define ERR_AX12_WRONG_PACKET             5
#define ERR_AX12_ERROR                    6
#define ERR_AX12_CHKSUM                   7
#define ERR_CMP03_NOT_RESPONDING          8
#define ERR_ADJD_S371_NOT_RESPONDING      9
#define ERR_LM_COMMAND_ERROR             10
#define ERR_LM_POSITION_ERROR            11
#define ERR_INVALID_AXIS                 12

// CMD_SET arguments
#define SET_PORTS_CONFIG_INPUTS        0x00        ///< Définir les entrées du PIC
#define SET_PORTS_CONFIG_OUTPUTS       0x01        ///< Définir les sorties du PIC
#define SET_PORTS_STATE_LOW            0x02        ///< Définir les sorties à l'état bas du PIC
#define SET_PORTS_STATE_HIGH           0x03        ///< Définir les sorties à l'état haut du PIC
#define SET_SERVO_CONFIG               0x04        ///< Définir la config des servomoteurs [Robot Interface]
#define SET_SERVO_STATE                0x05        ///< Définir l'état des servomoteurs [Robot Interface]
#define SET_BUZZER_STATE               0x06        ///< Activer le buzzer [Battery Monitoring]

// CMD_CALIBRATE arguments
#define CAL_START                      0x00
#define CAL_CONTINUE                   0x01
#define CAL_STOP                       0x02
#define CAL_ERROR                      0x03
#define CAL_DONE                       0x04
#define CAL_PLACE_INF                  0x05
#define CAL_PLACE_30                   0x06
#define CAL_PLACE_100                  0x07

// GET_RESET_SOURCE reponse
#define RESET_SOURCE_POR               0x01        ///< Power-on Reset
#define RESET_SOURCE_RI                0x02        ///< RESET Instruction
#define RESET_SOURCE_BOR               0x03        ///< Brown-out Reset
#define RESET_SOURCE_WDT               0x04        ///< Watchdog Time-out Reset
#define RESET_SOURCE_STKFUL            0x05        ///< Stack Full Reset
#define RESET_SOURCE_STKUNF            0x06        ///< Stack Underflow Reset
#define RESET_SOURCE_MCLR              0x07        ///< Master Clear Reset

// CMD_AX12
#define AX12_PING                         1
#define AX12_READ8                        2
#define AX12_READ16                       3
#define AX12_WRITE8                       4
#define AX12_WRITE16                      5
#define AX12_GOTO                         6
#define AX12_GET_POS                      7
#define AX12_GET_SPEED                    8
#define AX12_GET_LOAD                     9
#define AX12_GET_STATS                   10
#define AX12_WRITE_REG8                  11
#define AX12_WRITE_REG16                 12
#define AX12_ACTION                      13
#define AX12_RESET                       14
#define AX12_CONFIG                      15

#define AX12_EXEC_NOW                  0x00
#define AX12_EXEC_ACTION               0x01

// CMD_LCD
#define LCD_CLEAR                      0x00
#define LCD_CURSOR_ON                  0x01
#define LCD_CURSOR_OFF                 0x02
#define LCD_BACKLIGHT_ON               0x03
#define LCD_BACKLIGHT_OFF              0x04
#define LCD_GOTO_POS                   0x05
#define LCD_WRITE                      0x06
#define LCD_WRITE_LINE                 0x07

// CMD_USRF
#define USRF_MEASURE                   0x00
#define USRF_GET                       0x01

// CMD_TRAJ
#define TRAJ_INIT                      0x00
#define TRAJ_FORWARD                   0x01
#define TRAJ_BACKWARD                  0x02
#define TRAJ_TR                        0x03
#define TRAJ_TL                        0x04
#define TRAJ_GOTO                      0x05
#define TRAJ_FINISHED                  0x06
#define TRAJ_STOP                      0x07
#define TRAJ_NEW_POSITION              0x08
#define TRAJ_NEW_VELOCITY              0x09
#define TRAJ_CHANGE_VELOCITY           0x0A
#define TRAJ_CONFIG                    0x0B
#define TRAJ_CONFIG_DEFAULT            0x0C
#define TRAJ_START                     0x0D
#define TRAJ_GET_REL_POS               0x0E
#define TRAJ_READ_CONFIG               0x0F
#define TRAJ_TURN                      0x10

#define TRAJ_NOT_COMPLETED             0x00
#define TRAJ_COMPLETED                 0x01

#define TRAJ_STOP_MOTOR_OFF             256
#define TRAJ_STOP_ABRUPT                512
#define TRAJ_STOP_SMOOTH               1024

// CMD_MOTOR
#define MOTOR_ENABLE                      1
#define MOTOR_DISABLE                     2
#define MOTOR_MOVE                        3

#define MOTOR_RIGHT                       1        ///< Sélection du moteur de droite (moteur 1)
#define MOTOR_LEFT                        2        ///< Sélection du moteur de gauche (moteur 2)
#define MOTOR_BOTH                        3        ///< Sélection des 2 moteurs simultanément

// XXX à compléter
#define READ_VERSION                   0x00
#define READ_FLASH                     0x01
#define WRITE_FLASH                    0x02
#define ERASE_FLASH                    0x03
#define READ_EEDATA                    0x04
#define WRITE_EEDATA                   0x05
#define READ_CONFIG                    0x06
#define WRITE_CONFIG                   0x07
#define UPDATE_LED                     0x32
#define RESET                          0xFF

#define FLASH_BOOT_START               0x00
#define FLASH_BOOT_END                0x7FF
#define FLASH_VECTOR_START            0x800
#define FLASH_VECTOR_END              0x829
#define FLASH_PAGE_START              0x82A
#define FLASH_PAGE_END               0x7FFF

#define PORTA_RA0                         1
#define PORTA_RA1                         2
#define PORTA_RA2                         4
#define PORTA_RA3                         8
#define PORTA_RA4                        16
#define PORTA_RA5                        32
#define PORTA_RA6                        64

#define PORTB_RB0                         1
#define PORTB_RB1                         2
#define PORTB_RB2                         4
#define PORTB_RB3                         8
#define PORTB_RB4                        16
#define PORTB_RB5                        32
#define PORTB_RB6                        64
#define PORTB_RB7                       128

#define PORTC_RC0                         1
#define PORTC_RC1                         2
#define PORTC_RC2                         4
#define PORTC_RC4                        16
#define PORTC_RC5                        32
#define PORTC_RC6                        64
#define PORTC_RC7                       128

#define PORTD_RD0                         1
#define PORTD_RD1                         2
#define PORTD_RD2                         4
#define PORTD_RD3                         8
#define PORTD_RD4                        16
#define PORTD_RD5                        32
#define PORTD_RD6                        64
#define PORTD_RD7                       128

#define PORTE_RE0                         1
#define PORTE_RE1                         2
#define PORTE_RE2                         4
#define PORTE_RE3                         8

#endif  // PC_INTERFACE_H
