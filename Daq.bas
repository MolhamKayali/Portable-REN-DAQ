' ******************************************************************************
' * Title         : CORKY ROBOT.bas                                            *
' * Version       : 2.4                                                        *
' * Last Updated  : 07.8.20112                                                 *
' * Target Board  : Phoenix - REV 1.00                                         *
' * Target MCU    : ATMega128A                                                 *
' * Author        : Molham Kayali                                              *
' * IDE           : BASCOM AVR 2.0.7.0                                         *
' * UNIVERSITY    : Aleppo University                                          *
' * Description   : Portable DAQ                                               *
' ******************************************************************************
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

$regfile = "m128def.dat"
$crystal = 8000000
$baud = 9600
$lib "glcdKS108.lib"
$lib "I2C_TWI.LBX"

Config Sda = Portd.1
Config Scl = Portd.0
Config Twi = 100000

Config Clock = User
Const Ds3232w = &HD0
Const Ds3232r = &HD1
I2cinit
Config Date = Dmy , Separator = /
'-----------------------
'------------------------[INT0]
Config Int0 = Rising
On Int0 Int0_isr

'-----------------------

Config Adc = Single , Prescaler = Auto , Reference = Avcc
On Adc Adc_isr
Enable Adc

Config Single = Scientific , Digits = 2

Config Graphlcd = 128 * 64sed , Dataport = Portc , Controlport = Porta , Ce = 3 , Ce2 = 4 , Cd = 5 , Rd = 6 , Reset = 2 , Enable = 7

Config Timer0 = Counter , Edge = Rising                     'Humidity Sensor Fout Counter
On Timer0 Count_ovf

Config Timer1 = Timer , Prescale = 256
On Timer1 Secound
Timer1 = 34286
Dim Offset As Integer , Offset_h As Byte , Offset_l As Byte
Dim Sensitivity As Integer , Sensitivity_h As Byte , Sensitivity_l As Byte
Dim Uart_var As Byte , Freq_flag As Bit , Frequency As Word
Dim Count_ovf_num As Byte , Humidity As Single , Tcs As Byte , Adres As Byte
Dim Dsp_flag As Bit , Weekday As Byte
Dim Lm35_p_0 As Word
Dim Lm35_n_0 As Word
Dim Ldr_0 As Word
Dim Dcvolt_0 As Word
Dim Current_0 As Word
Dim Pressure_0 As Word
Dim I As Byte
Dim Last_sec As Byte
Const V_ref = 5
Const At24cxxw_hu = &HA2
Const At24cxxr_hu = &HA3
Dim Lm35 As Single
Dim Lm35_f As Single
Dim Ldr As Single
Dim Dcvolt As Single
Dim Current As Single
Dim Pressure As Single
Dim P As Single
Dim A As Single
Dim B As Single
Dim Channel As Byte
Dim C As Byte
Dim Arr(8) As Word
Dim Count_5s As Byte , Adc_val As Word
Dim Ac_flag As Bit
Dim X As Byte , Y As Byte
Dim H As Dword , V As Dword , Button As String * 1
Dim X_coord128 As Dword , Y_coord64 As Dword

Dim X1 As Byte , Ya As Integer , Y1 As Single , Q As Single

Enable Int0
Start Ac : Cls
Enable Interrupts

Ddrf.0 = 0 : Ddrf.1 = 0
Config Portc.0 = Output : Drive_a Alias Portf.0             'Set PinF.5 as Output
Config Portc.1 = Output : Drive_b Alias Portf.1
Gosub Logo
 Waitms 10 : Cls : Gosub Show_image
Do
Gosub Read_touch : Gosub Calc_coord
   Waitms 10

If Ischarwaiting() = 1 Then
      Uart_var = Inkey()
      If Uart_var = &HAA Then Gosub Get_ferq
      Reset Freq_flag
   End If

   If Freq_flag = 1 Then
      Reset Freq_flag
      Frequency = 256 * Count_ovf_num
      Frequency = Frequency + Timer0
      Print "Frequency: " ; Frequency
      Gosub Read_eeprom
      Gosub Calc_humidity
   End If

   If Dsp_flag = 1 Then
      Reset Dsp_flag : Gosub Getdatetime
      Setfont Font8x8
      Lcdat 1 , 1 , "Time: " ; Time$
      Lcdat 2 , 1 , "Date: " ; Date$
   End If

   Cls
   Gosub Read_adc : Gosub Lm35_sensor : Gosub Ldr_sensor    'ADC
   Gosub Display_temp : Gosub Display_lux : Gosub Display_solar : Gosub Solar_power : Gosub Air_pressure : Gosub Humidity_display : Wait 1
   Cls
'   Gosub Draw : Wait 1

Loop
End
'---<[End Main]
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

'--->ADC
Read_adc:
For C = 0 To 7
   Channel = C : Start Adc : Idle : Stop Adc
   'Print Adc_val
   Arr(c + 1) = Adc_val
Next C

Return
 '--->[Read ADC Input Values]
Adc_isr:
   Adc_val = Getadc(channel)
Return
 '---SENSORS EQUAITION
Lm35_sensor:
  Lm35_p_0 = Arr(4) : Lm35_n_0 = Arr(3)
  Lm35 = Lm35_p_0 - Lm35_n_0 : Lm35 = Lm35 / 2.048          ';* V_ref : Lm35 = Lm35 / 1024
  Print Lm35
Return
 '--->[Convert C > F] Fahrenheit = 1.8*(Celsius) + 32
C_to_f:
   Lm35_f = 1.8 * Lm35 : Lm35_f = Lm35_f + 32
   Print Lm35_f
Return
'--->[Calculate LUX]
'Vo = 5 * Rl /(rl + 3.3) > Lux =(2500 / Vo - 500) / 3.3
Ldr_sensor:
 Ldr_0 = Arr(6)
 Ldr = Ldr_0 * V_ref : Ldr = Ldr / 10.24
 Print Ldr
Return
 '--->[DC voltage]
 '22(10/10+39)=4.48
 '4.48/1024=4.375m
 Dc_voltage:
 Dcvolt_0 = Arr(7)
 Dcvolt = Dcvolt_0 * V_ref : Dcvolt = Dcvolt / 1024
 Print Dcvolt
 Dcvolt = Dcvolt * 4.48
 Print Dcvolt
 Return
 '--->[DC current 1]
 Dc_current:
 Current_0 = Arr(8)
 Current = Current_0 * V_ref : Current = Current / 1024
 Print Current
 Return
 '--->[Pressure ]
'Vout = VS x (0.009 x P - 0.095) ± (Pressure Error x Temp. Factor x 0.009 x VS)
'VS = 5.0 ± 0.25 Vdc

'P = [vout /(0.009 * Vs)] + [0.095 / 0.009]
'0.009 * 5 = 0.045
'0.095 / 0.009 = 10.555
 Air_pressure:
 Pressure_0 = Arr(5)
 Pressure = Pressure_0 * V_ref : Pressure = Pressure / 1024
 Pressure = Pressure_0 / 0.045
 Pressure = Pressure + 10.55
 Print Pressure
'------------------------
Get_ferq:
   Count_ovf_num = 0 : Frequency = 0
   Enable Interrupts
   Timer1 = 34286 : Timer0 = 0
   Enable Timer1 : Enable Timer0 : Start Timer1 : Start Timer0
Return
'------------------------
Read_eeprom:
   Adres = 10
   I2cstart
   I2cwbyte At24cxxw_hu
   I2cwbyte Adres
   I2cstart
   I2cwbyte At24cxxr_hu
   I2crbyte Sensitivity_h , Ack
   I2crbyte Sensitivity_l , Ack
   I2crbyte Offset_h , Ack
   I2crbyte Offset_l , Ack
   I2crbyte Tcs , Nack
   I2cstop

   Print "Error   : " ; Err
   Print "Sensit_H: " ; Sensitivity_h
   Print "Sensit_L: " ; Sensitivity_l
   Print "Offset_H: " ; Offset_h
   Print "Offset_L: " ; Offset_l
   Print "Tcs     : " ; Tcs
   Print "--------------------"
Return
'------------------------
Count_ovf:
   Incr Count_ovf_num
Return
'------------------------
Secound:
   Stop Timer0 : Stop Timer1 : Disable Interrupts
   Set Freq_flag
Return
'------------------------
Calc_humidity:
   Offset = Makeint(offset_l , Offset_h) : Print "Offset= " ; Offset
   Sensitivity = Makeint(sensitivity_l , Sensitivity_h) : Print "Sens= " ; Sensitivity
   Humidity = Offset - Frequency
   Humidity = Humidity * Sensitivity
   Humidity = Humidity / 4096
   Print "H%= " ; Humidity
   Print "--------------------"
Return
'------------------------
'--->[Display Temperature]
Display_temp:
   Gosub C_to_f :
   Setfont Font8x8
   Lcdat 1 , 1 , "C Temp:" ; Lm35 ; " C"
   Lcdat 2 , 1 , "F Temp:" ; Lm35_f ; " f"
Return
 '--->[Display LUX]
Display_lux:
   Setfont Font8x8
   Lcdat 3 , 1 , "Radiate:" ; Ldr ; " L"
Return
'--->[Display Dc voltage   DC current ]
Display_solar:
   Setfont Font8x8
   Lcdat 4 , 1 , "Voc: " ; Dcvolt ; "V"
   Lcdat 5 , 1 , "Isc: " ; Current ; "A"
Return
'--->[Display solar power  ]
Solar_power:
P = Dcvolt * Current
Print P
Setfont Font8x8
   Lcdat 6 , 1 , "power: " ; P ; "W"
   Lcdat 7 , 1 , "pres: " ; Pressure ; "B"
Return
'--->[Display Humidity ]
Humidity_display:
   Lcdat 8 , 1 , "Humidity:" ; P ; "%"
Return
'-----------------------
'--->[Set Text Font Size - 6.Pixel(X) x 8.Pixel(Y)]
'5 x 8 Font >> If GLCD = 64 x 128 Then >> 8-Row(Y) x 21-Col(X)
Logo:
   Cls
   Setfont Font8x8
   Lcdat 3 , 1 , "  I N F I N T E " : Wait 1
   Lcdat 4 , 1 , "   G R E E N" : Wait 1
   Lcdat 5 , 1 , "  E N E R G Y" : Wait 1
   Lcdat 6 , 1 , "  S O U R C E" : Wait 1
   Setfont Font16x16 : Cls
   Lcdat 3 , 1 , "IGES.CO" : Wait 3 : Cls
   Lcdat 2 , 1 , " Molham" : Wait 2
   Lcdat 5 , 1 , " Kayali" : Wait 2
   Setfont Font8x8 : Cls
   Lcdat 1 , 1 , "Portable Daq Sys" : Wait 1
   Lcdat 3 , 1 , "for" : Wait 1
   Lcdat 5 , 1 , "Renewable Energy" : Wait 1
Return
'-----------------------
Read_touch:
   '--->Read X
   Set Drive_a                                              'Driver-A [LEFT drive on, RIGHT drive on, TOP drive off]
   Reset Drive_b                                            'Driver-B [BOTTOM drive off]
   Waitms 5
   H = Getadc(1)                                            'Read X-axis coordinate [BOTTOM]
   'Print "ADC X : " ; X

   '--->Read Y
   Reset Drive_a                                            'Driver-A [LEFT drive off, RIGHT drive off, TOP drive on]
   Set Drive_b                                              'Driver-B [BOTTOM drive on]
   Waitms 5
   V = Getadc(2)                                            'Read the Y-axis coordinate [LEFT]
   'Print "ADC Y : " ; Y
Return
'-----------------------
'--->[Claculating the Coordinations]
Calc_coord:
   V = V * 128 : X_coord128 = V / 1024                      'X_coord128 = (X * 128) / 1024
   H = H * 64 : H = H / 1024 : Y_coord64 = 64 - H           'Y_coord064 = 64 - ((Y * 64) / 1024)

  'Print "X: " ; X_coord128
  'Print "Y: " ; Y_coord64

   If X_coord128 > 16 And X_coord128 < 40 Then
      If Y_coord64 > 15 And Y_coord64 < 30 Then Button = "A"
      If Y_coord64 > 40 And Y_coord64 < 55 Then Button = "D"
   Elseif X_coord128 > 52 And X_coord128 < 76 Then
      If Y_coord64 > 15 And Y_coord64 < 30 Then Button = "B"
      If Y_coord64 > 40 And Y_coord64 < 55 Then Button = "E"
   Elseif X_coord128 > 90 And X_coord128 < 114 Then
      If Y_coord64 > 15 And Y_coord64 < 30 Then Button = "C"
      If Y_coord64 > 40 And Y_coord64 < 55 Then Button = "F"
   End If

   If X_coord128 > 0 Then Print "The Button is: " ; Button
   Button = "?"
Return
'-----------------------
'--->[Drawing Images on the GLCD]
Show_image:
   Showpic 0 , 0 , Image
Return
'-----------------------
'DRAW SINE
'Draw:
'Line(0 , 40) -(127 , 39) , 1

'For X1 = 0 To 127
'Q = X1 * 0.049
'Y1 = 22 * Sin(q )
'Ya = -y1 + 40
'Pset X1 , Ya , 1
'Y1 = 22 * Cos(q )
'Ya = -y1 + 40
'Pset X1 , Ya , 1
'Next X1
'End
'Return
Initial:
   Time$ = "16:30:30"                                       'Initial Time
   Date$ = "08/07/12"                                       'Initial Date
   Gosub Dec_bcd_date : Gosub Dec_bcd_time
   Gosub Setdate : Gosub Settime : Gosub Set_sqw
Return
'-------------
'--->[INT7 - SQW]
Int0_isr:
   Set Dsp_flag
Return
'-------------
'--->[DS3232 Get Time & Date]
Getdatetime:
   I2cstart
   I2cwbyte Ds3232w
   I2cwbyte 0

   I2cstart
   I2cwbyte Ds3232r
   I2crbyte _sec , Ack
   I2crbyte _min , Ack
   I2crbyte _hour , Ack

   I2crbyte Weekday , Ack
   I2crbyte _day , Ack
   I2crbyte _month , Ack
   I2crbyte _year , Nack
   I2cstop

   Gosub Bcd_dec_date : Gosub Bcd_dec_time
Return
'-------------
'--->[DS3232 Set Date]
Setdate:
   I2cstart
   I2cwbyte Ds3232w
   I2cwbyte 4
   I2cwbyte _day
   I2cwbyte _month
   I2cwbyte _year
   I2cstop
Return
'-------------
'--->[DS3232 Set Time]
Settime:
   I2cstart
   I2cwbyte Ds3232w
   I2cwbyte 0
   I2cwbyte _sec
   I2cwbyte _min
   I2cwbyte _hour
   I2cstop
Return
'-------------
'--->[DS3232 Set Settings]
Set_sqw:
   I2cstart
   I2cwbyte Ds3232w
   I2cwbyte &H07
   I2cwbyte &B00010000
   I2cstop
Return
'-------------
'--->[DEC>BCD DATE]
Dec_bcd_date:
   _day = Makebcd(_day) : _month = Makebcd(_month) : _year = Makebcd(_year)
Return
'-------------
'--->[DEC>BCD TIME]
Dec_bcd_time:
   _sec = Makebcd(_sec) : _min = Makebcd(_min) : _hour = Makebcd(_hour)
Return
'-------------
'--->[BCD>DEC DATE]
Bcd_dec_date:
   _day = Makedec(_day) : _month = Makedec(_month) : _year = Makedec(_year)
Return
'-------------
'--->[BCD>DEC TIME]
Bcd_dec_time:
   _sec = Makedec(_sec) : _min = Makedec(_min) : _hour = Makedec(_hour)
Return
'-----------------------
'--->[Include Fonts from Extrnal Font Files]
   $include "Font5x8.font"
   $include "Font6x8.font"
   $include "Font8x8.font"
   $include "Font16x16.font"
'-----------------------
   Image:
$bgf "RE5.bgf"
'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
''-----------------------------' Y
'| '-----'   '-----'   '-----' | 15
'| |  A  |   |  B  |   |  C  | |
'| '-----'   '-----'   '-----' | 30
'|                             |
'| '-----'   '-----'   '-----' | 40
'| |  D  |   |  E  |   |  F  | |
'| '-----'   '-----'   '-----' | 55
''-----------------------------'
'X:16 > 40   52 > 76   90 > 114
