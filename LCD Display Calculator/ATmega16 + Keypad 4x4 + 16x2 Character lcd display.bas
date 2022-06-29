'======================================================================='

' Title: LCD Display Calculator
' Last Updated :  02.2022
' Author : A.Hossein.Khalilian
' Program code  : BASCOM-AVR 2.0.8.5
' Hardware req. : ATmega16 + Keypad 4x4 + 16x2 Character lcd display

'======================================================================='

$regfile = "m16def.dat"
$crystal = 1000000

Config Lcdpin = Pin Db4 = Pinb.2 , Db5 = Pinb.3 , Db6 = Pinb.4 , _
Db7 = Pinb.5 , E = Pinb.1 , Rs = Pinb.0
Config Lcd = 16 * 2

Config Kbd = Portc , Debounce = 50 , Delay = 50

Config Portd.0 = Input
Sh Alias Pind.0

Dim Shft As Bit
Dim Op3 As Byte
Dim Key As Byte
Dim A As Byte
Dim  B As Byte
Dim S As Bit
Dim N As Bit
Dim Op As String * 1
Dim Op2 As String * 1
Dim K As Byte
Dim Ans As Single
Dim F1 As Single
Dim F2 As Single
Dim F3 As Single
Dim In1(8) As Byte
Dim I1 As Byte
Dim In2(8) As Byte
Dim I2 As Byte
Dim Stn(9) As Long

Stn(1) = 1
Stn(2) = 10
Stn(3) = 100
Stn(4) = 1000
Stn(5) = 10000
Stn(6) = 100000
Stn(7) = 1000000
Stn(8) = 10000000

Cursor Noblink

'-----------------------------------------------------------

Start_new:
  Cursor Off
  I1 = 0 : I2 = 0 : S = 0 : N = 0 : Shft = 0
  Cls : Locate 2 , 1 : Lcd "0"

''''''''''''''''''''''''''''''

Do
   Op = ""
   Gosub Set_number1
   If S = 1 Then Goto Start_new                          'check for reset
   If Shft = 1 Then Gosub Cal_fun
   Cls
   Gosub Set_number2
   If S = 1 Then Goto Start_new                          'check for reset
   Gosub Functions
   Cls
   Locate 1 , 1 : Lcd "ANS="
   Locate 2 , 1 : Lcd Ans

Do
   K = Getkbd()
   Loop Until K <> 16
   Goto Start_new
Loop

End                                                         'end program

'-----------------------------------------------------------

Cal_fun:

Cls
Lcd "1=SIN   2=COS"
Locate 2 , 1
Lcd "3=TAN   4=SQR"
Shft = 0
I1 = 0
I2 = 0
S = 0
N = 0
Gosub Keypad
Op3 = Key
Cls
Gosub Set_number1

Select Case Op3

   Case 1:
   Ans = Sin(f1)
   Cls : Lcd "SIN " ; F1 ; " ="
   Locate 2 , 1 : Lcd Ans

   Case 2:
   Ans = Cos(f1)
   Cls : Lcd "COS " ; F1 ; " ="
   Locate 2 , 1 : Lcd Ans
   Case 3:
   Ans = Tan(f1)
   Cls : Lcd "TAN " ; F1 ; " ="
   Locate 2 , 1 : Lcd Ans
   Case 4:
   Ans = Sqr(f1)
   Cls : Lcd "SQR " ; F1 ; " ="
   Locate 2 , 1 : Lcd Ans

End Select

Do                                                       'wait and display ANS on lcd until press key
K = Getkbd()
Loop Until K <> 16
Goto Start_new

Return

''''''''''''''''''''''''''''''

Functions:

If Op2 = "+" Then Ans = F1 + F2
If Op2 = "-" Then Ans = F1 - F2
If Op2 = "*" Then Ans = F1 * F2
If Op2 = "/" Then Ans = F1 / F2

Return

''''''''''''''''''''''''''''''

Keypad:

Do

K = Getkbd()
If Sh = 0 Then
Shft = 1 : Waitms 100
End If

Select Case K

   Case 7 : Key = 0
   Case 2 : Key = 1
   Case 6 : Key = 2
   Case 10 : Key = 3
   Case 1 : Key = 4
   Case 5 : Key = 5
   Case 9 : Key = 6
   Case 0 : Key = 7
   Case 4 : Key = 8
   Case 8 : Key = 9
   Case 3 : Op = "c"
   Case 11 : Op = "="
   Case 12 : Op = "/"
   Case 13 : Op = "*"
   Case 14 : Op = "-"
   Case 15 : Op = "+"
End Select

Loop Until K <> 16

Do
K = Getkbd()
Loop Until K = 16
K = 0

Return

''''''''''''''''''''''''''''''

Set_number1:

I1 = 0 : F3 = 0 : F1 = 0
Locate 1 , 1
Do
Gosub Keypad
If Shft = 1 Then Return                             'back for select SIN COS TAN ...
If Ans = 0 And Op = "-" And N = 0 Then               'check for input negative namber
N = 1 : Op = ""
Locate 1 , 1 : Lcd "-"
Goto End_action
End If

If I1 = 0 Then                                 'check for functions and old ans EXP: ans+2=
If Op = "+" Or Op = "-" Or Op = "/" Or Op = "*" Then
F1 = Ans
Goto No_c
End If
End If
If Op = "c" Then
S = 1                                               'set s for check status
Cls
Goto No_c                                           'clear and goto start
End If

If Op = "+" Or Op = "-" Or Op = "/" Or Op = "*" Or Op = "=" Then Goto Lable1       'jump if not key
If I1 < 8 Then
Incr I1
In1(i1) = Key                                      'copy key to array
Lcd In1(i1)                                        'display key
End If

''''''''''''''''''''''''''''''

End_action:
Loop

''''''''''''''''''''''''''''''

Lable1:
B = I1
For A = 1 To B Step 1                              'change numbers to long nubmer
F3 = In1(a) * Stn(i1)
F1 = F1 + F3
Decr I1
Next
If N = 1 Then F1 = F1 * -1                         'negative number
N = 0

''''''''''''''''''''''''''''''

No_c:
Op2 = Op
If Op = "=" Then Op2 = ""
Return

''''''''''''''''''''''''''''''

Set_number2:
I2 = 0 : F3 = 0 : F2 = 0 : Op = ""
Locate 1 , 1
Lcd F1 ; Op2
Do
Gosub Keypad
If Op = "c" Then
S = 1                                               'set s for check status
Cls
Goto No_c2                                          'clear and goto start
End If

If Op = "+" Or Op = "-" Or Op = "/" Or Op = "*" Or Op = "=" Then Goto Lable2       'jump if not key
If I2 < 8 Then
Incr I2
In2(i2) = Key                                      'copy key to array
Lcd In2(i2)                                        'display key
End If

Loop

''''''''''''''''''''''''''''''

Lable2:
B = I2
For A = 1 To B Step 1
F3 = In2(a) * Stn(i2)
F2 = F2 + F3
Decr I2
Next

''''''''''''''''''''''''''''''

No_c2:
Return