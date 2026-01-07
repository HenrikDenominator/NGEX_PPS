
'FFPE: magLEAD DNA extraction Ver 1.7b 20171017 AH/LM Major edit.
'FFPE: magLEAD DNA extraction Ver 1.6e 20160920 KH Improved sample asp after lysis. Introduction of serienumber.Tip at safe stage (Z) during lysis incub.
'FFPE: magLEAD DNA extraction Ver 1.6d 20160809 KH Optimized deparaffinisation. Changed height for lysis asp. of sample. 
'FFPE: magLEAD DNA extraction Ver 1.6c 20160714 KH
'FFPE: DNA extraction Ver 1.6c 20160429 KH vacuum increased (from -200 to -280)
'FFPE: DNA extraction Ver 1.6b 20160412 KH 400 ul wash volume.
'FFPE: DNA extraction Ver 1.6a 20160412 KH Changed mixing speed during wash. 80 deg. for 6/12 lysis incubations. Less vacuum created for elution. 800 ul wash volume.
'FFPE: DNA extraction Ver 1.6 20150610 KH 
'FFPE: DNA extraction Ver 1.5 20150610 KH Generic commands
'FFPE: DNA extraction Ver 1.4 20141028 MFL Latest version 1.4b
'FFPE: DNA extraction Ver 1.4b 20141024 MFL Reduced Air Asp After Tip pickup
'FFPE: DNA extraction Ver 1.4a 20141020 MFL Removed bug Sample Asp. Change EluteVol.Implemented Z speed piercing changed from 24000 to 15000  
'FFPE: DNA extraction Ver 1.2 20140925 MFL Changed Recapture Vol for MagSep in Well-1
'FFPE: DNA extraction Ver 1.1 20140923 MFL Removed Checkpoints during deparaffinization step
'FFPE: DNA extraction Ver 1.0 20140904 MFL (latest version 02e)

'**********NA Purification **********
ClearScreen
	Print 0, 0, "NGEx magLEAD   "
	Print 0, 1, "FFPE: DNA extraction "
	Print 0, 2, "Ver 1.7"

	Print 0, 3, "Initializing.."

'-----------Initialization-------------------------
Org Z
Org M
Org Y
Org P

'************** Reagent ****************************************

'Collect: 1.5 ml tube (DNA Eluate)
'1st tip: Empty position
'2nd tip: DN100 Tip   (Tip sheath)
'Collect: Empty position
'Well 1 : DNA Beads		120 ul
'Well 2 : LB1	 		180 ul
'Well 3 : Prot K		40 ul 
'Well 4 : LB2+100%EtOH 		400 ul (200ul of each mixed together)
'Well 5 : MQ		 	250 ul
'Well 6 : LB2-Wash		400 ul
'Well 7 : Wash		 	400 ul
'Well 8 : Wash		 	400 ul
'Well 9 : Empty
'Well 10: Elution-DNA		1000 ul
'Well 11: Sample + HC 1,5ml tube
'Well 12: DNA Elution


'************* User Variables *************
Dim @LysisTime1
Dim @BindTime
Dim @WashTime
Dim @EluteTimeDNA
Dim @MagTime

Dim @LysisTemp1
Dim @LysisTemp2
Dim @EluteTempDNA

Dim @SampleVol
Dim @LB1Vol
Dim @LB2Vol
Dim @ProtKVol
Dim @BeadVol
Dim @TotSampleBeadVol
Dim @TotSampleBuffVol
Dim @MixTotSampleBuffVol
Dim @TempoMixTotSampleBuffVol
Dim @WashVol
Dim @WashVol2
Dim @TempoWashVol2
Dim @Vol
Dim @HistVol
Dim @MQVol

Dim @N
Dim @HoleYPos
Dim @HoleZMove
Dim @HoleZPos
Dim @HolePMove
Dim @Input
Dim @PiercingPos(10)
Dim @PiercingZProcess(10)


'************* Set Incub time, temp & Volumes*************
Let @LysisTime1, 600		
Let @BindTime, 300	
Let @WashTime, 45 

Let @EluteTimeDNA, 300	
Let @MagTime, 20

Let @LysisTemp1, 680	
Let @LysisTemp2, 900	
Let @EluteTempDNA, 680	

Let @SampleVol, 420
Let @LB1Vol, 180
Let @LB2Vol, 400 ' 200ul LB2 + 200ul 100%EtOH
Let @ProtKVol, 40 
Let @BeadVol, 120

Let @WashVol, 250
Let @WashVol2, 400


Let @HistVol, 1000
Let @MQVol, 200

@TotSampleBeadVol = @SampleVol + @BeadVol
@TotSampleBuffVol = @SampleVol + @LB2Vol				

@TempoMixTotSampleBuffVol = @TotSampleBuffVol * 8
@MixTotSampleBuffVol = @TempoMixTotSampleBuffVol / 10

@TempoWashVol2 = @WashVol2 * 95
@WashVol2 = @TempoWashVol2 / 100


'************* Protocol Variables *************
Dim @PreAir
Let @PreAir, 100
Dim @num
Dim @Temp
Dim @Slim_BF
Dim @Slim_Mag

Dim @Count		'loop variable
Dim @Pos		'well position variable
Dim @E30IsSel
Dim @E50IsSel
Dim @E100IsSel
Dim @KeyCode



'************* Speed Setting ***********************************
Dim @Speed_P_XH
Dim @Speed_P_HH
Dim @Speed_P_H
Dim @Speed_P_M
Dim @Speed_P_ML
Dim @Speed_P_L
Dim @Speed_P_LL
Dim @Speed_P_LLL

Let @Speed_P_XH, 15000
Let @Speed_P_HH, 8000
Let @Speed_P_H, 5000
Let @Speed_P_M, 2500
Let @Speed_P_ML, 1000
Let @Speed_P_L, 500
Let @Speed_P_LL, 300
Let @Speed_P_LLL, 100


Dim @P_InitPosition
Dim @P_StartPos
@P_InitPosition = @Axis.P.Pos.FullDispense
@P_StartPos = @P_InitPosition
Add @P_StartPos, 500

'************* Magnet Setting ****************************
Dim @Magnet_H
Dim @Magnet_M
Dim @Magnet_S
Dim @Magnet_Thick  

Let @Magnet_H, @Axis.M.Pos.Narrow 
Let @Magnet_M, @Axis.M.Pos.Narrow
Let @Magnet_S, @Axis.M.Pos.Narrow
Let @Magnet_Thick, @Axis.M.Pos.Thick  
Let @Axis.M.Speed, 5000
Add @Magnet_H, 100
Add @Magnet_M, -400
Add @Magnet_S, -900

Dim @conf_skip_to_binding
Let @conf_skip_to_binding, 1

'**********NA Purification **********
*Check

ClearScreen
	Print 0, 0, "ExScale magLEAD   "
	Print 0, 1, "FFPE: DNA extraction "
	Print 0, 2, "Ver 1.7"
    Print 0,3, "         OK=RETURN  "
	KeyIn @Input
	If @Input <> 11 Then GoTo *Check

'===============<< Information >>===============

*Confirm
*Information2
	ClearScreen
	Print 0,0, "Well 11: 1.5 ml sample tube"
	Print 0,1, "Hole 1 : Empty 1.5ml tube"
	Print 0,3, "         OK=RETURN  "
        KeyIn @Input
	If @Input <> 14 Then GoTo *EscI1
	GoTo *Check
	*EscI1
        If @Input <> 11 Then GoTo *Information2

*Information3
	ClearScreen
	Print 0,0, "Hole 3 : Tip"

	Print 0,3, "OK=RETURN, Prev.=ESC"
        KeyIn @Input
	If @Input <> 14 Then GoTo *EscI0
	GoTo *Information2
	*EscI0
        If @Input <> 11 Then GoTo *Information3


'===============<< Information >>===============
*Confirm2
*EluVol
	ClearScreen
	Print 0,0, "Select elution vol. "
	Print 0,1, "1.  30 ul 3. 100 ul "
	Print 0,2, "2.  50 ul "
	Print 0,4, "Next=1-3, Prev.=ESC"
	
        KeyIn @Input
	If @Input <> 14 Then GoTo *Esc3
	GoTo *Confirm
	*Esc3
	If @Input <> 1 Then GoTo *EluVolSel1
		@E30IsSel = 1
		@E50IsSel = 0
		@E100IsSel = 0
		GoTo *Display
	*EluVolSel1
	If @Input <> 2 Then GoTo *EluVolSel2
		@E30IsSel = 0
		@E50IsSel = 1
		@E100IsSel = 0
		GoTo *Display
	*EluVolSel2
	If @Input <> 3 Then GoTo *EluVol
		@E30IsSel = 0
		@E50IsSel = 0
		@E100IsSel = 1
		GoTo *Display

*Display
	ClearScreen

*Display3
        If @E30IsSel Then GoTo *Elution30
        If @E50IsSel Then GoTo *Elution50
        If @E100IsSel Then GoTo *Elution100
*Elution30
        Print 0,0, "Elution : 30 ul"
        GoTo *Display4
*Elution50
        Print 0,0, "Elution : 50 ul"
        GoTo *Display4
*Elution100
        Print 0,0, "Elution : 100 ul"
        GoTo *Display4

*Display4
        Print 0,3, "OK=RETURN, Prev.=ESC "
        KeyIn @Input
	If @Input <> 14 Then GoTo *Esc4
	GoTo *Confirm
	*Esc4
        If @Input <> 11 Then GoTo *Display4

		
*StartProc
    ClearScreen
	Print 0,0, "Ready to Start."
	Print 0,1, "Push START key!"
	Print 0,3, "Next=START,Prev.=ESC" 
	
	KeyIn @Input
	If @Input <> 14 Then GoTo *Esc50
	GoTo *Confirm
	*Esc50
	If @Input <> 18 Then GoTo *StartProc		

HoodLock On 'Let @Cover.Locked, 1

ClearScreen	
If @conf_skip_to_binding Then GoTo *skip_to_binding

'=============<< Piercing (12GC) >>===============

        ClearScreen
	Print 0,0, "Piercing...."

        @PiercingPos(1) = @Proc(1).Y.Center
        @PiercingPos(2) = @Proc(2).Y.Center
        @PiercingPos(3) = @Proc(3).Y.Center
        @PiercingPos(4) = @Proc(4).Y.Center
        @PiercingPos(5) = @Proc(5).Y.Center
        @PiercingPos(6) = @Proc(6).Y.Center
        @PiercingPos(7) = @Proc(10).Y.Center
        @PiercingPos(8) = @Proc(9).Y.Center
        @PiercingPos(9) = @Proc(8).Y.Center
        @PiercingPos(10) = @Proc(7).Y.Center

        @PiercingZProcess(1) = @Proc(1).Z.Process
        @PiercingZProcess(2) = @Proc(2).Z.Process
        @PiercingZProcess(3) = @Proc(3).Z.Process
        @PiercingZProcess(4) = @Proc(4).Z.Process
        @PiercingZProcess(5) = @Proc(5).Z.Process
        @PiercingZProcess(6) = @Proc(6).Z.Process
        @PiercingZProcess(7) = @Proc(10).Z.Process
        @PiercingZProcess(8) = @Proc(9).Z.Process
        @PiercingZProcess(9) = @Proc(8).Z.Process
        @PiercingZProcess(10) = @Proc(7).Z.Process

        Let @N, 0

        AMove Z, @Stage.Z.Safe
        Let @HolePMove, @Axis.P.Pos.FullDispense
        Add @HolePMove, -10400				
        AMove P, @HolePMove				

*Loop_Hole

	Add @N, 1
		Let @HoleYPos, @PiercingPos(@N) '@Proc(@N).Y.Center
		Add @HoleYPos, 640					
		Let @HoleZMove, @Proc(@N).Z.Process
		Add @HoleZMove, -16000					
		Let @HoleZPos, @Proc(@N).Z.Process			
		Add @HoleZPos, -20000					
				
		Let @Axis.Y.Speed, 6000
		AMove Y, @HoleYPos
		Let @Axis.Z.Speed, 15000   '20000 
		AMove Z, @HoleZMove
		Let @Axis.Z.Speed, 6000 
		AMove Z, @HoleZPos
		Let @Axis.Z.Speed, 6000 
		AMove Z, @HoleZMove

        If @N < 10 Then GoTo *Loop_Hole

	Let @Axis.Z.Speed, 15000 'Let @Axis.Z.Speed, 24000  
        AMove Z, @Stage.Z.Safe
        AMove P, @Axis.P.Pos.FullDispense
        Let @Axis.Y.Speed, 8000


'=============<< New Tip >>===============
	ClearScreen
	Print 0,0, "Get tip"
	AMove Y, @Tip(2).Y.Center
	GetTip
	Org Z
	AMove P, @Axis.P.Pos.FullDispense
	Aspirate 300


'---(Well 5)---

ClearScreen
Print 0, 0, "Deparaffinization"
'Print 0, 1, "Transfer MQ"
 
	MoveToPos(5)
	HeightMM(0)
	RMove Z, 400
	AspirateS(@MQVol, @Speed_P_L)
	

'---(Well 11)---
'ClearScreen
'Print 0, 0, "Well 11"
	MoveToPos(11)
	HeightMM(5)
	DispenseS(@MQVol, @Speed_P_L)



'==========<<  HC removal  Sample >>============

'ClearScreen
'Print 0, 0, "Well 11"
'Print 0, 1, "Removal HC"

MoveToPos(11)
HeightMM(12)
Wait 5000
AspirateS(700, @Speed_P_L) 'AspirateS(@HistVol, @Speed_P_L)
HeightMM(9)
AspirateS(300, @Speed_P_LL)

MoveToPos(-1) 
HeightMM(10)
DispenseS(@HistVol, @Speed_P_M)

'HeightMM(25)
AMove Z, @Stage.Z.Safe
RMove Z, -4000
RMove Y, 280
Wait 7000
DispenseS(75, @Speed_P_M)
RMove Y, -560
Wait 3000
DispenseS(75, @Speed_P_M)
RMove Y, 280
AMove Z, @Stage.Z.Safe 'HeightMM(20)
Wait 100
AspirateS(150, @Speed_P_M)

'==========<< LB1 + ProtK >>============
ClearScreen
Print 0, 0, "Lysis"
	MoveToPos(2)
	HeightMM(0)
	RMove Z, 400
	AspirateS(@LB1Vol, @Speed_P_L)
	Wait 1000
	RMove Z, -600
	AspirateS(50, @Speed_P_LL)
	RMove Z, 100
	Wait 1000
	AMove Z, @Stage.Z.Safe
	Wait 1000
	AspirateS(10, @Speed_P_M)

'---(Well 0)---
'ClearScreen
	MoveToPos(11)
	HeightMM(5)
	DispenseS(@LB1Vol, @Speed_P_M)
	DispenseS(60, @Speed_P_M)
	
'ClearScreen
'Print 0, 0, "Transfer ProtK"
	MoveToPos(3)
	HeightMM(0)
	RMove Z, 400
	AspirateS(@ProtKVol, @Speed_P_L)
	Wait 1000
	RMove Z, -600
	AspirateS(10, @Speed_P_L)
	RMove Z, 100
	Wait 1000
	AMove Z, @Stage.Z.Safe
	Wait 1000
	AspirateS(10, @Speed_P_M)

'---(Well 0)---
'ClearScreen

	MoveToPos(11)
	HeightMM(1)
	DispenseS(@ProtKVol, @Speed_P_M)
	DispenseS(20, @Speed_P_M)
	
	HeightMM(10)
	Wait 3000
	DispenseS(50, @Speed_P_M)
	Wait 1000

	AMove Z, @Stage.Z.Safe
	AspirateS(50, @Speed_P_M)



'=============<< Temperature setting >>===============
SetTemperature @LysisTemp1
'==============<< Temp waiting >>=================   

'ClearScreen
'Print 0,0, "Heating"
'Print 0,1, ""

*Temp_Wait_Lysis
        GetTemperature @Temp
'	Print 4,2, @Temp
'	Print 6,2, " "
       If @Temp < @LysisTemp1 Then GoTo *Temp_Wait_Lysis


'---(Well 11)---
'ClearScreen
'Print 0, 1, " Lysis Incubation"

	'MixTwoLevels(10,350,@Speed_P_L,@Speed_P_L,100,100) 
	

'	Print 0, 2, " Incub 1 of 12   "
	IncubTime(@LysisTime1)
	HeightMM(1)
	MixTwoLevels(10,350,@Speed_P_L,@Speed_P_L,100,100)
	AMove Z, @Stage.Z.Safe	
	
'	Print 0, 2, " Incub 2 of 12   "
	IncubTime(@LysisTime1)
	HeightMM(1)
	MixTwoLevels(10,350,@Speed_P_L,@Speed_P_L,100,100)
	AMove Z, @Stage.Z.Safe
	
'	Print 0, 2, " Incub 3 of 12   "
	IncubTime(@LysisTime1)
	HeightMM(1)
	MixTwoLevels(10,350,@Speed_P_L,@Speed_P_L,100,100)
	AMove Z, @Stage.Z.Safe	
	
'	Print 0, 2, " Incub 4 of 12   "
	IncubTime(@LysisTime1)
	HeightMM(1)
	MixTwoLevels(10,@SampleVol,@Speed_P_L,@Speed_P_L,100,100)'	
	AMove Z, @Stage.Z.Safe	
	
'	Print 0, 2, " Incub 5 of 12   "
	IncubTime(@LysisTime1)
	HeightMM(1)
	MixTwoLevels(10,@SampleVol,@Speed_P_L,@Speed_P_L,100,100)'
	AMove Z, @Stage.Z.Safe
	
'	Print 0, 2, " Incub 6 of 12   "
	IncubTime(@LysisTime1)
	HeightMM(1)
	MixTwoLevels(10,@SampleVol,@Speed_P_L,@Speed_P_L,100,100)
	
	AMove Z, @Stage.Z.Safe

	'=============<< Temperature setting >>===============
	SetTemperature @LysisTemp2
	'==============<< Temp waiting >>=================   

'	ClearScreen	
'	Print 0,0, "Heating"
'	Print 0,1, ""

	*Temp_Wait_Lysis2
        GetTemperature @Temp
'	Print 4,2, @Temp
'	Print 6,2, " "
        If @Temp < @LysisTemp2 Then GoTo *Temp_Wait_Lysis2

	
'	Print 0, 2, " Incub 7 of 12   "
	IncubTime(@LysisTime1)
	HeightMM(1)
	MixTwoLevels(10,@SampleVol,@Speed_P_L,@Speed_P_L,100,100)
	AMove Z, @Stage.Z.Safe	
	
'	Print 0, 2, " Incub 8 of 12   "
	IncubTime(@LysisTime1)
	HeightMM(1)
	MixTwoLevels(10,@SampleVol,@Speed_P_L,@Speed_P_L,100,100)'
	AMove Z, @Stage.Z.Safe
	
'	Print 0, 2, " Incub 9 of 12   "
	IncubTime(@LysisTime1)
	HeightMM(1)
	MixTwoLevels(10,@SampleVol,@Speed_P_L,@Speed_P_L,100,100)
	AMove Z, @Stage.Z.Safe	
	
'	Print 0, 2, " Incub 10 of 12   "
	IncubTime(@LysisTime1)
	HeightMM(1)
	MixTwoLevels(10,@SampleVol,@Speed_P_L,@Speed_P_L,100,100)
	AMove Z, @Stage.Z.Safe
	
'	Print 0, 2, " Incub 11 of 12   "
	IncubTime(@LysisTime1)
	HeightMM(1)
	MixTwoLevels(10,@SampleVol,@Speed_P_L,@Speed_P_L,100,100)
	AMove Z, @Stage.Z.Safe	
	
'	Print 0, 2, " Incub 12 of 12   "
	IncubTime(@LysisTime1)
	HeightMM(1)
	MixTwoLevels(10,@SampleVol,@Speed_P_L,@Speed_P_L,100,100)	

'Print 0, 1, " Lysis Incub Finish"	
		

'==========<< Transfer Lysis to Beads >>============

SetTemperature @EluteTempDNA

'---(Sample)---
'ClearScreen
'Print 0, 0, "Aspirate Sample"
	MoveToPos(11)
	HeightMM(-1)
	RMove Z, 140
	AspirateS(@SampleVol, @Speed_P_L)
	Wait 1000
	RMove Z, -800
	AspirateS(50, @Speed_P_LL)
	RMove Z, 100
	Wait 1000
	AMove Z, @Stage.Z.Safe
	Wait 1000
	AspirateS(10, @Speed_P_M)

	
'---(Well 1)---
'ClearScreen
'Print 0, 0, "Dispense Sample"
	MoveToPos(3)
	HeightMM(2)
	DispenseS(@SampleVol, @Speed_P_M)
	DispenseS(60, @Speed_P_M)
		

'---(Sample)---
'ClearScreen
'Print 0, 0, "Aspirate LB2"
	MoveToPos(4)
	HeightMM(0)
	RMove Z, 400
	AspirateS(@LB2Vol, @Speed_P_M)
	Wait 1000
	RMove Z, -600
	AspirateS(50, @Speed_P_L)
	RMove Z, 100
	Wait 1000
	AMove Z, @Stage.Z.Safe
	Wait 1000
	AspirateS(10, @Speed_P_M)

'---(Well 1)---
'ClearScreen
'Print 0, 0, "Dispense LB2"
	MoveToPos(3)
	HeightMM(1)
	DispenseS(@LB2Vol, @Speed_P_M)
	DispenseS(60, @Speed_P_M)
	
	HeightMM(1)
	MixTwoLevels(15,@MixTotSampleBuffVol,@Speed_P_H,@Speed_P_HH,100,100)
	AMove Z, @Stage.Z.Safe	
	
'ClearScreen
'Print 0, 0, "Aspirate Sample"
	MoveToPos(3)
	HeightMM(0)
	RMove Z, 400
	AspirateS(@SampleVol, @Speed_P_M)
	AspirateS(@LB2Vol, @Speed_P_M)
	Wait 1000
	RMove Z, -600
	AspirateS(50, @Speed_P_L)
	RMove Z, 100
	Wait 1000
	AMove Z, @Stage.Z.Safe
	Wait 1000
	AspirateS(10, @Speed_P_M)

'---(Well 1)---
'ClearScreen
'Print 0, 0, "Well 1"
*skip_to_binding
	MoveToPos(1)
	HeightMM(1)
	'DispenseS(@SampleVol, @Speed_P_M)
	'DispenseS(@LB2Vol, @Speed_P_M)
	'DispenseS(60, @Speed_P_M)

'==========<< Binding to Beads >>============
ClearScreen
Print 0, 0, " Binding "

'===========<< Beads separation >>==================
	Mix(15,400,@Speed_P_HH,@Speed_P_H,100,100)
	MixTwoLevels(10,@MixTotSampleBuffVol,@Speed_P_H,@Speed_P_H,100,100)
	
'Print 0, 2, " Incub 1 of 3   "
	'IncubTime(@BindTime)
	'MixTwoLevels(10,@MixTotSampleBuffVol,@Speed_P_H,@Speed_P_H,100,100)
	
'Print 0, 2, " Incub 2 of 3   "
	'IncubTime(@BindTime)
	'MixTwoLevels(10,@MixTotSampleBuffVol,@Speed_P_H,@Speed_P_H,100,100)
	
'Print 0, 2, " Incub 3 of 3   "
	'IncubTime(@BindTime)
	'MixTwoLevels(10,@MixTotSampleBuffVol,@Speed_P_H,@Speed_P_H,100,100)
	HeightMM(1)
	

'===========<< Beads separation >>==================

'ClearScreen
'Print 0, 0, "Separation"	
	HeightMM(1)
	AMove M, @Magnet_M
	Mix(15,@MixTotSampleBuffVol,@Speed_P_H,@Speed_P_M,2000,50) 
	AspirateS(@TotSampleBuffVol, @Speed_P_M)
	
	Wait 1000
	Let @Slim_BF, @Proc(1).Z.Process  
		Add @Slim_BF, 5200
		AMove Z, @Slim_BF
		AMove M, @Magnet_M
		AspirateS(100, @Speed_P_L)
		Wait 3000
		DispenseS(@TotSampleBuffVol, @Speed_P_L)
		DispenseS(100, @Speed_P_L)
		Wait 100
	
	Wait 100
	Org M
	AMove Z, @Stage.Z.Safe	

	WashBeads(6, 50, @WashVol2, @Speed_P_H, @Speed_P_H, 50, 100)
	HeightMM(1) 
	
	' Removed entire block of second bead capture as it is not needed.
	 MoveToPos(1)
	 HeightMM(1)
	 MixTwoLevels(20,@MixTotSampleBuffVol,@Speed_P_H,@Speed_P_H,100,100)
	 AMove M, @Magnet_S

	 Mix(15,@MixTotSampleBuffVol,@Speed_P_H,@Speed_P_L,1000,50)
	 AspirateS(@TotSampleBuffVol, @Speed_P_M)
	 'Wait 1000
	 'AspirateS(200, @Speed_P_M)
	 'Wait 1000

	 Wait 1000
	 Let @Slim_BF, @Proc(1).Z.Process  
		 Add @Slim_BF, 5200
		 AMove Z, @Slim_BF
		 AMove M, @Magnet_M
		 AspirateS(100, @Speed_P_L)
		 Wait 3000
		 DispenseS(@TotSampleBuffVol, @Speed_P_L)
		 DispenseS(100, @Speed_P_L)'DispenseS(300, @Speed_P_L)
		 Wait 100

	 WashBeads(6, 15, @WashVol2, @Speed_P_H, @Speed_P_HH, 50, 100)
	 'HeightMM(1) 
	
	AMove Z, @Stage.Z.Safe
	
	
'========<< DNA Wash 1 >>====================
'---(Well 3)---
ClearScreen
Print 0, 0, " Washing 1"
	MoveToPos(6)
	HeightMM(1)
	
	Mix(15,@WashVol2,@Speed_P_H,@Speed_P_H,50,50) 
	MixByTime(@WashTime,@WashVol2,@Speed_P_M,@Speed_P_M,50,50) 
	AMove Z, @Proc(6).Z.Process

'---BF Separation---
'Print 0, 1, "Separation"

	MagSepPos(6, @WashVol2, @Magnet_M)
	

'========<< DNA Wash 2 >>====================
'ClearScreen
'Print 0, 1, " DNA Wash 2"
	MoveToPos(7)
	HeightMM(1)
	
	WashBeads(7, 8, @WashVol2, @Speed_P_H, @Speed_P_HH, 50, 100)
	Mix(15,@WashVol2,@Speed_P_H,@Speed_P_H,100,50)
	MixByTime(@WashTime,@WashVol2,@Speed_P_M,@Speed_P_M,50,50)

	MoveToPos(6)
	HeightMM(1)
	
	Mix(15,@WashVol2,@Speed_P_H,@Speed_P_H,50,50) 
	MixByTime(@WashTime,@WashVol2,@Speed_P_M,@Speed_P_M,50,50) 
	AMove Z, @Proc(6).Z.Process

'---BF Separation---
'Print 0, 1, "Separation"

	MagSepPos(6, @WashVol2, @Magnet_M) 

	MoveToPos(7)
	HeightMM(1)
	
	Mix(15,@WashVol2,@Speed_P_H,@Speed_P_H,100,50)
	MixByTime(@WashTime,@WashVol2,@Speed_P_M,@Speed_P_M,50,50)
	AMove Z, @Proc(7).Z.Process

'---BF Separation---
'ClearScreen
'Print 0, 1, "Separation"

	MagSepPos(7, @WashVol2, @Magnet_M)

'========<< DNA Wash 3 >>====================
'ClearScreen
'Print 0, 1, " DNA Wash 3"
	MoveToPos(8)
	HeightMM(1)
	
	WashBeads(8, 8, @WashVol2, @Speed_P_H, @Speed_P_HH, 50, 100)
	Mix(15,@WashVol2,@Speed_P_H,@Speed_P_H,50,50)
	MixByTime(@WashTime,@WashVol2,@Speed_P_M,@Speed_P_M,50,50)
	MoveToPos(7)
	HeightMM(1)
	
	Mix(15,@WashVol2,@Speed_P_H,@Speed_P_H,50,50) 
	MixByTime(@WashTime,@WashVol2,@Speed_P_M,@Speed_P_M,50,50) 
	AMove Z, @Proc(7).Z.Process

'---BF Separation---
'Print 0, 1, "Separation"

	MagSepPos(7, @WashVol2, @Magnet_M)
	
	MoveToPos(8)
	HeightMM(1)
	
	Mix(15,@WashVol2,@Speed_P_H,@Speed_P_H,50,50)
	MixByTime(@WashTime,@WashVol2,@Speed_P_M,@Speed_P_M,50,50)

	AMove Z, @Proc(8).Z.Process

'---BF Separation---
'ClearScreen
'Print 0, 1, "Separation"

	MagSepPos(8, @WashVol2, @Magnet_M)
	AMove M, @Magnet_M
	DispenseS(50, @Speed_P_H)
	Org M
	MoveToPos(10)
	HeightMM(1)
'==============<< Temp waiting >>=================   
	
'============<< Elution-1 >>====================
'---(Well 10)---
ClearScreen
Print 0, 0, "Elution"
	

	If @E30IsSel Then GoTo *30Elute
	If @E50IsSel Then GoTo *50Elute
	If @E100IsSel Then GoTo *100Elute

'********* 30 Elute *************
*30Elute
	AspirateS(35, @Speed_P_LL)
	AMove Z, @Stage.Z.Safe	

'	ClearScreen
	MoveToPos(12)
	HeightMM(1)
	Mix(5,100,@Speed_P_ML,@Speed_P_H,50,50)
	Mix(5,150,@Speed_P_ML,@Speed_P_H,50,50)
	Mix(10,180,@Speed_P_ML,@Speed_P_H,50,50)
	Mix(5,100,@Speed_P_ML,@Speed_P_H,50,50)
	Mix(5,50,@Speed_P_ML,@Speed_P_H,50,50)	
	DispenseS(35, @Speed_P_M)
	
		GoTo *elutionend1
'********* 50 Elute *************
*50Elute
	AspirateS(60, @Speed_P_LL)
	AMove Z, @Stage.Z.Safe	

'	ClearScreen
	MoveToPos(12)
	HeightMM(1)
	Mix(5,100,@Speed_P_ML,@Speed_P_H,50,50)
	Mix(5,150,@Speed_P_ML,@Speed_P_H,50,50)
	Mix(10,180,@Speed_P_ML,@Speed_P_H,50,50)	
	Mix(5,100,@Speed_P_ML,@Speed_P_H,50,50)
	Mix(5,50,@Speed_P_ML,@Speed_P_H,50,50)	
	DispenseS(60, @Speed_P_M)
	
		GoTo *elutionend1

'********* 100 Elute *************
*100Elute
	AspirateS(110, @Speed_P_LL)
	AMove Z, @Stage.Z.Safe	

'	ClearScreen
	MoveToPos(12)
	HeightMM(1)
	Mix(5,100,@Speed_P_ML,@Speed_P_H,50,50)
	Mix(5,150,@Speed_P_ML,@Speed_P_H,50,50)
	Mix(10,180,@Speed_P_ML,@Speed_P_H,50,50)
	Mix(5,100,@Speed_P_ML,@Speed_P_H,50,50)
	Mix(5,50,@Speed_P_ML,@Speed_P_H,50,50)		
	DispenseS(110, @Speed_P_M)
	
		GoTo *elutionend1

*elutionend1

'	ClearScreen
'	Print 0, 0, "Resuspension"	

	'---(Beads Suspension)---
	Wait 10000
	MoveToPos(8)
	HeightMM(1)
	Mix(15,@WashVol2,@Speed_P_H,@Speed_P_H,50,50)
	MixByTime(@WashTime,@WashVol2,@Speed_P_M,@Speed_P_M,50,50)
	MagSepPos(8, @WashVol2, @Magnet_M)
	AMove M, @Magnet_M
	DispenseS(50, @Speed_P_H)
	Org M
	MoveToPos(12)
	HeightMM(1) 
	Mix(5,100,@Speed_P_ML,@Speed_P_H,50,50)
	Mix(5,150,@Speed_P_ML,@Speed_P_H,50,50)
	Mix(10,180,@Speed_P_ML,@Speed_P_H,50,50)	

	If @E30IsSel Then GoTo *30Elute2
	If @E50IsSel Then GoTo *50Elute2
	If @E100IsSel Then GoTo *100Elute2

	Mix(15,@WashVol,@Speed_P_HH,@Speed_P_HH,50,50)
	HeightMM(3)
	MixByTime(@WashTime,@WashVol,@Speed_P_HH,@Speed_P_HH,50,50)
	
	
	
'********* 30 Elute *************
*30Elute2

'---(Elution)---
'	ClearScreen
'	Print 0, 0, " Elution incub 1   "
	IncubTime(@EluteTimeDNA)
	
'Print 0, 1, " Resuspension "
	Mix(5,180,@Speed_P_ML,@Speed_P_H,50,50)

'	ClearScreen
'	Print 0, 0, " Elution incub 2   "
	IncubTime(@EluteTimeDNA)

'Print 0, 1, " Resuspension "
	Mix(5,180,@Speed_P_ML,@Speed_P_H,50,50)
	
'	ClearScreen
'	Print 0, 0, " Elution incub 3   "
	IncubTime(@EluteTimeDNA)
	
'Print 0, 1, " Resuspension "	
	Mix(5,180,@Speed_P_ML,@Speed_P_H,50,50)
	AMove Z, @Proc(12).Z.Process
	
	GoTo *elutionend2
	
	'********* 50 Elute *************
*50Elute2

'---(Elution)---
'	ClearScreen
'	Print 0, 0, " Elution incub 1   "
	IncubTime(@EluteTimeDNA)
	
'Print 0, 1, " Resuspension "
	Mix(5,180,@Speed_P_ML,@Speed_P_H,50,50)

'	ClearScreen
'	Print 0, 0, " Elution incub 2   "
	IncubTime(@EluteTimeDNA)

'Print 0, 1, " Resuspension "
	Mix(5,180,@Speed_P_ML,@Speed_P_H,50,50)
	
'	ClearScreen
'	Print 0, 0, " Elution incub 3   "
	IncubTime(@EluteTimeDNA)
	
'Print 0, 1, " Resuspension "	
	Mix(5,180,@Speed_P_ML,@Speed_P_H,50,50)
	AMove Z, @Proc(12).Z.Process
	
	GoTo *elutionend2

'********* 100 Elute *************
*100Elute2

'---(Elution)---
'	ClearScreen
'	Print 0, 0, " Elution incub 1   "
	IncubTime(@EluteTimeDNA)

'Print 0, 1, " Resuspension "
	Mix(5,180,@Speed_P_ML,@Speed_P_H,50,50)
	
'	ClearScreen
'	Print 0, 0, " Elution incub 2   "
	IncubTime(@EluteTimeDNA)
	
	
'Print 0, 1, " Resuspension "
	Mix(5,180,@Speed_P_ML,@Speed_P_H,50,50)
	
'	ClearScreen
'	Print 0, 0, " Elution incub 3   "
	IncubTime(@EluteTimeDNA)

'Print 0, 1, " Resuspension "
	Mix(5,180,@Speed_P_ML,@Speed_P_H,50,50)
	AMove Z, @Proc(12).Z.Process
	
	GoTo *elutionend2

*elutionend2
	SetTemperature 250

	'---(BF Separation)---
'	Print 0, 1, " Separation     " 
	AMove M, @Magnet_Thick	
	AspirateS(200, @Speed_P_ML)
	Wait 5000
	DispenseS(200, @Speed_P_L)
	AspirateS(200, @Speed_P_ML)
	Wait 5000
	DispenseS(200, @Speed_P_L)
	AspirateS(200, @Speed_P_ML)
	Wait 5000
	DispenseS(250, @Speed_P_L)
	RMove Z, 6000
	Wait 5000
	DispenseS(100, @Speed_P_L)     
	Org M

	AMove Z, @Stage.Z.Safe

'===========<< Tip Wash >>===============
'---(Well 10)---
'	ClearScreen 
'	Print 0, 0, " Tip wash"

	MoveToPos(10)
	HeightMM(1)	
	Mix(20,500,@Speed_P_H,@Speed_P_XH,50,0)	
	RMove Z, 8000
	DispenseS(100, @Speed_P_L)   
	AMove Z, @Stage.Z.Safe
	AspirateS(100, @Speed_P_M) 
	AspirateS(150, @Speed_P_M) 

'======<< Collection >>=========

'---(Well 12)---
'ClearScreen 
'Print 0, 0, "DNA collection"
	MoveToPos(12)
	HeightMM(0)

RMove Z, -280 'go further down to create vacuum
	Wait 1000
	AspirateSE(200, @Speed_P_ML)


'----------------AspirateSE---------------------------------
'@AspVolume = aspiration volume in ul 
'@AspirateSpeed = asp speed in pulses/sec

'assume tip has gone to the absolute bottom, thus having slight vacuum while Aspirate everything. Therefore move slightly up very slowly
'assume moving to absolute bottom via RMove Z, -400

AspirateSE(@AspVolume, @AspirateSpeed) {

	@Axis.P.Speed = @AspirateSpeed

	Aspirate @AspVolume
	Wait 10
	RMove Z, 40
	Wait 10
	RMove Z, 40
	Wait 10
	RMove Z, 40
	Wait 10
	RMove Z, 40
	Wait 10
	RMove Z, 40
	Wait 10
	RMove Z, 40
	Wait 10
	RMove Z, 40
	Wait 10
}
  
 'Elution 1
'---(Collection Tube)---
	AMove Z, @Stage.Z.Safe 
	MoveToPos(-3) 
	Let @Slim_BF, @Storage.Z.Process
	Add @Slim_BF, 6600
	AMove Z, @Slim_BF

'---(BF Separation)---

	AMove M, @Magnet_H
	Wait 5000
	Let @Axis.P.Speed, @Speed_P_LLL
	
	AspirateS(80, @Speed_P_LLL)
	Wait 5000
	AspirateS(40, @Speed_P_LLL)
	Wait 5000
	DispenseS(300, @Speed_P_LLL)
	DispenseS(120, @Speed_P_L)
	Let @Axis.Y.Speed, 1500
	RMove Y, -150
	RMove Y, 150
	Let @Axis.Y.Speed, 10000
	Org M
	
	
'======<< Return Tip >>=============
'	ClearScreen
'	Print 0, 0, "Return tip" 
	AMove Z, @Stage.Z.Safe
	AMove Y, @Tip(2).Y.Center 
	ReleaseTip 
	AMove Z, @Stage.Z.Safe

'-=======<< ORG ALL >>===============
	ClearScreen
	Org Z
	Org M
	Org Y
	Org P

	SetTemperature 250

	HoodLock Off 'Let @Cover.Locked, 0

'=========<< Finish >>===============
	ClearScreen
	Print 0, 0, "COMPLETED!"

	Beep On 'on
	Beep Off 'off
	Beep On 'on
	Beep Off 'off
	Beep On 'on
	Beep Off 'off
	Beep On 'on
	Beep Off 'off
	Beep On 'on
	Beep Off 'off

*End_Proc




'======<< FUNCTIONS >>=========

'--- move to Z.Process -----------
MoveTo(@num) {
AMove Z, @Stage.Z.Safe
AMove Y, @Proc(@num).Y.Center
AMove Z, @Proc(@num).Z.Process
}

'--- move to 400 steps from Z.Process -------------
Dim @Zpos1
MoveTo1(@num) {
AMove Z, @Stage.Z.Safe
@Zpos1 = @Proc(@num).Z.Process
@Zpos1 = @Zpos1 + 400
AMove Y, @Proc(@num).Y.Center
AMove Z, @Zpos1
}

'-----------------------------------------FUNCTIONS--------------------------------------------------------


'----------------AspirateS---------------------------------
'@AspVolume = aspiration volume in ul 
'@AspirateSpeed = asp speed in pulses/sec

AspirateS(@AspVolume, @AspirateSpeed) {
	@Axis.P.Speed = @AspirateSpeed
	Aspirate @AspVolume
}

'----------------DispenseS---------------------------------
'@DispVolume = Dispense volume in ul 
'@DispenseSpeed = Disp speed in pulses/sec

DispenseS(@DispVolume, @DispenseSpeed) {
	@Axis.P.Speed = @DispenseSpeed
	Dispense @DispVolume
}


' ~~~~~~~~~~~~~~MOVE TO DESIGNATED POSTION
' Sample Tube = 0
' Tip2 Pos = -1
' Tip1 Pos = -2 
' Elution Tube = -3
' Well 1 = 1,......Well 10 = 10



MoveToPos(@Position) {
	AMove M,0  
	AMove Z, @Stage.Z.Safe
	
	If @Position = 0 Then GoTo *SampleTube
	If @Position = -1 Then GoTo *Tip2Pos
	If @Position = -2 Then GoTo *Tip1Pos
	If @Position = -3 Then GoTo *ElutionTube
	
	AMove Y, @Proc(@Position).Y.Center
	GoTo *Finished
	
	*SampleTube
		AMove Y, @Sample.Y.Center
		GoTo *Finished
	*Tip2Pos
		AMove Y, @Tip(2).Y.Center 
		GoTo *Finished
	*Tip1Pos
		AMove Y, @Tip(1).Y.Center 
		GoTo *Finished
	*ElutionTube
		AMove Y, @Storage.Y.Center
	*Finished
	@Pos = @Position
}

' ~~~~~~~~~~~~~~move to mm from Z.Process
'move tip to a height relative to Z.Process in mm. 
'To use the tip must be in the same well as the prior MoveTo() function use.
'This function uses @Pos

HeightMM(@mm) {
	Dim @heightTo
	Dim @Steps2Use
	Let @Steps2Use, @mm
	@Steps2Use = @Steps2Use * 400	'convert mm to pulses.  Z axis = 400pulses/mm
	@heightTo = @Steps2Use
	
	If @Pos = 0 Then GoTo *sampleTube
	If @Pos = -1 Then GoTo *elutionTube
		
	@heightTo = @heightTo + @Proc(@Pos).Z.Process
'@heightTo = @heightTo - 18000
	
	GoTo *AdjustHeight
	
	*sampleTube
		@heightTo = @heightTo + @Sample.Z.Process
		GoTo *AdjustHeight
	*elutionTube
		@heightTo = @heightTo + @Storage.Z.Process	
	*AdjustHeight
	
	AMove Z, @heightTo
}


' ~~~~~~~~~~~~~~move to mm from Z.Process
'now function in debugger
MoveToHeight(@Position, @mm) {
	Dim @heightTo
	Dim @Steps2Use
	Let @Steps2Use, @mm
	@Steps2Use = @Steps2Use * 400	'convert mm to pulses.  Z axis = 400pulses/mm
	@heightTo = @Steps2Use

	@Pos = @Position
	
	If @Position = 0 Then GoTo *sampleTube
	If @Position = -1 Then GoTo *elutionTube 'If @Position = -3 Then GoTo *elutionTube
		
	@heightTo = @heightTo + @Proc(@Position).Z.Process
	
	GoTo *AdjustHeight
	
	*sampleTube
		@heightTo = @heightTo + @Sample.Z.Process
		GoTo *AdjustHeight
	*elutionTube
		@heightTo = @heightTo + @Storage.Z.Process	
	*AdjustHeight
	
	AMove Z, @heightTo
}

' ~~~~~~~~~~~~~~Mix at current position/height
'@MixCycles = number of mix cycles
'@MixVol = volume to mix in ul.
'@AspSpeed = aspiration speed in pps.
'@DispSpeed = dispense speed in pps.
'@AspWait = wait time after aspiration in msec.
'@DispWait = wait time after dispense in msec.

Mix(@MixCycles, @MixVol, @AspSpeed, @DispSpeed, @AspWait, @DispWait) {

@Count = 0
'     Print 16, 3, "/"
'     Print 17, 3, @MixCycles
*MixStart
	@Count = @Count + 1
'	Print 13, 3, @Count
	@Axis.P.Speed = @AspSpeed
	Aspirate @MixVol
	Wait @AspWait
	@Axis.P.Speed = @DispSpeed
	Dispense @MixVol
	Wait @DispWait
	If @Count < @MixCycles Then GoTo *MixStart
'	Print 0, 3, "                    "
}

' ~~~~~~~~~~~~~~Mix at current position/height with percentage of Volume
'@MixCycles = number of mix cycles
'@TotVol = volume in well in ul.
'@Percentage = Percentage of Total Volume to mix
'@AspSpeed = aspiration speed in pps.
'@DispSpeed = dispense speed in pps.
'@AspWait = wait time after aspiration in msec.
'@DispWait = wait time after dispense in msec.

MixVol(@MixCycles, @TotVol, @Percentage, @AspSpeed, @DispSpeed, @AspWait, @DispWait) {
Dim @MixVol
@MixVol = @TotVol * @Percentage
@MixVol = @MixVol / 100

@Count = 0
'     Print 16, 3, "/"
'     Print 17, 3, @MixCycles

*MixStart
	@Count = @Count + 1
'	Print 13, 3, @Count
	@Axis.P.Speed = @AspSpeed
	Aspirate @MixVol
	Wait @AspWait
	@Axis.P.Speed = @DispSpeed
	Dispense @MixVol
	Wait @DispWait
	If @Count < @MixCycles Then GoTo *MixStart
'	Print 0, 3, "                    "
}

' ~~~~~~~~~~~~~~Mix at current position/height with percentage of Volume
'@MixTime = Mix time in seconds.
'@TotVol = volume in well in ul.
'@Percentage = Percentage of Total Volume to mix
'@AspSpeed = aspiration speed in pps.
'@DispSpeed = dispense speed in pps.
'@AspWait = wait time after aspiration in msec.
'@DispWait = wait time after dispense in msec.

MixByTimeVol(@MixTime, @TotVol, @Percentage, @AspSpeed, @DispSpeed, @AspWait, @DispWait) {
Dim @EndTime
Dim @MixVol
@MixVol = @TotVol * @Percentage
@MixVol = @MixVol / 100

@Clock.Value = 0
@EndTime = @Clock.Value + @MixTime

'Print 11, 3, "/    sec"
'Print 13, 3, @MixTime

*MixStart
'	Print 7, 3, @Clock.Value
	@Axis.P.Speed = @AspSpeed
	Aspirate @MixVol
'	Print 7, 3, @Clock.Value
	Wait @AspWait
'	Print 7, 3, @Clock.Value
	@Axis.P.Speed = @DispSpeed
	Dispense @MixVol
'	Print 7, 3, @Clock.Value
	Wait @DispWait
'	Print 7, 3, @Clock.Value
	If @Clock.Value < @EndTime Then GoTo *MixStart
'	Print 0, 3, "                     "
}


' ~~~~~~~~~~~~~~Mix at different position/height with percentage of Volume
'@MixCycles = number of mix cycles
'@TotVol = volume in well in ul.
'@Percentage = Percentage of Total Volume to mix
'@AspSpeed = aspiration speed in pps.
'@DispSpeed = dispense speed in pps.
'@AspWait = wait time after aspiration in msec.
'@DispWait = wait time after dispense in msec.

MixVolTwoLevels(@MixCycles, @TotVol, @Percentage, @AspSpeed, @DispSpeed, @AspWait, @DispWait) {
Dim @MixVol
@MixVol = @TotVol * @Percentage
@MixVol = @MixVol / 100

@Count = 0
'     Print 16, 3, "/"
 '    Print 17, 3, @MixCycles

*MixStart
	@Count = @Count + 1
'	Print 13, 3, @Count
	@Axis.P.Speed = @AspSpeed
	Aspirate @MixVol
	Wait @AspWait
	@Axis.Z.Speed = 8000
	RMove Z, 2000
	@Axis.P.Speed = @DispSpeed
	Dispense @MixVol
	@Axis.Z.Speed = 8000
	RMove Z, -2000
	Wait @DispWait
	If @Count < @MixCycles Then GoTo *MixStart
'	Print 0, 3, "                    "
}


'~~~~~~~~~~~~~~Mix By Time at current position/height
'@MixTime = Mix time in seconds.
'@MixVol = volume to mix in ul.
'@AspSpeed = aspiration speed in pps.
'@DispSpeed = dispense speed in pps.
'@AspWait = wait time after aspiration in msec.
'@DispWait = wait time after dispense in msec.

MixByTime(@MixTime, @MixVol, @AspSpeed, @DispSpeed, @AspWait, @DispWait) {
Dim @EndTime
@Clock.Value = 0
@EndTime = @Clock.Value + @MixTime

'Print 11, 3, "/    sec"
'Print 13, 3, @MixTime

*MixStart
'	Print 7, 3, @Clock.Value
	@Axis.P.Speed = @AspSpeed
	Aspirate @MixVol
'	Print 7, 3, @Clock.Value
	Wait @AspWait
'	Print 7, 3, @Clock.Value
	@Axis.P.Speed = @DispSpeed
	Dispense @MixVol
'	Print 7, 3, @Clock.Value
	Wait @DispWait
'	Print 7, 3, @Clock.Value
	If @Clock.Value < @EndTime Then GoTo *MixStart
'	Print 0, 3, "                     "
}

' ~~~~~~~~~~~~~~Mix at different position/height
'@MixCycles = number of mix cycles
'@MixVol = volume to mix in ul.
'@AspSpeed = aspiration speed in pps.
'@DispSpeed = dispense speed in pps.
'@AspWait = wait time after aspiration in msec.
'@DispWait = wait time after dispense in msec.
'@HeightDiff = Height difference to move in mm
'@MoveSpeed = the speed of movement in Z

MixTwoLevels(@MixCycles, @MixVol, @AspSpeed, @DispSpeed, @AspWait, @DispWait) {

@Count = 0
 '    Print 16, 3, "/"
  '   Print 17, 3, @MixCycles

*MixStart
	@Count = @Count + 1
'	Print 13, 3, @Count
	@Axis.P.Speed = @AspSpeed
	Aspirate @MixVol
	Wait @AspWait
	@Axis.Z.Speed = 8000
	'@Axis.Z.Speed = @MoveSpeed
	RMove Z, 2000
	@Axis.P.Speed = @DispSpeed
	Dispense @MixVol
	@Axis.Z.Speed = 8000
	RMove Z, -2000
	Wait @DispWait
	If @Count < @MixCycles Then GoTo *MixStart
'	Print 0, 3, "                  "
}

' ~~~~~~~~~~~~~~Resusp Beads
ResuspBeads(@Position, @Vol) {
	MoveTo(@Position)			
	HeightMM(2)	
	Mix(25,@Vol,5000,8000,500,500) 'Mix(@MixCycles, @MixVol, @AspSpeed, @DispSpeed, @AspWait, @DispWait)
	Wait 1000
	AspirateS(@Vol, 2500)	
	Wait 1000
	DispenseS(@Vol, 2500)
	AMove Z, @Stage.Z.Safe	
	'AMove P, @P_StartPos	
	'Wait 3000
	'DispenseS(@Vol, 2500)
	'Wait 500
	'AspirateS(@Vol, 2500)
	}

' ~~~~~~~~~~~~~~WashingBeads
WashBeads(@Position, @MixCycles, @MixVol, @AspSpeed, @DispSpeed, @AspWait, @DispWait) {
	MoveToPos(@Position)			
	HeightMM(2)
	Mix(@MixCycles,@MixVol,@AspSpeed,@DispSpeed,@AspWait,@DispWait) 'Mix(20,5000,2500,8000,500,500)
	AMove M, @Magnet_M
	@Axis.Z.Speed = 300
	AMove Z, @Stage.Z.Safe
	Org M
	@Axis.Z.Speed = 8000
	Wait 5000
	HeightMM(2)
	Mix(@MixCycles,@MixVol,@AspSpeed,@DispSpeed,@AspWait,@DispWait) 'Mix(20,5000,2500,8000,500,500)
	AspirateS(200, 2500)	
	Wait 1000
	DispenseS(200, 2500)
	Wait 1000
	DispenseS(50, 2500)
	AMove Z, @Stage.Z.Safe	
	'AMove P, @P_StartPos	
	'Wait 3000
	'DispenseS(200, 2500)
	'Wait 500
	AspirateS(50, 2500)
	}

' ~~~~~~~~~~~~~~MagSepBeads
MagSepPos(@Position, @MagSepVol, @MagStrength) {
	AMove Z, @Proc(@Position).Z.Process  
	AspirateS(@MagSepVol, @Speed_P_M)
	Let @Slim_BF, @Proc(@Position).Z.Process  
	Add @Slim_BF, 5200
	AMove Z, @Slim_BF
	AMove M, @MagStrength
	AspirateS(100, @Speed_P_L)
	Wait 3000
	DispenseS(@MagSepVol, @Speed_P_L)
	DispenseS(100, @Speed_P_L)
	Wait 1000
	
	RMove M, -3000
	AMove Z, @Proc(@Position).Z.Process
	AspirateS(@MagSepVol, @Speed_P_M)
	Let @Slim_BF, @Proc(@Position).Z.Process  
	Add @Slim_BF, 5200
	Wait 500
	AMove Z, @Slim_BF
	AMove M, @MagStrength
	Wait 3000
	AspirateS(100, @Speed_P_L)
	Wait 3000
	DispenseS(@MagSepVol, @Speed_P_L)
	DispenseS(100, @Speed_P_H)
	Wait 1000
	Org M
	AMove Z, @Stage.Z.Safe
}
	
'--- Tip Wash before elution ----
TipWash(@Position) {
'	ClearScreen 
'	Print 0, 0, "Well @Position"
'	Print 0, 1, " Beads washing"
	AMove Y, @Proc(@Position).Y.Center
	AMove Z, @Proc(@Position).Z.Process

	RMove Z, 3000
	AMove M, @Magnet_M
	AspirateS(800, @Speed_P_L)
	RMove Z, 2500
	Wait 1000
	DispenseS(400, @Speed_P_L)
	Wait 1000
	DispenseS(450, @Speed_P_L)
	Wait 200
	RMove Z, 2500
	DispenseS(50, @Speed_P_L)
	Wait 1000
	DispenseS(100, @Speed_P_HH)
	Org M
	AMove Z, @Stage.Z.Safe
	AspirateS(200, @Speed_P_HH)
}
'-----Calc msec to sec-----
MyWait(@WaitTime) {
@WaitTime = @WaitTime * 1000

Wait @WaitTime
}

'~~~~~~~~~~~~~~IncubationTime
'@Time2Incub = Time in seconds.

IncubTime(@Time2Incub) {
Dim @EndTime
@Clock.Value = 0
@EndTime = @Clock.Value + @Time2Incub

'Print 11, 3, "/    sec"
'Print 13, 3, @Time2Incub

*TimeStart
'	Print 7, 3, @Clock.Value
	Wait 1000
	If @Clock.Value < @EndTime Then GoTo *TimeStart
'	Print 0, 3, "                     "
}
