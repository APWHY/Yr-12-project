Width = 640
Height = 460

Graphics Width, Height, 24, 2
SetBuffer BackBuffer()
ClsColor 0,220,255

;ints
running  = 1
x = 230	  ;xy co-ords integer
y = 230
mex# = 230 ;xy co-ords decimal
mey#= 0
mevx# = 0  ;speed in xy direction
mevy#= 0
meax# = 0  ;Acceleration in xy direction
meay# = 0
Friction# = 0.25
dir = 1 ;1 is right 0 is left, right is default to start with.

;frame sprite shenanigans

Dim frame(1) ;frame for sprite to display
frame(1) = 0
frame(0) = 0
Dim deciframe#(1)  ;decimal value so that we can get slower fps for sprite
deciframe(1) = 0
deciframe(0) = 0
Dim walk#(1)
walk(1) = 0.25
walk(0) = 0.5
isrun = 1 ;flag to be used on walk to toggle between running and walking. 0 is walk, 1 is run.


;frame stuff
fps = 50
fpstime = 0




;images
Bkground = LoadImage("Level1ground.bmp")
MaskImage Bkground,255,0,0
groundwall = LoadImage("groundwall.bmp")
Dim mariobig(1)

mariobig(1) = LoadAnimImage("mariobig.bmp",32,64,0,7)
mariobig(0) = LoadAnimImage("mariobigleft.bmp",32,64,0,7)


MidHandle mariobig(1)
MidHandle mariobig(0)

;debugging things
Color 0,0,0


While running = 1
	;fps calculations
	fpstime = MilliSecs()
	
	
	
	;UPDATE
	If KeyDown(30) ;That's A, by the way
		isrun = 1
	Else
		isrun = 0
	EndIf
	
	
	;Up = 200, Left = 203, Right = 205, Down = 208 
	If KeyDown(203) = True ;Left
		meax = 6*walk(isrun) 																				;Turns on an exact acceleration constant (negative for other direction)
		deciframe(0) = deciframe(0) + walk(isrun)															;Then adds a certain number onto the deciframe counter to advance sprite animation
		If deciframe(0) >= 2.5 Then deciframe(0) = 0.00														;Does the mod calculation since mod is being annoying to me at the moment
		frame(0) = 4 + Int(deciframe(0)) 																	;rounds the current frame number off so we can show a valid frame number								
		dir = 0																								;Gives us our direction flag
																											;Thank the gods that I managed to optimize my movement code, or this would be 3 pages long
	EndIf
	If KeyDown(205) = True ;Right
	
		meax = -6*walk(isrun)
		deciframe(1) = deciframe(1) - walk(isrun)
		If deciframe(1) <= 0 Then deciframe(1) = 2.49
		frame(1) = Int(deciframe(1)) ;rounds the current frame number off
		dir = 1
	EndIf
	If KeyDown(200) = True ;Up
	
		meay = 5
		mevy = 0
	EndIf
	If KeyDown(208) = True ;Down
		;meay = -0.2
		;Lol, do nutin unless standing on pipe, but we'll figure that later
	EndIf
	If KeyDown(203) = False And KeyDown(205) = False
		meax = 0
		frame(1) = 5
		frame(0) = 1
	ElseIf meax/Abs(meax) = -mevx/Abs(mevx) ;so if velocity is different to direction being held down AND here is an acceleration (so a key is being held down)
		frame(1) = 6
		frame(0) = 0
		
	EndIf 
	If KeyDown(200) = False And KeyDown(208) = False
		meay = 0
	EndIf 
	
	
	mevx = mevx + meax
	mevy = mevy + meay
	If mevx^2 => 2*(isrun+1)* 25 Then mevx = (isrun+1)*  5*(mevx/Abs(mevx)) ;limit speed. The '2*(isrun+1)*' basically doubles the speed if running and keeps it normal otherwise
	If mevy^2 => 49 Then mevy = 7*(mevy/Abs(mevy))
	If mevx <> 0
		mevx = (mevx/Abs(mevx))*(Abs(mevx) - Friction)
		If Abs(mevx) < 0.05 ;to stop random decimal shenanigans from happening, otherwise you get speeds of 0.0000000000000000000000000012313413 or something due binary-decimal conversion
			mevx = 0
		EndIf 
	EndIf
	If mey <> 0 ;NOTE IT'S MEY NOT MEVY BECAUSE GRAVITY
		mevy = mevy - Friction
	EndIf

		
	mex = mex + mevx
	mey = mey + mevy
	x = Int(mex)
	y = Int(mey)
	
	
	;Mario sprite frame calculations
	

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	;Text 0,0,meax+" - " +meay+" - "+mevx+" - "+mevy+" - "+mex+" - "+mey
	Text 0,0, dir +" - " + frame(1) +" - " + deciframe(1) +" - " +frame(0)+" - " +deciframe(0)
	
	
	
	If KeyDown(28)
	running = 0
	EndIf
	
	
	
	
	
	
	
	;RENDER
	
	
	
	DrawImage BkGround,x-230,y
	DrawImage mariobig(dir), 230, y+180,frame(dir) 
	
	
	
	While MilliSecs() < fpstime + (1000/fps)
	Wend
	
	Flip
Cls

Wend



Print "Wait!"




WaitKey()
End
