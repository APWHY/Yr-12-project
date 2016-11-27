Width = 640
Height = 460

Graphics Width, Height, 24, 2
SetBuffer BackBuffer()
ClsColor 0,220,255

;ints for mario and background
running  = 1 ;I WONDER WHAT THIS DOES
Imgsety = 0 ;sets y-coordinate for background. Yes, it does change.
Imgsetx = 230 ;sets x-offset for background. It won't change during program, but I'm going to be mucking around with this
x = 230	  ;xy co-ords integer
y = 230
mex# = 230 ;xy co-ords decimal
mey#= 0
mevx# = 0  ;speed in xy direction
mevy#= 0
meax# = 0  ;Acceleration in xy direction
meay# = -3
Friction# = 0.25
dir = 1 ;1 is right 0 is left, right is default to start with.
onground = 0
jumpcounter = 0 ;Allows for shorthops and longhops
upheld = 0 ;flag that tells us if up was held
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

Dim mariosmall(1)

mariosmall(1) = LoadAnimImage("mariosmall.bmp",32,32,0,7)
mariosmall(0) = LoadAnimImage("mariosmallleft.bmp",32,32,0,7)

MidHandle mariosmall(1)
MidHandle mariosmall(0)
MidHandle mariobig(1)
MidHandle mariobig(0)

Dim mario(1)
mario(1) = mariobig(1)
mario(0) = mariobig(0)

;debugging things
Color 0,0,0


While running = 1
	;fps calculations
	fpstime = MilliSecs()
	
	
	
	;UPDATE
	;FOR MARIO
	;Recieve events and set variables for later calculations
	
	
	
	If KeyDown(30) ;That's A, by the way
		isrun = 1
	Else
		isrun = 0
	EndIf
	If KeyDown(31)
		changemario(1)
	Else

		changemario(2)
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
	
	
	meay = 2 ;gravity's constant, apparently.
	
	If KeyDown(200) = True ;Up
		If jumpcounter > 0

			meay = -(jumpcounter)
			jumpcounter = jumpcounter - 1
			upheld = 1
		EndIf
		If onground = 1
			meay = -15
			jumpcounter = 3
			upheld = 1
			mevy = 0
			onground = 0
		EndIf

		If upheld = 0
			jumpcounter = 0
			meay = 2
		EndIf	
		upheld = 0
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

	EndIf 
	If onground = 0
		frame(1) = 3
		frame(0) = 3
	EndIf
	;Calculations go here
	
	
	mevx = mevx + meax
	mevy = mevy + meay
	If mevx^2 => 2*(isrun+1)* 25 Then mevx = (isrun+1)*  5*(mevx/Abs(mevx)) ;limit speed. The '2*(isrun+1)*' basically doubles the speed if running and keeps it normal otherwise
	If mevy^2 => 4000 Then mevy = 20*(mevy/Abs(mevy))
	If mevy > 20 Then mevy = 20
	If mevy < -30 Then mevy = -30 
	If mevx <> 0
		mevx = (mevx/Abs(mevx))*(Abs(mevx) - Friction)
		If Abs(mevx) < 0.05 ;to stop random decimal shenanigans from happening, otherwise you get speeds of 0.0000000000000000000000000012313413 or something due binary-decimal conversion
			mevx = 0
		EndIf 
	EndIf

	

		
	;mex = mex + mevx
	;mey = mey + mevy
	;x = Int(mex)
	;y = Int(mey)
	
	If ImageRectCollide(groundwall, Int(mex+mevx)-Imgsetx, Imgsety, 0, 230-(ImageWidth(mario(dir))/2), y-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)) ) = True ;This moves the background along the x axis and checks for collision before finishing calculations
		While ImageRectCollide(groundwall, Int(mex-(mevx/Abs(mevx)))-Imgsetx, Imgsety, 0, 230-(ImageWidth(mario(dir))/2), y-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)) ) = True
			mex = mex - (mevx/Abs(mevx))
		Wend
		
		
		mevx = 0 ;If there is one, the calculation isn't made and the player stops moving sideways
		meax = 0

	Else
		mex = mex + mevx ;Otherwise, there is no collision and we are free to change the x-co-ord as we want.
		
	EndIf
	x = Int(mex)
	If ImageRectCollide(groundwall,x-Imgsetx, Imgsety,0,230-(ImageWidth(mario(dir))/2),Int(mey+mevy)-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)) ) = True ;This now tests for vertical movement and collision. Doing so separately so that diagonal movement doesn't screw with pixel priority.
	;Damn, even having written the previous line, it still sounds like gibberish. Basically, they're separate so that if mario jumps into a corner the program doesn't die. I haven't tried doing them together, but I don't want to risk it


		While ImageRectCollide(groundwall,x-Imgsetx, Imgsety,0,230-(ImageWidth(mario(dir))/2),Int(mey+(mevy/Abs(mevy)))-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)) ) = False ;This is to stop mario landing, but leaving a small gap between his feet and the ground because that's the gap he WOULD have moved had he not been stopped. Please work.

			mey = mey + (mevy/Abs(mevy))
		Wend
		If mevy > 0
			onground = 1
		EndIf
		meay = 0
		jumpcount = 0
		mevy = 0
		
		;mevy = 0 ;MUST BE AFTER THE WHILE LOOP
		;note meay isn't reset to 0 because gravity is a constant. So if I walk off a platform, I'm still going to fall off. 
		 ;this is going to tell us whether jumping and ducking is allowed.
	
	Else
		mey = mey + mevy ;Again, if there is no collision, DO WHAT YOU WANT. YEAH.
		onground = 0
		
	EndIf
	y = Int(mey) ;and now the code has to break. I just wrote too much for it not to break


	

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	Text 0,20, onground +" - " + jumpcounter
	Text 0,0,meax+" - " +meay+" - "+mevx+" - "+mevy+" - "+mex+" - "+mey
	;Text 0,0, dir +" - " + frame(1) +" - " + deciframe(1) +" - " +frame(0)+" - " +deciframe(0)
	
	
	
	If KeyDown(28)
	running = 0
	EndIf
	
	
	
	
	
	
	
	;RENDER
	
	
	
	DrawImage Bkground,x-Imgsetx,Imgsety
	DrawImage mario(dir), 230, y,frame(dir) 
	
	
	
	While MilliSecs() < fpstime + (1000/fps)
	Wend
	
	Flip
Cls

Wend



Print "Wait!"




WaitKey()
End

Function changemario(level)
Select level
	Case 1
		mario(1) = mariosmall(1)
		mario(0) = mariosmall(0)
	Case 2
		mario(1) = mariobig(1)
		mario(0) = mariobig(0)
	Case 3
	Default
End Function