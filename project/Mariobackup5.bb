Width = 640
Height = 460

Graphics Width, Height, 24, 2
SetBuffer BackBuffer()
ClsColor 0,220,255
SeedRnd MilliSecs()
;counters!
randomcounter = 0
failcounter = 0
counter = 0

;ints for mario and background. In hindsight, it might have been possible to put this in a STRUCT, but it's too tricky, because not all variables directly apply to mario
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

;death stuff for mario
dead = 0; 0 = alive, 1= lose powerups or death by enemy, 2 = finishing death by enemy, 3 = death by falling, 4 = show respawn screen
deathpoint = 0 ;Allows to give mario his death animation
lives = 3;
level = 0; 1 = small mario, 2 = big mario, 3 = FIREBALLMARIO




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
changemario(2)

;blocks
Type ablock
	Field x% ;x-pos of block
	Field y% ;y-pos of block
	Field what%; what type of block is it? 0 = normal, 1 = coin, 2 = coin but looks like a brick, 3 = mushroom/flower, 4 = 1up, 5 = invincibility, 6 = static
	Field visible%;can you see the block? Only here because there is only one invisible block.
	Field frame%;blocks will use frames, so what frame will we be on?
	Field deciframe#
	Field sprite;this will hold the sprite file.
	Field myhandle;just in case I need to pull out handle and object commands, but I doubt it. Otherwise, it won't do anything
	Field movementcounter ;allows me to make the blocks bounce when headbutted.
	Field timeshit;awkward variable name...it's stands for 'Times Hit' and it's for that idiota multihit questionmark box
End Type
tosplit$ = ""

allblocks = ReadFile("blocks.txt")



While Not Eof(allblocks)
tosplit =  ReadLine$(allblocks)
block.ablock = New ablock
;Splitme(tosplit, block\x, block\y, block\what, block\visible) this is what I miss from c++, the ability to do stuff like this line.
block\x = Mid(tosplit,1,4) ;I've stored the locations and types of all interactive objects (so brick and '?' blocks) in a notepad in a 10-digit format. First four give x-pos, then next four give y-pos, then 9th gives type and 10th gives visible or not.
block\y = Mid(tosplit,5,4)
block\what = Mid(tosplit,9)
block\visible = Mid(tosplit, 10)

block\sprite = LoadAnimImage("blocks.bmp", 32,32,0,7) ;012 are animated '?' blocks, 3 is brick block, 4 is hit '?' block
MaskImage block\sprite, 255,0,255
block\timeshit = 1


If block\what = 0 Or block\what = 2
	block\frame = 5
	
	If block\what = 2 Then block\timeshit = 12
	
Else
	
	block\frame = 0
	
	block\deciframe = 0.00
EndIf

Wend
CloseFile(allblocks)



;block fragments
Type ablockfragment
	Field x%
	Field y%
	Field vx%
	Field vy%
	Field ay%
	Field image[179]; the fragments actually rotate, can you believe that ?!?!?!?!?!?!?!?!?!?!?!
	Field degree
End Type

Dim fragment(179)

TFormFilter 1
For randomcounter = 0 To 179
fragment(randomcounter) = LoadImage("blockfragment.bmp")
RotateImage fragment(randomcounter), 2*(randomcounter)
Next
TFormFilter 0








;debugging things
Color 0,0,0


While running = 1
	;fps calculations
	fpstime = MilliSecs()
	
	
	
	;UPDATE
	;FOR MARIO
	;Recieve events and set variables for later calculations
	Select Dead
		Case 0 
			Gosub updatemario
			Gosub updateblocks	
			Gosub testmariocollision
			Gosub drawgame
		Case 1 ;This is only excuted once when mario dies

			;If level = 1 <---- this works but only if you consider the case where he's got a powerup, so leave this in blue for now.
						Gosub deathbygoomba
			;EndIf

		Case 2
			meax = 0
			meay = 1.5
			mevy = mevy + meay
			mevx = mevx + meax ;standard physics calcs
			mex = mevx + mex
			mey = mey + mevy
			If mey > 600 ;leads us to lives screen
				Dead = 4
			EndIf 
			x = Int(mex)
			y = Int(mey)
			frame(1) = 4
			frame(0) = 2
			
			Gosub drawgamestatic ;I WONDER WHAT THIS DOES. Code different because the stage isn't moving
		Case 3
		
			Delay(1000)
			;play music during delay ofc
			dead = 4

		Case 4
			mevy = 0
			mevx = 0
			mey = 0
			mex = 230
			staydead = 0
			RuntimeError"That's all, folks!"		
		Default	
	End Select






	Gosub debug
	
	
	If KeyDown(28)
	running = 0
	EndIf
	
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
End Select
End Function




.updatemario
	;UPDATE
	;FOR MARIO
	;Recieve events and set variables for later calculations
If KeyDown(30) ;That's A, by the way
		isrun = 1
	Else
		isrun = 0
EndIf
If KeyDown(31) ;That's S
		changemario(1)
		level = 1
	Else

		changemario(2) ;BE CAREFUL USING THIS FUNCTION. MUST REMEMBER TO ADD THIS AFTERWARDS: Mey = mey - 16
		level = 2
EndIf
If KeyDown(32)
	Dead = 1
EndIf
	
	
	;Up = 200, Left = 203, Right = 205, Down = 208 
	If KeyDown(203) = True ;Left
		meax = 6*walk(isrun) 																				;Turns on an exact acceleration constant (negative for other direction)
		deciframe(0) = deciframe(0) + walk(Abs(1-isrun))															;Then adds a certain number onto the deciframe counter to advance sprite animation
		If deciframe(0) >= 2.5 Then deciframe(0) = 0.00														;Does the mod calculation since mod is being annoying to me at the moment
		frame(0) = 4 + Int(deciframe(0)) 																	;rounds the current frame number off so we can show a valid frame number								
		dir = 0																								;Gives us our direction flag
																											;Thank the gods that I managed to optimize my movement code, or this would be 3 pages long
	EndIf
	If KeyDown(205) = True ;Right
	
		meax = -6*walk(isrun)
		deciframe(1) = deciframe(1) - walk(Abs(1-isrun))
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
Return
.updateblocks
For block.ablock = Each ablock
	;block\x = block\x + mevx ;consider if mevx is less than zero for when we get to the point that the stage doesn't move when mario goes left.
	If block\frame < 5
		block\deciframe = (block\deciframe + 0.25)
		If block\deciframe >= 4.5 Then block\deciframe = -0.49
		block\frame = Int(block\deciframe)
	EndIf
Next



Return

.testmariocollision
	;mex = mex + mevx
	;mey = mey + mevy
	;x = Int(mex)
	;y = Int(mey)
	
	If ImageRectCollide(groundwall, Int(mex+mevx)-Imgsetx, Imgsety, 0, 230-(ImageWidth(mario(dir))/2), y-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)) ) = True ;This moves the background along the x axis and checks for collision before finishing calculations
		While ImageRectCollide(groundwall, Int(mex+(mevx/Abs(mevx)))-Imgsetx, Imgsety, 0, 230-(ImageWidth(mario(dir))/2), y-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)) ) = False
			mex = mex + (mevx/Abs(mevx)) ;makes sure Mario is as close as his sprite range will allow him to be next to the wall.
		Wend
		
		
		mevx = 0 ;If there is one, the calculation isn't made and the player stops moving sideways
		meax = 0

	Else
		For block.ablock = Each ablock ;tests for sideways collision with any blocks.
			If RectsOverlap(230-(ImageWidth(mario(dir))/2),y-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)), block\x+Int(mex+mevx)-Imgsetx, block\y,32,32) = True
				While RectsOverlap(230-(ImageWidth(mario(dir))/2),y-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)), block\x+(mex + (mevx/Abs(mevx)))-Imgsetx, block\y,32,32) = False
					mex = mex + (mevx/Abs(mevx))
				Wend
				
				mevx = 0
				meax = 0
			EndIf
		Next
		;collision checks must all be done before mex is changed, or mey
		mex = mex + mevx ;Otherwise, there is no collision and we are free to change the x-co-ord as we want.
		
	EndIf
	x = Int(mex)
	If ImageRectCollide(groundwall,x-Imgsetx, Imgsety,0,230-(ImageWidth(mario(dir))/2),Int(mey+mevy)-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)) ) = True ;This now tests for vertical movement and collision. Doing so separately so that diagonal movement doesn't screw with pixel priority.
	;Damn, even having written the previous line, it still sounds like gibberish. Basically, they're separate so that if mario jumps into a corner the program doesn't die. I haven't tried doing them together, but I don't want to risk it


		While ImageRectCollide(groundwall,x-Imgsetx, Imgsety,0,230-(ImageWidth(mario(dir))/2),Int(mey+(mevy/Abs(mevy)))-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)) ) = False ;This is to stop mario landing, but leaving a small gap between his feet and the ground because that's the gap he WOULD have moved had he not been stopped. Please work.

			mey = mey + (mevy/Abs(mevy))
		Wend

		If mevy => 0
			onground = 1
		EndIf
		meay = 0
		jumpcount = 0
		mevy = 0
		
		;mevy = 0 ;MUST BE AFTER THE WHILE LOOP
		;note meay isn't reset to 0 because gravity is a constant. So if I walk off a platform, I'm still going to fall off. 
		 ;this is going to tell us whether jumping and ducking is allowed.
	
	Else
					onground = 0
		For block.ablock = Each ablock ;tests for VERTICAL collision with any blocks.
			If RectsOverlap(230-(ImageWidth(mario(dir))/2),Int(mey+mevy)-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)), block\x+x-Imgsetx, block\y,32,32) = True
				While RectsOverlap(230-(ImageWidth(mario(dir))/2),Int(mey + (mevy/Abs(mevy)))-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)), block\x+x-Imgsetx, block\y,32,32) = False ;makes sure feet touch the ground
					mey = mey + (mevy/Abs(mevy))
				Wend
				
				If mevy >= 0 ;If he is travelling downwards and hits from above
					onground = 1
					
				Else ;If he hits from below....TIME TO LOAD SOME SPRITES.
					onground = 0 ;redundant, but whatever
					If block\what <> 0 
						block\timeshit = block\timeshit - 1
						block\movementcounter = 5 ;FOR THE LOVE OF GOD KEEP THIS ODD
					Else;so block\what IS 0
						If level = 1 ;so if mario can't break the blocks
							block\movementcounter = 5
						Else
							
							spawnfragments(block\x, block\y)
							Delete block
						EndIf
					
					EndIf
					
					
					

				EndIf
				mevy = 0
				meay = 0
			EndIf

		Next
		mey = mey + mevy ;Again, if there is no collision, DO WHAT YOU WANT. YEAH.

		
	EndIf
	y = Int(mey) ;and now the code has to break. I just wrote too much for it not to break

	;again, testing for horizontal collision before vertical collision

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	If RectsOverlap(230-(ImageWidth(mario(dir))/2),Int(mey+mevy)-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)), -50,550,5000,1) = True ;Technically, the last four parameters could be 0,260,3374,0, but I'm giving myself breathing space here 

		Dead = 3
	EndIf
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
Return

.deathbygoomba
	
			
			
			mevy = -28 ;reset all speeds
			mevx = 0 ;Mario stops sideways movement when he death's by goomba
			meay = 2.75
			meax = 0
			mex = 0
			changemario(1) ;make mario smallest			
			level = 1	;again, same thing
			Lives = Lives - 1 ;I WONDER WHAT THIS DOES
			deathpoint = x	;sets the stage to a static point


 ;standard physics calcs
			mevx = mevx + meax
			mevy = mevy + meay
			deathpoint = x
			mex = mex + mevx
			mey = mey + mevy
			
			x = Int(mex)
			y = Int(mey)
			Gosub drawgamestatic
			Flip
			Cls
			Delay(1000)
			dead = 2

Return
Function spawnfragments(x,y)
For failcounter = 1 To 4	
		afragment.ablockfragment = New ablockfragment
			afragment\x = Rand(x, x+32)
			afragment\y = Rand(y, y+32)
			afragment\vx = Rand(1,3)
			afragment\vy = -8
			afragment\ay = 2
			For randomcounter = 0 To 179
				afragment\image[randomcounter] = fragment(randomcounter)
			Next
			afragment\degree = Rand(0,179)
Next
End Function
.updatefragments
For afragment.ablockfragment = Each ablockfragment
	afragment\vy = afragment\vy + afragment\ay
	afragment\x = afragment\x + afragment\vx
	afragment\y = afragment\y + afragment\vy
	afragment\degree = (afragment\degree + 2)Mod 179
Next
Return



.drawgame
	DrawImage Bkground,x-Imgsetx,Imgsety
	For block.ablock = Each ablock
		DrawImage block\sprite, block\x+x-Imgsetx, block\y, block\frame
	Next
	For afragment.ablockfragment = Each ablockfragment
		DrawImage afragment\image[afragment\degree], afragment\x+x-Imgsetx, afragment\y
	Next	
	DrawImage mario(dir), 230, y,frame(dir) 
Return

.drawgamestatic
	DrawImage Bkground,deathpoint-Imgsetx,Imgsety
	For block.ablock = Each ablock
		DrawImage block\sprite, block\x+deathpoint-Imgsetx, block\y, block\frame
	Next
	For afragment.ablockfragment = Each ablockfragment
		DrawImage afragment\image[afragment\degree], afragment\x+deathpoint-Imgsetx, afragment\y
	Next
	DrawImage mario(dir), x+230, y,frame(dir)
Return


.debug
	Text 0, 60, MouseX()+ " - "+ MouseY() + " - " + level
	Text 0,40, "Onground - " + onground +" jumpcounter - " + jumpcounter + " Dead - " + dead
	Text 0,0, "meax - " +meax+" meay - " +meay+" mevx - "+mevx+" mevy - "+mevy
	Text 0,20," mex - "+mex+" mey - "+mey
	;Text 0,0, dir +" - " + frame(1) +" - " + deciframe(1) +" - " +frame(0)+" - " +deciframe(0)
Return