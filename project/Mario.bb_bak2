Width = 640
Height = 460

Graphics Width, Height,24, 2
SetBuffer BackBuffer()
ClsColor 0,220,255
SeedRnd MilliSecs()
;counters and random useless flag! Not all of them though, I don't remember where they are defined though...
founter = 0
randomcounter = 0
failcounter = 0
counter = 0
arandomflag = 0
;ints for mario and background. In hindsight, it might have been possible to put this in a STRUCT, but it's too tricky, because not all variables directly apply to mario
running  = 1 ;I WONDER WHAT THIS DOES
Imgsety = 0 ;sets y-coordinate for background. Yes, it does change.
Imgsetx = 230 ;sets x-offset for background. It won't change during program, but I'm going to be mucking around with this
x = 230	  ;xy co-ords integer
y = 230
mex# = 230 ;xy co-ords decimal
Global mey#= 0 ; required for mario type change.
Global mevx# = 0  ;speed in xy direction
Global mevy#= 0
meax# = 0  ;Acceleration in xy direction
meay# = -3
Friction# = 0.25
dir = 1 ;1 is right 0 is left, right is default to start with.
onground = 0
jumpcounter = 0 ;Allows for shorthops and longhops
upheld = 0 ;flag that tells us if up was held
Global Invincible = 0 ;I WONDER WHAT THIS DOES (0 = normal, 1 = star power, 2 = you ran into a goomba and suck at playing this, like me)
	Global Invincitimer = 0 ;Sets how long Mario stays invincible for
	Global invincicolordeci# = 0.50 ;to help cycle slower or you get epileptic Mario
	Global Invincicolor = 0 ;will cycle between 1 and 4 while invincible 
Global Score = 0 ; I WONDER WHAT THIS DOES
;death stuff for mario
Global dead = 0; 0 = alive, 1= lose powerups or death by enemy, 2 = finishing death by enemy, 3 = death by falling, 4 = show respawn screen
deathpoint = 0 ;Allows to give mario his death animation
Global lives = 3;Global for GOOD measure. Yeah. GOOD MEASURE. YEAH.
Global level = 0; 1 = small mario, 2 = big mario, 3 = FIREBALLMARIO
;for levelling up and down animations
Global animateleft = 0 ;time left in the animation, also gives the sprite data to be fed into the changemario function
Global animateleftdeci# = 0.0
Global animatetype = 0 ;1 = small to big, 2 = big to fire, 3 = down to small, regardless.
Global fcounter = 0 ;also required to be here, apparently, so that we can track the length of the animation.
Global oneup = 0 ;This isn't reset, because it tells us if the 1up mushroom has been collected or not. It can only be collected once

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
Global fps = 50

Global ddfpstime = 0




;images
Bkground = LoadImage("Level1ground.bmp")
MaskImage Bkground,255,0,0
groundwall = LoadImage("groundwall.bmp")
Dim invincibig(4,1) ;ugh this part sucked big-time...I might just use these instead, y'know. Also, I could read these off a txt file, but it only shortens code, doesn't speed anything up
invincibig(1,0) = LoadAnimImage("mariobigleft.bmp",32,64,0,7);Most of these are only required for the invicibility animation, y'know.
invincibig(2,0) = LoadAnimImage("mariofireleft.bmp",32,64,0,7)
invincibig(3,0) = LoadAnimImage("altmariobigleft1.bmp",32,64,0,7)
invincibig(4,0) = LoadAnimImage("altmariobigleft2.bmp",32,64,0,7)
invincibig(1,1) = LoadAnimImage("mariobig.bmp",32,64,0,7)
invincibig(2,1) = LoadAnimImage("mariofire.bmp",32,64,0,7)
invincibig(3,1) = LoadAnimImage("altmariobig1.bmp",32,64,0,7)
invincibig(4,1) = LoadAnimImage("altmariobig2.bmp",32,64,0,7)
Dim invincismall(4,1)
invincismall(1,0) = LoadAnimImage("mariosmallleft.bmp",32,32,0,7)
invincismall(2,0) = LoadAnimImage("altmariosmallleft2.bmp",32,32,0,7)
invincismall(3,0) = LoadAnimImage("altmariosmallleft3.bmp",32,32,0,7)
invincismall(4,0) = LoadAnimImage("altmariosmallleft1.bmp",32,32,0,7)
invincismall(1,1) = LoadAnimImage("mariosmall.bmp",32,32,0,7)
invincismall(2,1) = LoadAnimImage("altmariosmall2.bmp",32,32,0,7)
invincismall(3,1) = LoadAnimImage("altmariosmall3.bmp",32,32,0,7)
invincismall(4,1) = LoadAnimImage("altmariosmall1.bmp",32,32,0,7)
For counter = 1 To 4
	For randomcounter = 0 To 1
		MidHandle invincibig(counter,randomcounter)
		MaskImage invincibig(counter,randomcounter),255,0,255
		MidHandle invincismall(counter,randomcounter)
		MaskImage invincismall(counter,randomcounter),255,0,255
	Next
Next
Dim mario(1)
changemario(1)

;blocks
Type ablock
	Field x% ;x-pos of block
	Field y% ;y-pos of block
	Field originaly%;since I can't fix the bump glitch any other way....
	Field what%; what type of block is it? 0 = normal, 1 = coin, 2 = coin but looks like a brick, 3 = mushroom/flower, 4 = 1up, 5 = invincibility, 6 = static
	Field visible%;can you see the block? Only here because there is only one invisible block.
	Field frame%;blocks will use frames, so what frame will we be on?
	Field deciframe#
	Field sprite;this will hold the sprite file.
	Field myhandle;just in case I need to pull out handle and object commands, but I doubt it. Otherwise, it won't do anything
	Field movementcounter ;allows me to make the blocks bounce when headbutted.
	Field timeshit;awkward variable name...it's stands for 'Times Hit' and it's for that idiota multihit questionmark box
End Type
tosplit$ = "" ;temporary variable used for reading stuff from txt files...and probably for writing stuff later

allblocks = ReadFile("blocks.txt")



While Not Eof(allblocks)
	tosplit =  ReadLine$(allblocks)
	block.ablock = New ablock
	;Splitme(tosplit, block\x, block\y, block\what, block\visible) this is what I miss from c++, the ability to do stuff like this line.
	block\x = Mid(tosplit,1,4) ;I've stored the locations and types of all interactive objects (so brick and '?' blocks) in a notepad in a 10-digit format. First four give x-pos, then next four give y-pos, then 9th gives type and 10th gives visible or not.
	block\y = Mid(tosplit,5,4)
	block\originaly = block\y
	block\what = Mid(tosplit,9,1)
	block\visible = Mid(tosplit, 10,1)
	
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

;Enemies! There are two...all goombas and one Koopa troopa....which means that code will have to be written for a green shell as well...great.
Type anenemy
	Field x
	Field y
	Field vy
	Field vx
	Field ay ;no ax because no sideways acceleration
	Field what ;1 = koopatroopa, 2 = shell, 0 = goomba
	Field height ;otherwise the goombas are taller than they look
	Field frame;so we know what frame to sprite
	Field frameregulate ;so the frames won't switch too quickly
	Field sprite
	Field spawned;The goombas and koopas will only spawn when Mario is, let's say...900 pixels away from them. 0 = not spawned, 1 = spawned, 2 = killed by fireball or starman, 3 = squished
	Field counter;for saying how long a squished goomba shall remain until it is removed from existence
End Type
allenemies = ReadFile("goombatroopa.txt");shoulda named it enemygoomba.txt, but whatever.
While Not Eof(allenemies); First 4 give xpos, next 3 give ypos, last flag gives goomba or koopa
	tosplit = ReadLine$(allenemies)
	enemy.anenemy = New anenemy
	enemy\x = Mid(tosplit,1,4)
	enemy\y = Mid(tosplit,5,3)
	enemy\what = Mid(tosplit,8,1)
	enemy\sprite = LoadAnimImage("enemygoomba.bmp",32,48,0,10)
	MaskImage enemy\sprite, 255,0,255
	enemy\frame =4*(enemy\what)
	enemy\frameregulate = 0
	enemy\spawned = 0
	enemy\vx = -3
	enemy\vy = 0
	enemy\ay = 2
	enemy\height = 32 + 16*(enemy\what)
Wend
failcounter = 0





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
;veryrandomuselessonetimeonlytempvariablethatisonlyforthisbit = LoadImage("blockfragment.bmp") ;Why?? Because I can. It's going to be hell on my data dictionary though.
TFormFilter 1
For randomcounter = 0 To 179
fragment(randomcounter) = LoadImage("blockfragment.bmp")
MaskImage fragment(randomcounter), 2,2,2
RotateImage fragment(randomcounter), 2*(randomcounter)
Next
TFormFilter 0


;Powerups
powerups = LoadAnimImage("powerups.bmp", 32,32,0,14)
ugh = 0; this is just a one-time use flag.
Type powerup
	Field x%
	Field y%
	Field dx#
	Field dy#
	Field vx#
	Field vy#
	Field ay#
	Field image
	Field frame%
	Field deciframe#
	Field taken;when this is one, delete the type
	Field stillappearing; for original rising animation
	Field what;tells us what type
End Type




;debugging things
Color 0,0,0

.main;THIS IS NOT A GOTO IT'S JUST HERE FOR MY NAVIGATION PURPOSES BECAUSE THIS CODE IS HUGE
While running = 1
	;fps calculations
	fpstime = MilliSecs()
	
	
	
	;UPDATE
	;FOR MARIO
	;Recieve events and set variables for later calculations
	Select Dead
		Case 0 ;Alive
			Gosub updatemario
			;Gosub verifyandcheckmario
			Gosub updateenemies
			Gosub updateblocks
			Gosub updatepowerups
			Gosub verifyandcheckmario

			Gosub updatefragments
			If Invincible = 1
				applyinvincibility(level)
			ElseIf Invincible = 2
				Invincicounter = Incinvicounter + 1
			EndIf

			Gosub drawgame
		Case 1 ;This is only excuted once when mario dies

			;If Level= 1 <---- this works but only If you consider the Case where he's got a powerup, so leave this in blue For now.
						Gosub deathbygoomba
			;EndIf

		Case 2 ;rest of the death animation for mario
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
		Case 3 ;initial abyss death
		
			Delay(1000)
			;play music during delay ofc
			dead = 4

		Case 4 ;reset stage
			Lives = Lives - 1
			If lives > 0 
				Gosub cleanup
				Gosub reset
			Else
				RuntimeError "YOU DEAD."
			EndIf
			;RuntimeError"That's all, folks! You have this many lives left: " + lives
		Case 5 ;special case....this Mario state is for me to implement the power up and power down animations
			animate(animatetype)
			Gosub verifyandcheckmario
			Gosub drawgame	
			If animateleft = 0
				Dead = 0
				animateleftdeci = 0.0;not necessary?
				changemario(animatetype+1)
				animatetype = 12 ;just some random value
			EndIf
		
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









Function changemario(leveld)
Select leveld
	Case 1
		mario(1) = invincismall(1,1)
		mario(0) = invincismall(1,0)

		level = 1
	Case 2
		mario(1) = invincibig(1,1)
		mario(0) = invincibig(1,0)

		level = 2
	Case 3
		mario(1) = invincibig(2,1)
		mario(0) = invincibig(2,0)
		level = 3
	Default
End Select

End Function
Function animate(Thetype)
;animateleft = 0
;DebugLog "Thetype: " + thetype
Select thetype;1 = small to big, 2 = big to fire, 3 = down to small, regardless.
	
	Case 1
		;DebugLog "Fcounter: " + fcounter

		fcounter = fcounter + 1 ;using it in function so it's local don't have to worry bout a thing
		;DebugLog "Fcounter: " + fcounter
		If fcounter <0		
			changemario(2)
		ElseIf fcounter => 4
			changemario(1)
			fcounter = -5
		EndIf				
	Case 2
		fcounter = fcounter + 1
		If fcounter <0		
			changemario(3)
		ElseIf fcounter => 4
			changemario(2)
			fcounter = -5
		EndIf		
		
	Case 3
		;might have to make some noclip thingo...ugh...these animations make me sad.	
	Default
	
End Select
Mevx = 0
mevy = 0
;DebugLog "Animateleft: " +animateleft
animateleft = animateleft - 5
End Function



.updatemario
	;UPDATE
	;FOR MARIO
	;Recieve events and set variables for later calculations
;DebugLog invincicolor
;DebugLog MilliSecs() - invincitimer
If MilliSecs() - invincitimer >=2000 And Invincible = 2 ;for invincible = 1, that is already dealt with in the applyinvincibility() function
	Invincible = 0
EndIf
If Invincible = 2
	Invincicolor = Invincicolor - 1
	If invincicolor <=-5 Then invincicolor = 4
EndIf

If KeyDown(30) ;That's A, by the way
		isrun = 1
	Else
		isrun = 0
EndIf
If KeyHit(16) ;That's Q
		changemario(1)
EndIf
If KeyHit(17) ;That's W
		changemario(2)
EndIf
If KeyHit(18) ;That's E
		changemario(3)
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
		If jumpcounter > 0 And upheld = 1

			meay = -(jumpcounter)
			jumpcounter = jumpcounter - 1
			upheld = 1
		EndIf
		If onground = 1
			meay = -12
			jumpcounter = 3
			upheld = 1
			mevy = 0
			onground = 0
		EndIf

		If upheld = 0
			jumpcounter = 0
			meay = 2
		EndIf	
	Else
			upheld = 0
	EndIf

	If KeyDown(203) = False And KeyDown(205) = False;if sliding
		meax = 0
		frame(1) = 5
		frame(0) = 1
	ElseIf meax/Abs(meax) = -mevx/Abs(mevx) ;so if velocity is different to direction being held down AND here is an acceleration (so a key is being held down)
		frame(1) = 6
		frame(0) = 0
		
	EndIf 
	If KeyDown(208) = True ;ducking
		meax = 0
		If onground = 1 And level > 1 
			frame(1) = 4
			frame(0) = 2
		EndIf
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


.updateenemies
For enemy.anenemy = Each anenemy
	If  enemy\x+x-Imgsetx <= 900 And enemy\spawned = 0 ;This spawns enemies if mario is within 900 pixels of their spawn point.
		enemy\spawned = 1
	EndIf
	
	;Time to test for collisions with the level....yay!
	;Ahh...but I have a trick up my sleeve...because I can just steal the movement code for the mushroom powerup and apply it to the goombas and koopas. Hell, even the shells, since I'll just change their vx value. GENIUS
	;However, it does mean that this subroutine might have to be moved after testmariocollision, but we'll see. It shouldn't matter.
	;Also, it does all my movement for me too. Yay.
	If enemy\spawned = 1;derp, I forgot this
				enemy\vy = enemy\vy + enemy\ay
		If ImageRectCollide(groundwall, Int(x)-Imgsetx, Imgsety,0, enemy\x+x-Imgsetx, enemy\y+enemy\vy, 32,48 ) = True ;testing vertical collision
			enemy\vy = 0
			While ImageRectCollide(groundwall, x-Imgsetx, Imgsety,0, enemy\x+x-Imgsetx, enemy\y+1, 32,48 ) = False 
					enemy\y = enemy\y + 1
			Wend				
			;apowerup\vy = 0;if there is a collision, the mush- I MEAN THE ENEMY won't drop
			enemy\y = enemy\y + enemy\vy
		Else
				;apowerup\vy = apowerup\vy + apowerup\ay
			For block.ablock = Each ablock
				If RectsOverlap(block\x+x-Imgsetx, block\originaly,32,32,enemy\x+x-Imgsetx, enemy\y+enemy\vy, 32,48) = True
					enemy\vy = 0
					While RectsOverlap(block\x+x-Imgsetx, block\originaly,32,32,enemy\x+x-Imgsetx, enemy\y+1, 32,48) = False
						enemy\y = enemy\y + 1
					Wend
				EndIf
			Next
			enemy\y = enemy\y + enemy\vy
		EndIf


		If ImageRectCollide(groundwall, Int(x)-Imgsetx, Imgsety,0, enemy\x + enemy\vx+x-Imgsetx, enemy\y, 32,48 ) = True ;testing horizontal collision For mushrooms And stars
			enemy\vx = -enemy\vx
			;enemy\frame = (enemy\what)*6;This code will never see the light of day because the koopa troopa never changes direction, still, it's good practice, no? Actually, screw that. It breaks some other code. I know why, but don't fix somethin that ain't broke.
		EndIf
		For block.ablock = Each ablock
			If RectsOverlap(block\x+x-Imgsetx, block\originaly,32,32,enemy\x + enemy\vx+x-Imgsetx, enemy\y, 32,48) = True
				enemy\vx = -enemy\vx
				;enemy\frame = (enemy\what)*6
				enemy\frameregulate = 0
			EndIf
		Next
		enemy\x = enemy\x + enemy\vx
	
	;update frame data
			
		If enemy\what <2	
			enemy\frameregulate = enemy\frameregulate + 1
			If enemy\frameregulate = 5
				enemy\frame = enemy\frame + 1
			Else If enemy\frameregulate => 10
				enemy\frameregulate = 0
				enemy\frame = enemy\frame - 1
			EndIf

			
		EndIf
	
	EndIf

	
	
	
	If enemy\spawned = 3 Then

		If enemy\counter > 0
			enemy\frame = 3
			enemy\counter = enemy\counter - 1

		ElseIf enemy\counter =< 0

			enemy\spawned = 4
		EndIf

	EndIf
	
	If enemy\y > 800
		enemy\spawned = 4
	EndIf
	
	If enemy\spawned = 4
		Delete enemy
	EndIf 

Next

Return


















.updateblocks
For block.ablock = Each ablock
	;block\x = block\x + mevx ;consider if mevx is less than zero for when we get to the point that the stage doesn't move when mario goes left.
	If block\frame < 5
		block\deciframe = (block\deciframe + 0.25)
		If block\deciframe >= 4.5 Then block\deciframe = -0.49
		block\frame = Int(block\deciframe)
	EndIf
	
	If block\movementcounter > 0 ;And (block\frame <> 4 Or block\timeshit > 0) ;you can still hit the multicoin block, but nothing happens. Handy thing is that block\frame<5 covers both brick types
		block\y = block\y - (block\movementcounter-6)
		block\movementcounter = block\movementcounter - 1
		;RuntimeError block\y


	EndIf
;	If block\y <> block\originaly And block\movementcounter = 0
;		block\y = block\y - (original\y - block\y)
;	EndIf
	If block\what = 2 And block\timeshit < 1 
		block\what = 0
		;RuntimeError "dsfa"
	EndIf
	
	If block\timeshit = 0 And (block\frame < 5)
		block\frame = 6
	EndIf

	
	
	
Next



Return
.verifyandcheckmario
If ImageRectCollide(groundwall, x-Imgsetx, Imgsety, 0, 230-(ImageWidth(mario(dir))/2), y-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)) ) = True ;checking to see if anything has happened so that mario now clips with stuff...should just be caused by mario's sprite changing size		
					While ImageRectCollide(groundwall, x-Imgsetx, Imgsety, 0, 230-(ImageWidth(mario(dir))/2), Int(mey)-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)) ) = True ;we'll just teleport mario up until he 's above ground again
						mey = mey - 1
						;DebugLog mey
					Wend
						y = Int(mey)
						onground = 1
Else 
				For block.ablock = Each ablock ;tests for VERTICAL collision with any blocks.
					If RectsOverlap(230-(ImageWidth(mario(dir))/2),y-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)), block\x+x-Imgsetx, block\originaly,32,32) = True ; checking the same thing but for blocks
 						If onground = 1
							While RectsOverlap(230-(ImageWidth(mario(dir))/2),Int(mey)-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)), block\x+x-Imgsetx, block\originaly,32,32) = True
								mey = mey - 1
							Wend
							y = Int(mey)
						Else	
							block\x = block\x + 8000;basically activates noclip on any blocks mario is clipping with
						EndIf
					EndIf
				Next						
					Gosub testmariocollision
				For block.ablock = Each ablock
					If RectsOverlap(230-(ImageWidth(mario(dir))/2),y-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)), block\x-8000+x-Imgsetx, block\originaly+32,32,32) = True;Note the +32 on the block's y value? that's to make sure that Mario doesn't fall out of noclip range while that happens 
									; checking the same thing but for blocks
 						block\x = block\x - 8000;restore original x pos of any noclipped blocks
					EndIf
				Next

EndIf
Return



.testmariocollision
	;mex = mex + mevx
	;mey = mey + mevy
	;x = Int(mex)
	;y = Int(mey)
	
	
	;before testing for ground collision, I need to test for collisions with enemies.
	For enemy.anenemy = Each anenemy
		If enemy\spawned = 1	
				If RectsOverlap(enemy\x+Int(mex+mevx)-Imgsetx, enemy\y+(48-(enemy\height)), 32,enemy\height, 230-(ImageWidth(mario(dir))/2), y-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir))) = True
					Select invincible					
						Case 0		;normal
							If enemy\what = 2 And enemy\x = 0 ;basically, if mario horizontally comes into contact with an unmoving shell, he 'throws' it
								enemy\vx = (mevx/Abs(mevx))*5 ;depending on which way he's moving.
							ElseIf level = 1							
		
										Dead = 1
								Else
										changemario(1)
										Invincible = 2;write code for when he is invincible pls
										Invincitimer = MilliSecs()
										Invincicolor = 4

										;run degrade animation
							EndIf
						Case 1 ;starman
							enemy\spawned = 2

							;Kill enemy, do later
						Case 2 ;just lost powerup
							;Nothing happens
						Default
					End Select
				EndIf
				;vertical collision
				If RectsOverlap(enemy\x+Int(mex+mevx)-Imgsetx, enemy\y+(48-(enemy\height)), 32,enemy\height,  230-(ImageWidth(mario(dir))/2), Int(mevy+mey)-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)));I've just realised that maybe my parameters for collision with blocks are a bit of a dud. The vertical collision numbers should....it shouldn't matter anyway. Don't fix what ain't broke.					
					If enemy\y+(48-(enemy\height))-Int(mevy+mey)+(ImageHeight(mario(dir))/2) <= 0 ;so you can't stomp a falling goomba....basically, takes the upper bound of enemies hitbox and makes sure it's below the lower bound of Mario's hitbox
						Select invincible					
							Case 0		;normal
								If enemy\what = 2 And enemy\x = 0 ;basically, if mario horizontally comes into contact with an unmoving shell, he 'throws' it
									enemy\vx = (mevx/Abs(mevx))*5 ;depending on which way he's moving.
								ElseIf level = 1							
			
											Dead = 1
									Else
											changemario(1)
											Invincible = 2;write code for when he is invincible pls
											Invincitimer = MilliSecs()
											Invincicolor = 4
	
											;run degrade animation
								EndIf
							Case 1 ;starman
								enemy\spawned = 2
	
								;Kill enemy, do later
							Case 2 ;just lost powerup
								;Nothing happens
							Default
						End Select
					
					
					Else
					
					
					
					
					
					
						
						If invincible = 0; the invincible code will be implemented in horizontal detection	
							Select enemy\what
								Case 0
									enemy\spawned = 3
									mevy = -10
									enemy\counter = 40
	
								Case 1
									enemy\what = 2
	
									enemy\frame = 8
									enemy\height = 32
									enemy\vx = 0
									mevy = -10
									Exit
								Case 2
									If enemy\vx <> 0 
										enemy\vx = 0
										mevy = -10
									Else
										enemy\vx = (mevx/Abs(mevx))*5
										mevy = -10
									EndIf
								Default
							End Select
						EndIf
							
					EndIf	
						
				EndIf

			
			
			
					
			
			
				
		EndIf	
	Next
	
	
	
	
	
	
	
	If ImageRectCollide(groundwall, Int(mex+mevx)-Imgsetx, Imgsety, 0, 230-(ImageWidth(mario(dir))/2), y-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)) ) = True ;This moves the background along the x axis and checks for collision before finishing calculations
		While ImageRectCollide(groundwall, Int(mex+(mevx/Abs(mevx)))-Imgsetx, Imgsety, 0, 230-(ImageWidth(mario(dir))/2), y-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)) ) = False
			mex = mex + (mevx/Abs(mevx)) ;makes sure Mario is as close as his sprite range will allow him to be next to the wall.
		Wend
		
		
		mevx = 0 ;If there is one, the calculation isn't made and the player stops moving sideways
		meax = 0

	Else
		For block.ablock = Each ablock ;tests for sideways collision with any blocks.
			If RectsOverlap(230-(ImageWidth(mario(dir))/2),y-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)), block\x+Int(mex+mevx)-Imgsetx, block\originaly,32,32) = True
				While RectsOverlap(230-(ImageWidth(mario(dir))/2),y-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)), block\x+(mex + (mevx/Abs(mevx)))-Imgsetx, block\originaly,32,32) = False
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
			If RectsOverlap(230-(ImageWidth(mario(dir))/2),Int(mey+mevy)-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)), block\x+x-Imgsetx, block\originaly,32,32) = True
				While RectsOverlap(230-(ImageWidth(mario(dir))/2),Int(mey + (mevy/Abs(mevy)))-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)), block\x+x-Imgsetx, block\originaly,32,32) = False ;makes sure feet touch the ground
					mey = mey + (mevy/Abs(mevy))
				Wend
				
				If mevy >= 0 ;If he is travelling downwards and hits from above
					onground = 1
					
				Else ;If he hits from below....TIME TO LOAD SOME SPRITES.
										onground = 0 ;redundant, but whatever
					If block\what <> 0 And block\frame <> 6 ;a little lazy...this check might not be necessary if I put this in the right nested if loop, but I'd rather not mess with that.
						spawnpowerups(block\x, block\y, block\what)
						If block\what = 1 Or block\what = 2 Then score = score + 200 ;add 200 points per coin bump thingo.
					EndIf
					
					
					If ((block\what <> 0 And block\frame <5) Or block\what = 2) And block\movementcounter = 0;for nonbrick blocks which haven't changed or that batshit crazy multihit block
						block\timeshit = block\timeshit - 1

						;If block\movementcounter = 0
							block\movementcounter = 11;FOR THE LOVE OF GOD KEEP THIS ODD - also, adding block\movementcounter+ is a quick fix...it only applies to one block with a certain behaviour and is unlikely to happen, but if it does, nothing major happens anyway. It's just not a faithful reproduction of the mario game. I think.
						;EndIf ;so many nested ifs.....
					Else;so block\what IS 0 or 2
						If level = 1 And block\frame <> 6 ;so if mario can't break the blocks
							If block\movementcounter = 0
								block\movementcounter = 11
							EndIf
							
						Else
							
							If (block\frame <> 6 And block\what<>2) ;Or (block\what = 2 And block\timeshit >= 0)
								spawnfragments(block\x, block\y)
								Delete block
							EndIf
							
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
	For apowerup.powerup = Each powerup
		If RectsOverlap(230-(ImageWidth(mario(dir))/2),y-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)), apowerup\x+x-Imgsetx, apowerup\y,32,32)
			;RuntimeError "YOU PICKED ME UP!!!!"
			apowerup\taken = 1;eh, this line is probably redundant
			applypowerup(apowerup\what)
			Delete apowerup
		EndIf
	Next
	;again, testing for horizontal collision before vertical collision

	

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	If RectsOverlap(230-(ImageWidth(mario(dir))/2),Int(mey+mevy)-(ImageHeight(mario(dir))/2), ImageWidth(mario(dir)), ImageHeight(mario(dir)), -50,550,7000,1) = True ;Technically, the last four parameters could be 0,260,3374,0, but I'm giving myself breathing space here 

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


			deathpoint = x	;sets the stage to a static point


 ;standard physics calcs
			;mevx = mevx + meax
			;mevy = mevy + meay
			deathpoint = x
			;mex = mex + mevx
			;mey = mey + mevy
			
			x = Int(mex)
			y = Int(mey)
			Gosub drawgamestatic
			Flip
			;Cls
			Delay(1000)
			dead = 2

Return
Function spawnfragments(x,y)
	For failcounter = 1 To 4	
		afragment.ablockfragment = New ablockfragment
			afragment\x = Rand(x, x+32)
			afragment\y = Rand(y, y+32)
			afragment\vx = Rand(1,7)-4
			afragment\vy = -16
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
Function spawnpowerups(x,y,what)
	apowerup.powerup = New powerup
		apowerup\x% = x
		apowerup\y% = y
		apowerup\dx# = x
		apowerup\dy# = y
		apowerup\vx# = 2
		apowerup\vy# = 1
		apowerup\ay# = 1
		apowerup\image =  LoadAnimImage("powerups.bmp", 32,32,0,14)
		MaskImage apowerup\image, 255,0,255
		apowerup\taken = 0
		Select what
			Case 1
				apowerup\frame = 0 
				apowerup\stillappearing = 0 
				apowerup\what = what
				apowerup\vy = -6.5
				apowerup\ay = 10
			Case 2
				apowerup\frame = 0
				apowerup\stillappearing = 0
				apowerup\what = 1 ;Is the same as other because both spawn a coin
				apowerup\vy = -6.5
				apowerup\ay = 10
			Case 3
				If level = 1
					apowerup\frame = 4
					apowerup\stillappearing = 43 
					apowerup\what = 3
				Else
					;RuntimeError level
					apowerup\frame = 6
					apowerup\stillappearing = 43 ; I just KNEW leaving myself a free what would be good......this is for the fireflower to spawn.
					apowerup\what = 2
				EndIf 
					
			Case 4
					apowerup\frame = 5
					apowerup\stillappearing = 43 
					apowerup\what = 4
			Case 5 
				apowerup\frame = 10
				apowerup\stillappearing = 43
				apowerup\what = 5
				apowerup\ay = 1
			Default

		End Select
	apowerup\deciframe = 0
End Function 
.updatepowerups
	For apowerup.powerup = Each powerup 
		Select  apowerup\what ;ugh, in all fairness, I assigned inefficent apowerup\what values, so I have to use a select case
			Case 1
				apowerup\deciframe = apowerup\deciframe + 0.5
				If apowerup\deciframe => 3.49
					apowerup\deciframe = -0.49
				EndIf
				apowerup\frame = Int(apowerup\deciframe) 
				If apowerup\ay > 0	
					apowerup\dy = apowerup\dy + apowerup\vy
					apowerup\y = Int(apowerup\dy)
					apowerup\ay = apowerup\ay - 1
				Else
					apowerup\taken = 1
				
				EndIf
				
			Case 2
				apowerup\deciframe = apowerup\deciframe + 0.25
				If apowerup\deciframe => 3.49
					apowerup\deciframe = -0.49
				EndIf
				apowerup\frame = Int(apowerup\deciframe) + 6
			Case 5 
				apowerup\deciframe = apowerup\deciframe + 0.25
				If apowerup\deciframe => 3.49
					apowerup\deciframe = -0.49
				EndIf
				apowerup\frame = Int(apowerup\deciframe)  + 10
			Default
		End Select
 
		
		
		
		
		
		
		
		
		If apowerup\stillappearing > 0 ;while the sprite is raising out of the block - This is for all types except for coin types
			apowerup\dy = apowerup\dy - 0.75
			apowerup\y = Int(apowerup\dy)
			apowerup\stillappearing = apowerup\stillappearing - 1
		Else
		
			If (apowerup\what => 3 And apowerup\what <= 5) And apowerup\stillappearing = 0 
				If ImageRectCollide(groundwall, Int(x)-Imgsetx, Imgsety,0, Int(apowerup\dx + apowerup\vx)+x-Imgsetx, apowerup\y, 32,32 ) = True ;testing horizontal collision For mushrooms And stars
					apowerup\vx = -apowerup\vx
				EndIf
				For block.ablock = Each ablock
					If RectsOverlap(block\x+x-Imgsetx, block\originaly,32,32,Int(apowerup\dx + apowerup\vx)+x-Imgsetx, apowerup\y, 32,32) = True
						apowerup\vx = -apowerup\vx
					EndIf
				Next
				apowerup\dx = apowerup\dx + apowerup\vx
				apowerup\x = Int(apowerup\dx)
			EndIf
		EndIf
		If apowerup\what = 3 Or apowerup\what = 4 ;And apowerup\stillappearing = 0 ;vertical collision code for the Big mario mushroom.......hnnnnnnnng there's a more efficent way to do this ,adn to combine code for this and the stars, but it's too hard to do it in BB (for minimal gain)...if C++ had worked....
			apowerup\vy = apowerup\vy + apowerup\ay

			If ImageRectCollide(groundwall, Int(x)-Imgsetx, Imgsety,0, apowerup\x+x-Imgsetx, Int(apowerup\dy+apowerup\vy), 32,32 ) = True ;testing vertical collision
				apowerup\vy = 0
				While ImageRectCollide(groundwall, Int(x)-Imgsetx, Imgsety,0, apowerup\x+x-Imgsetx, Int(apowerup\dy+1), 32,32 ) = False 
						apowerup\dy = apowerup\dy + 1
				Wend				
				;apowerup\vy = 0;if there is a collision, the mushroom won't drop
				apowerup\dy = apowerup\dy + apowerup\vy
				apowerup\y = Int(apowerup\dy)

			Else

				;apowerup\vy = apowerup\vy + apowerup\ay
				For block.ablock = Each ablock
					If RectsOverlap(block\x+x-Imgsetx, block\originaly,32,32,apowerup\x+x-Imgsetx, Int(apowerup\dy+apowerup\vy), 32,32) = True
						apowerup\vy = 0
						While RectsOverlap(block\x+x-Imgsetx, block\originaly,32,32,apowerup\x+x-Imgsetx, Int(apowerup\dy+1), 32,32) = False
							apowerup\dy = apowerup\dy + 1
						Wend
					EndIf
				Next
				apowerup\dy = apowerup\dy + apowerup\vy
				apowerup\y = Int(apowerup\dy)
			EndIf

		EndIf
		If apowerup\what = 5 
			If ImageRectCollide(groundwall, Int(x)-Imgsetx, Imgsety,0, apowerup\x+x-Imgsetx, Int(apowerup\dy+apowerup\vy), 32,32 ) = True ;testing vertical collision
				apowerup\vy = -15
				While ImageRectCollide(groundwall, Int(x)-Imgsetx, Imgsety,0, apowerup\x+x-Imgsetx, Int(apowerup\dy+1), 32,32 ) = False 
					apowerup\dy = apowerup\dy + 1
				Wend				
				;apowerup\vy = 0;if there is a collision, the mushroom won't drop
				;apowerup\dy = apowerup\dy + apowerup\vy
				apowerup\y = Int(apowerup\dy)

			Else

				ugh = 0
				For block.ablock = Each ablock
					If RectsOverlap(block\x+x-Imgsetx, block\originaly,32,32,apowerup\x+x-Imgsetx, Int(apowerup\dy+apowerup\vy), 32,32) = True
						apowerup\vy = -apowerup\vy ;Now, LOGICALLY, this should work, but I bet you the bounces change height. UPDATE. THEY DO WHYWHWHYWHYHWYWHYWHWHWHWHYWHWYHWYHWH
						
						ugh = 1
						While RectsOverlap(block\x+x-Imgsetx, block\originaly,32,32,apowerup\x+x-Imgsetx, Int(apowerup\dy+1), 32,32) = False
							apowerup\dy = apowerup\dy - (apowerup\vy/Abs(apowerup\vy))
						Wend
					EndIf
				Next
				If ugh  = 0 ;I don't see another way out of using ugh. I avoided it with the mushroom, but this required something slightly different, and that was enough to ruin it
					apowerup\vy = apowerup\vy + apowerup\ay
				EndIf 
				apowerup\dy = apowerup\dy + apowerup\vy
				apowerup\y = Int(apowerup\dy)
			EndIf

		EndIf
		If apowerup\taken = 1
			Delete apowerup
		EndIf 

	Next
			 
Return
Function applypowerup(what)

	Select what
		Case 1
			;YOU ARE A DONKEY IF YOU ARE READING THIS. Unless you are me.

		Case 2
			Score = score + 1000
			animatetype = 2
			animateleft = fps
			Dead = 5
			Changemario(3) ;Make fire - will put this line in later

		Case 3 

			Score = score + 1000
			animatetype = 1
			animateleft = fps
			Dead = 5
			Changemario(2) ;Make big mario

		Case 4 
			Lives = lives + 1
			Score = score + 1000
			oneup = 1

		Case 5
			Invincible = 1 ;OMGWTFBBQ THINGS BE HAPPENING
			Invincitimer = MilliSecs()
			Score = score + 1000

		Default
	End Select

End Function
Function applyinvincibility(leveln)
invincicolordeci = invincicolordeci + 0.25
If invincicolordeci > 4.49 Then invincicolordeci = 0.51
Invincicolor = Int(invincicolordeci)
;DebugLog invincicolor
;DebugLog invincicolordeci
Select leveln
	Case 1
		mario(1) = invincismall(invincicolor,1) ;Maybe a for loop outside the select case might tidy the code up a smidgen, but I DON'T CARE 
		mario(0) = invincismall(invincicolor,0)
		level= 1
	Case 2
		;RuntimeError invincicolor
		mario(1) = invincibig(invincicolor,1)
		mario(0) = invincibig(invincicolor,0)
		level = 2
	Case 3
		mario(1) = invincibig(invincicolor,1)
		mario(0) = invincibig(invincicolor,0)
		level = 3
	Default
End Select
If MilliSecs() - invincitimer >=12000
	Invincible = 0
	changemario(level);this is a pretty neat way to revert Mario back to his original skin...if I may say so myself.
EndIf
End Function




.drawgame
	DrawImage Bkground,x-Imgsetx,Imgsety
	For apowerup.powerup = Each powerup
		DrawImage apowerup\image, apowerup\x+x-Imgsetx, apowerup\y, apowerup\frame

		;RuntimeError apowerup\x + " - " + apowerup\y
	Next
	For block.ablock = Each ablock ;to stop powerups from peeking out
		Color 0,220,255
		Rect block\x+x-Imgsetx, block\originaly,32,32
		Color 0,0,0
	Next
	
	For block.ablock = Each ablock
		arandomflag = 0
		If block\x > 8000 
			block\x = block\x - 8000
			arandomflag = 1
		EndIf
		
		DrawImage block\sprite, block\x+x-Imgsetx, block\y, block\frame
		If arandomflag = 1
			block\x = block\x + 8000
		EndIf
	Next
	For enemy.anenemy = Each anenemy
		DebugLog enemy\frame
		If enemy\spawned <> 0 
			DrawImage enemy\sprite, enemy\x+x-Imgsetx,enemy\y, enemy\frame
		EndIf
	Next
	
	
	
	
	For afragment.ablockfragment = Each ablockfragment
		DrawImage afragment\image[afragment\degree], afragment\x+x-Imgsetx, afragment\y
	Next	
	If Not Invincible = 2 And invincicolor => 0
		DrawImage mario(dir), 230, y,frame(dir) 
	EndIf
Return

.drawgamestatic ;this is a largly unused portion of code....only seen when mario dies..but I keep it up to date just in case I get around to telling the difference between mario moving left and right.
	DrawImage Bkground,deathpoint-Imgsetx,Imgsety;anyway, all static does is replaces all instances of x with deathpoint
	For block.ablock = Each ablock ;to stop powerups from peeking out
		Color 0,220,255
		Rect block\x+deathpoint-Imgsetx, block\originaly,32,32
		Color 0,0,0
	Next		
	For block.ablock = Each ablock
		arandomflag = 0
		If block\x > 8000 
			block\x = block\x - 8000
			arandomflag = 1
		EndIf
		
		DrawImage block\sprite, block\x+deathpoint-Imgsetx, block\y, block\frame
		If arandomflag = 1
			block\x = block\x + 8000
		EndIf
	Next
	For enemy.anenemy = Each anenemy
		If enemy\spawned <> 0 
			DrawImage enemy\sprite, enemy\x+deathpoint-Imgsetx,enemy\y, enemy\frame
		EndIf
	Next
	
	For afragment.ablockfragment = Each ablockfragment
		DrawImage afragment\image[afragment\degree], afragment\x+deathpoint-Imgsetx, afragment\y
	Next
	If Not Invincible = 2 And invincicolor => 0
		DrawImage mario(dir), x+230, y,frame(dir) 
	EndIf
Return


.debug
	Text 0, 60, MouseX()+ " - "+ MouseY() + " - " + level
	Text 0,40, "Onground - " + onground +" jumpcounter - " + jumpcounter + " Dead - " + dead
	Text 0,0, "meax - " +meax+" meay - " +meay+" mevx - "+mevx+" mevy - "+mevy
	Text 0,20," mex - "+mex+" mey - "+mey + " animateleft - " + animateleft
	For block.ablock = Each ablock
		Text block\x+x-Imgsetx, block\y-20, "Times hit - " +block\timeshit + " Type - " +block\what
	Next

	;Text 0,0, dir +" - " + frame(1) +" - " + deciframe(1) +" - " +frame(0)+" - " +deciframe(0)
Return

.cleanup
For block.ablock = Each ablock
	Delete block
Next
For enemy.anenemy = Each anenemy
	Delete enemy
Next
For afragment.ablockfragment = Each ablockfragment
	Delete afragment
Next
For apowerup.powerup = Each powerup
	Delete apowerup
Next 
Return
.reset
;reset most of mario's numbers except for score and lives, obviously.
founter = 0
randomcounter = 0
failcounter = 0
counter = 0
arandomflag = 0
running  = 1 
Imgsety = 0 
Imgsetx = 230 
x = 230	  
y = 230
mex# = 230 
mey#= 0 
mevx# = 0 
mevy#= 0
meax# = 0  
meay# = -3
Friction# = 0.25
dir = 1 
onground = 0
jumpcounter = 0 
upheld = 0 
Invincible = 0
	Invincitimer = 0 
	invincicolordeci# = 0.50 
	Invincicolor = 0 
;death stuff for mario...reset all except for lives
dead = 0
deathpoint = 0 
;DO NOT RESET LIVES LOL
level = 0
animateleft = 0 
animateleftdeci# = 0.0
animatetype = 0 
fcounter = 0 
changemario(1)

;frame sprite shenanigans...which will be reset just in case

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


;frame stuff doesn't need to be reset




;images don't need to be reloaded



;blocks. Must reload level data for them
allblocks = ReadFile("blocks.txt")
While Not Eof(allblocks)
	tosplit =  ReadLine$(allblocks)
	block.ablock = New ablock
	block\x = Mid(tosplit,1,4) 
	block\y = Mid(tosplit,5,4)
	block\originaly = block\y
	block\what = Mid(tosplit,9,1)
	block\visible = Mid(tosplit, 10,1)
	block\sprite = LoadAnimImage("blocks.bmp", 32,32,0,7)
	MaskImage block\sprite, 255,0,255
	block\timeshit = 1
	If block\what = 0 Or block\what = 2
		block\frame = 5
		
		If block\what = 2 Then block\timeshit = 12
	Else	
		block\frame = 0
		
		block\deciframe = 0.00
	EndIf
	If block\what = 4
		If oneup = 1
			Delete block
		EndIf
	EndIf
Wend
CloseFile(allblocks)

;Enemies. Must reload level data for them
allenemies = ReadFile("goombatroopa.txt")
While Not Eof(allenemies);
	tosplit = ReadLine$(allenemies)
	enemy.anenemy = New anenemy
	enemy\x = Mid(tosplit,1,4)
	enemy\y = Mid(tosplit,5,3)
	enemy\what = Mid(tosplit,8,1)
	enemy\sprite = LoadAnimImage("enemygoomba.bmp",32,48,0,10)
	MaskImage enemy\sprite, 255,0,255
	enemy\frame =4*(enemy\what)
	enemy\frameregulate = 0
	enemy\spawned = 0
	enemy\vx = -3
	enemy\vy = 0
	enemy\ay = 2
	enemy\height = 32 + 16*(enemy\what)
Wend
failcounter = 0

;block fragments
;nothing to reset


;Powerups

ugh = 0; this is just a one-time use flag.



Return