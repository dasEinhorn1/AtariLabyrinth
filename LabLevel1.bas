   ;***************************************************************
   ;
   ;  Sprite, Ball, and Missile With Collision Prevention
   ;
   ;  Example program by Duane Alan Hahn (Random Terrain) using
   ;  hints, tips, code snippets, and more from AtariAge members
   ;  such as batari, SeaGtGruff, RevEng, Robert M, Nukey Shay,
   ;  Atarius Maximus, jrok, supercat, GroovyBee, and bogax.
   ;
   ;```````````````````````````````````````````````````````````````
   ;
   ;  Instructions:
   ;
   ;  Use the joystick to move the sprite. Press the fire button
   ;  to shoot the missile. Watch the ball bounce. Press the reset
   ;  switch to reset the program and toggle between two screens.
   ;
   ;```````````````````````````````````````````````````````````````
   ;
   ;  If this program will not compile for you, get the latest
   ;  version of batari Basic:
   ;
   ;  http://www.randomterrain.com/atari-2600-memories-batari-basic-commands.html#gettingstarted
   ;
   ;***************************************************************

   set smartbranching on

   ;***************************************************************
   ;
   ;  Variable aliases go here (DIMs).
   ;
   ;  You can have more than one alias for each variable.
   ;  If you use different aliases for bit operations,
   ;  it's easier to understand and remember what they do.
   ;
   ;  I start variable aliases with one underscore so I won't
   ;  have to worry that I might be using bB keywords by mistake.
   ;  I also start labels with two underscores for the same
   ;  reason. The second underscore also makes labels stand out
   ;  so I can tell at a glance that they are labels and not
   ;  variables.
   ;
   ;  Use bit operations any time you need a simple off/on
   ;  variable. One variable essentially becomes 8 smaller
   ;  variables when you use bit operations.
   ;
   ;  I start my bit aliases with "_Bit" then follow that
   ;  with the bit number from 0 to 7, then another underscore
   ;  and the name. Example: _Bit0_Reset_Restrainer
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Player0/missile0 direction bits.
   ;
   dim _BitOp_P0_M0_Dir = g
   dim _Bit0_P0_Dir_Up = g
   dim _Bit1_P0_Dir_Down = g
   dim _Bit2_P0_Dir_Left = g
   dim _Bit3_P0_Dir_Right = g
   dim _Bit4_M0_Dir_Up = g
   dim _Bit5_M0_Dir_Down = g
   dim _Bit6_M0_Dir_Left = g
   dim _Bit7_M0_Dir_Right = g
   ;```````````````````````````````````````````````````````````````
   ;  Player1/missile1 direction bits.
   ;
   dim _BitOp_P1_M1_Dir = k
   dim _Bit0_P1_Dir_Up = k
   dim _Bit1_P1_Dir_Down = k
   dim _Bit2_P1_Dir_Left = k
   dim _Bit3_P1_Dir_Right = k
   dim _Bit4_P1_Col_Up = k
   dim _Bit5_P1_Col_Down = k
   dim _Bit6_P1_Col_Left = k
   dim _Bit7_P1_Col_Right = k

   dim _Frame_Count = h

   ;'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
   ;  Minotaur awareness radius
   ;
   const _Minotaur_Awareness_Size = 100

   ;```````````````````````````````````````````````````````````````
   ;  Game Stuff
   ;
   const _Score_Phase_1 = 10
   const _Score_Phase_2 = 30

   dim _Bit0_Carrying_Gem = x
   dim _Bit1_Game_Over = x
   dim _Bit2_Start_Screen = x

   ;```````````````````````````````````````````````````````````````
   ;  Bits that do various jobs.
   ;
   dim _BitOp_01 = y
   dim _Bit0_Reset_Restrainer = y
   dim _Bit1_Toggle_Screen = y

   ;```````````````````````````````````````````````````````````````
   ;  Makes better random numbers.
   ;
   dim rand16 = z

   ;***************************************************************
   ;
   ;  Defines the edges of the playfield for an 8 x 8 sprite.
   ;  If your sprite is a different size, you`ll need to adjust
   ;  the numbers.
   ;
   const _P_Edge_Top = 9
   const _P_Edge_Bottom = 88
   const _P_Edge_Left = 1
   const _P_Edge_Right = 153



   ;***************************************************************
   ;
   ;  Defines the edges of the playfield for the ball. If the
   ;  ball is a different size, you'll need to adjust the numbers.
   ;
   const _B_Edge_Top = 2
   const _B_Edge_Bottom = 88
   const _B_Edge_Left = 2
   const _B_Edge_Right = 160



   ;***************************************************************
   ;
   ;  Defines the edges of the playfield for the missile.
   ;  If the missile is a different size, you'll need to adjust
   ;  the numbers.
   ;
   const _M_Edge_Top = 2
   const _M_Edge_Bottom = 88
   const _M_Edge_Left = 2
   const _M_Edge_Right = 159

   ;***************************************************************
   ;***************************************************************
    ;
   ;  PROGRAM START/RESTART
   ;
   ;
__Start_Restart


   ;***************************************************************
   ;
   ;  Mutes volume of both sound channels.
   ;
   AUDV0 = 0 : AUDV1 = 0


   ;***************************************************************
   ;
   ;  Clears 24 of the normal 26 variables (fastest way).
   ;  The variable y holds a bit that should not be cleared. The
   ;  variable z is used for random numbers in this program and
   ;  clearing it would mess up those random numbers.
   ;
   a = 0 : b = 0 : c = 0 : d = 0 : e = 0 : f = 0 : g = 0 : h = 0 : i = 0
   j = 0 : k = 0 : l = 0 : m = 0 : n = 0 : o = 0 : p = 0 : q = 0 : r = 0
   s = 0 : t = 0 : u = 0 : v = 0 : w = 0 : x = 0


   ;***************************************************************
   ;
   ;  Clears 7 of the 8 all-purpose bits. The 4th bit toggles the
   ;  playfield when the reset switch is pressed in this example,
   ;  so we have to leave it alone.
   ;
   _BitOp_01 = _BitOp_01 & %00010000


   ;***************************************************************
   ;
   ;  Sets starting position of player0.
   ;
   player0x = 77 : player0y = 60
   player1x = 21 : player1y = 16

   ;***************************************************************
   ;
   ;  Makes sure missile0 is off the screen.
   ;
   missile0x = 200 : missile0y = 200

   ;***************************************************************
   ; Set up gem starting position
   ;
   missile0x = 80 : missile0y = 79

   ;***************************************************************
   ;
   ;  Defines missile0 size.
   ;
   NUSIZ0 = $10 : missile0height = 1
   NUSIZ1 = $30 : missile0height = 4


   ;***************************************************************
   ;
   ;  Sets playfield color.
   ;
   COLUPF = $2C


   ;***************************************************************
   ;
   ;  Sets background color.
   ;
   COLUBK = 0

   ;***************************************************************
   ;
   ;  Sets beginning direction that missile0 will shoot if the
   ;  player doesn't move.
   ;
   _Bit3_P0_Dir_Right{3} = 1


   ;***************************************************************
   ;
   ;  Restrains the reset switch for the main loop.
   ;
   ;  This bit fixes it so the reset switch becomes inactive if
   ;  it hasn't been released after being pressed once.
   ;
   _Bit0_Reset_Restrainer{0} = 1

;   playfield:
;   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;   X..............................X
;   X..............................X
;   X..............................X
;   X..............................X
;   X..............XX..............X
;   X..............................X
;   X..............................X
;   X..............................X
;   X..............................X
;   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;end

   playfield:
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
   X..............................X
   X..XXXXXX....XX..XX....XXXXXX..X
   X..X........XXX..XXX........X..X
   X..............................X
   X..XX..XX..XX......XX..XX..XX..X
   X..............................X
   X..X........XXX..XXX........X..X
   X..XXXXXX....XX..XX....XXXXXX..X
   X..............................X
   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
end

   player0:
   %00110110
   %00100100
   %00100100
   %00011000
   %01011010
   %00111100
   %00011000
   %00011000
end

   player1:
   %01100110
   %00100100
   %10011001
   %10111101
   %01111110
   %00011000
   %00111100
   %00100100
end
   ;''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
   ;  Reset the score
   ;
   score = 0

   dim _sc1 = score ; 100,000s and 10,000s (XX 00 00)
   dim _sc2 = score+1 ; 1,000s and 100s (00 XX 00)
   dim _sc3 = score+2 ; 10s and ones (00 00 XX)

   r = %00000010
   dim _Chase_Speed = r

   ;***************************************************************
   ;***************************************************************
   ;
   ;  MAIN LOOP (MAKES THE PROGRAM GO)
   ;
   ;
__Main_Loop

   if _Bit1_Game_Over{1} then if joy0fire goto __Start_Restart

   ;''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
   ;  Check collision
   ;
   if !collision(player0, player1) then __Skip_Player_Caught
   _Bit1_Game_Over{1} = 1
   goto __Game_Over
__Skip_Player_Caught

   ;***************************************************************
   ;
   ;  Defines missile0 size.
   ;
   NUSIZ0 = $10 : missile0height = 1
   NUSIZ1 = $30 : missile1height = 7

   missile1x = 78 : missile1y = 48

   _Frame_Count = _Frame_Count + 1

   ;***************************************************************
   ;
   ;  Sets color of player0 sprite.
   ;
   COLUP0 = $9C
   COLUP1 = $34
   scorecolor = $9C

   if _sc1=%00 && _sc2=%00 && _sc3 >= %00 + _Score_Phase_2 then _Chase_Speed = 0 : goto __Skip_Speed_Check2 else COLUP1 = $33
   if _sc1=%00 && _sc2=%00 && _sc3 >= %00 + _Score_Phase_1 then _Chase_Speed = 1 else COLUP1 = $30
__Skip_Speed_Check2

   ;***************************************************************
   ;
   ;  Joystick movement precheck.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Skips section if joystick hasn't been moved.
   ;
   if !joy0up && !joy0down && !joy0left && !joy0right then goto __Skip_Joystick_Precheck

   ;```````````````````````````````````````````````````````````````
   ;  Clears player0 direction bits since joystick has been moved.
   ;
   _BitOp_P0_M0_Dir = _BitOp_P0_M0_Dir & %11110000

__Skip_Joystick_Precheck

   ;***************************************************************
   ;
   ;  Joy0 up check.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if joystick isn't moved up.
   ;
   if !joy0up then goto __Skip_Joy0_Up

   ;```````````````````````````````````````````````````````````````
   ;  Turns on the up direction bit.
   ;
   _Bit0_P0_Dir_Up{0} = 1

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;
   if player0y <= _P_Edge_Top then goto __Skip_Joy0_Up

   ;```````````````````````````````````````````````````````````````
   ;  Stops movement if a playfield pixel is in the way.
   ;
   temp5 = (player0x-11)/4

   temp6 = (player0y-8)/8

   if temp5 < 34 then if pfread(temp5,temp6) then goto __Skip_Joy0_Up

   temp4 = (player0x-16)/4

   if temp4 < 34 then if pfread(temp4,temp6) then goto __Skip_Joy0_Up

   temp3 = temp5 - 1

   if temp3 < 34 then if pfread(temp3,temp6) then goto __Skip_Joy0_Up

   ;```````````````````````````````````````````````````````````````
   ;  Moves player0 up.
   ;
   player0y = player0y - 1

__Skip_Joy0_Up



   ;***************************************************************
   ;
   ;  Joy0 down check.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if joystick isn't moved down.
   ;
   if !joy0down then goto __Skip_Joy0_Down

   ;```````````````````````````````````````````````````````````````
   ;  Turns on the down direction bit.
   ;
   _Bit1_P0_Dir_Down{1} = 1

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;
   if player0y >= _P_Edge_Bottom then goto __Skip_Joy0_Down

   ;```````````````````````````````````````````````````````````````
   ;  Stops movement if a playfield pixel is in the way.
   ;
   temp5 = (player0x-11)/4

   temp6 = (player0y)/8

   if temp5 < 34 then if pfread(temp5,temp6) then goto __Skip_Joy0_Down

   temp4 = (player0x-16)/4

   if temp4 < 34 then if pfread(temp4,temp6) then goto __Skip_Joy0_Down

   temp3 = temp5 - 1

   if temp3 < 34 then if pfread(temp3,temp6) then goto __Skip_Joy0_Down

   ;```````````````````````````````````````````````````````````````
   ;  Moves player0 down.
   ;
   player0y = player0y + 1

__Skip_Joy0_Down



   ;***************************************************************
   ;
   ;  Joy0 left check.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if joystick isn't moved to the left.
   ;
   if !joy0left then goto __Skip_Joy0_Left

   ;```````````````````````````````````````````````````````````````
   ;  Turns on the left direction bit.
   ;
   _Bit2_P0_Dir_Left{2} = 1

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;
   if player0x <= _P_Edge_Left then goto __Skip_Joy0_Left

   ;```````````````````````````````````````````````````````````````
   ;  Stops movement if a playfield pixel is in the way.
   ;
   temp5 = (player0y-1)/8

   temp6 = (player0x-17)/4

   if temp6 < 34 then if pfread(temp6,temp5) then goto __Skip_Joy0_Left

   temp3 = (player0y-7)/8

   if temp6 < 34 then if pfread(temp6,temp3) then goto __Skip_Joy0_Left

   ;```````````````````````````````````````````````````````````````
   ;  Moves player0 left.
   ;
   player0x = player0x - 1

__Skip_Joy0_Left



   ;***************************************************************
   ;
   ;  Joy0 right check.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if joystick isn't moved to the right.
   ;
   if !joy0right then goto __Skip_Joy0_Right

   ;```````````````````````````````````````````````````````````````
   ;  Turns on the right direction bit.
   ;
   _Bit3_P0_Dir_Right{3} = 1

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;
   if player0x >= _P_Edge_Right then goto __Skip_Joy0_Right

   ;```````````````````````````````````````````````````````````````
   ;  Stops movement if a playfield pixel is in the way.
   ;
   temp5 = (player0y-1)/8

   temp6 = (player0x-10)/4

   if temp6 < 34 then if pfread(temp6,temp5) then goto __Skip_Joy0_Right

   temp3 = (player0y-7)/8

   if temp6 < 34 then if pfread(temp6,temp3) then goto __Skip_Joy0_Right

   ;```````````````````````````````````````````````````````````````
   ;  Moves player0 right.
   ;
   player0x = player0x + 1

__Skip_Joy0_Right


   ;''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
   ;  If the player is within the awareness radius of the Minotaur
   ;
   temp1 = player0x + 8 ; player0 right side
   temp2 = player0y + 8 ; player0 bottom

   temp3 = player1x + 4 ; Minotaur center x
   temp4 = player1y + 4 ; Minotaur center y

   temp5 = _Minotaur_Awareness_Size

   if _Frame_Count&_Chase_Speed then goto __Skip_AI_Right

   ; check player top >= awareness bottom
   if player0y >= temp4 + temp5 then goto __Skip_AI_Right

   ; check player bottom <= awareness top
   if temp4 < temp5 then temp4 = temp5
   if temp2 <= temp4 - temp5 then goto __Skip_AI_Right

   ; check player right <= awareness left
   if temp3 < temp5 then temp3 = temp5
   if temp1 <= temp3 - temp5 then goto __Skip_AI_Right

   ; check player left >= awareness right
   if player0x >= temp3 + temp5 then goto __Skip_AI_Right

   ;''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
   ;  Rush the player1 at player0
   ;
   ;***************************************************************
   ;
   ;  AI up check.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if AI isn't below.
   ;
   if player1y <= player0y then goto __Skip_AI_Up

   ;```````````````````````````````````````````````````````````````
   ;  Turns on the up direction bit.
   ;
   _Bit0_P1_Dir_Up{0} = 1

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;
   if player1y <= _P_Edge_Top then goto __Skip_AI_Up

   ;```````````````````````````````````````````````````````````````
   ;  Stops movement if a playfield pixel is in the way.
   ;
   temp5 = (player1x-10)/4

   temp6 = (player1y-9)/8

   if temp5 < 34 then if pfread(temp5,temp6) then goto __Skip_AI_Up

   temp4 = (player1x-17)/4

   if temp4 < 34 then if pfread(temp4,temp6) then goto __Skip_AI_Up

   temp3 = temp5 - 1

   if temp3 < 34 then if pfread(temp3,temp6) then goto __Skip_AI_Up

   ;```````````````````````````````````````````````````````````````
   ;  Moves player1 up.
   ;
   player1y = player1y - 1

__Skip_AI_Up



   ;***************************************************************
   ;
   ;  AI down check.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if AI isn't above.
   ;
   if player1y >= player0y then goto __Skip_AI_Down

   ;```````````````````````````````````````````````````````````````
   ;  Turns on the down direction bit.
   ;
   _Bit1_P1_Dir_Down{1} = 1

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;
   if player1y >= _P_Edge_Bottom then goto __Skip_AI_Down

   ;```````````````````````````````````````````````````````````````
   ;  Stops movement if a playfield pixel is in the way.
   ;
   temp5 = (player1x-10)/4

   temp6 = (player1y)/8

   if temp5 < 34 then if pfread(temp5,temp6) then goto __Skip_AI_Down

   temp4 = (player1x-17)/4

   if temp4 < 34 then if pfread(temp4,temp6) then goto __Skip_AI_Down

   temp3 = temp5 - 1

   if temp3 < 34 then if pfread(temp3,temp6) then goto __Skip_AI_Down

   ;```````````````````````````````````````````````````````````````
   ;  Moves player1 down.
   ;
   player1y = player1y + 1

__Skip_AI_Down



   ;***************************************************************
   ;
   ;  AI left check.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if the player is left of the AI.
   if player1x <= player0x then goto __Skip_AI_Left

   ;```````````````````````````````````````````````````````````````
   ;  Turns on the left direction bit.
   ;
   _Bit2_P1_Dir_Left{2} = 1

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;
   if player1x <= _P_Edge_Left then goto __Skip_AI_Left

   ;```````````````````````````````````````````````````````````````
   ;  Stops movement if a playfield pixel is in the way.
   ;
   temp5 = (player1y-1)/8

   temp6 = (player1x-18)/4

   if temp6 < 34 then if pfread(temp6,temp5) then goto __Skip_AI_Left

   temp3 = (player1y-8)/8

   if temp6 < 34 then if pfread(temp6,temp3) then goto __Skip_AI_Left

   ;```````````````````````````````````````````````````````````````
   ;  Moves player1 left.
   ;
   player1x = player1x - 1

__Skip_AI_Left



   ;***************************************************************
   ;
   ;  AI right check.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if joystick isn't moved to the right.
   if player1x >= player0x then goto __Skip_AI_Right

   ;```````````````````````````````````````````````````````````````
   ;  Turns on the right direction bit.
   ;
   _Bit3_P1_Dir_Right{3} = 1

   ;```````````````````````````````````````````````````````````````
   ;  Skips this section if hitting the edge.
   ;
   if player1x >= _P_Edge_Right then goto __Skip_AI_Right

   ;```````````````````````````````````````````````````````````````
   ;  Stops movement if a playfield pixel is in the way.
   ;
   temp5 = (player1y-1)/8

   temp6 = (player1x-9)/4

   if temp6 < 34 then if pfread(temp6,temp5) then goto __Skip_AI_Right

   temp3 = (player1y-8)/8

   if temp6 < 34 then if pfread(temp6,temp3) then goto __Skip_AI_Right

   ;```````````````````````````````````````````````````````````````
   ;  Moves player1 right.
   ;
   player1x = player1x + 1

__Skip_AI_Right


;''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
;  Pick up the gem
;
   if !collision(missile0, player0) then __Skip_Gem_Collection
   _Bit0_Carrying_Gem{0} = 1
   missile0x = 200 : missile0y = 200
__Skip_Gem_Collection

   ;''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
   ;  Deposit the gem on the platform
   ;
   if !collision(missile1, player0) then __Skip_Gem_Deposit
   if !_Bit0_Carrying_Gem{0} then __Skip_Gem_Deposit
   _Bit0_Carrying_Gem{0} = 0
   score = score + 1
   missile0x = (rand & 118) + 23 : missile0y = (rand & 70) + 9
__Skip_Gem_Deposit

__Game_Over
   if !_Bit1_Game_Over{1} then goto __Skip_Game_Over
   COLUPF = 14
   scorecolor = 14
   COLUBK = $36
   playfield:
   ....XXXXX..XXXX..XX.XX..XXXX....
   ....X......X..X..X.X.X..X.......
   ....X..XX..XXXX..X.X.X..XXX.....
   ....X...X..X..X..X.X.X..X.......
   ....XxXXX..X..X..X.X.X..XXXX....
   ................................
   ....XXXXX..X...X..XXXX..XXX.....
   ....X...X..X...X..X.....X..X....
   ....X...X..X...X..XXX...XXX.....
   ....X...X...X.X...X.....X..X....
   ....XXXXX....X....XXXX..X..X....
end

   player0x = 200 : player0y = 200
   player1x = 200 : player1y = 200
   missile0x = 200 : missile0y = 200
   missile1x = 200 : missile1y = 200
__Skip_Game_Over

   ;***************************************************************
   ;
   ;  Displays the screen.
   ;
   drawscreen



   ;***************************************************************
   ;
   ;  Reset switch check and end of main loop.
   ;
   ;  Any Atari 2600 program should restart when the reset
   ;  switch is pressed. It is part of the usual standards
   ;  and procedures.
   ;
   ;```````````````````````````````````````````````````````````````
   ;  Turns off reset restrainer bit and jumps to beginning of
   ;  main loop if the reset switch is not pressed.
   ;
   if !switchreset then _Bit0_Reset_Restrainer{0} = 0 : goto __Main_Loop

   ;```````````````````````````````````````````````````````````````
   ;  Jumps to beginning of main loop if the reset switch hasn't
   ;  been released after being pressed.
   ;
   if _Bit0_Reset_Restrainer{0} then goto __Main_Loop

   ;```````````````````````````````````````````````````````````````
   ;  Restarts the program.
   ;
   goto __Start_Restart
