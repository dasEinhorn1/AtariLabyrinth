; Short for remark means comment
; Hello World Program

; We define a playfield using 12 lines of 32px (12th is used for vert. scroll)
; . is blank
; X is filled



  playfield:
  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  X..............................X
  X..............................X
  X..............................X
  X..............................X
  X..............XX..............X
  X..............................X
  X..............................X
  X..............................X
  X..............................X
  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
end

  player0:
  %01100110
  %00100100
  %10011001
  %10111101
  %01111110
  %00011000
  %00111100
  %00100100
end

  player1:
  %00100100
  %00100100
  %00100100
  %10011001
  %01011010
  %00111100
  %00011000
  %00011000
end

__START_RESTART
  ;-------------------------------------------------------------------------
  ; DEFINE BOUNDS OF SPRITE
  ;-------------------------------------------------------------------------
  const _P_Edge_Top = 9
  const _P_Edge_Bottom = 88
  const _P_Edge_Left = 1
  const _P_Edge_Right = 153

  ;-------------------------------------------------------------------------
  ; BALL SETUP
  ;-------------------------------------------------------------------------
  ballx = 64
  bally = 78

  ballheight = 4 : rem * Ball 4 pixels high.

  CTRLPF = $21 : rem * Ball 4 pixels wide.

  COLUPF = $1E : rem * Ball/playfield color.
;-------------------------------------------------------------------------
; PRE-LOOP SETUP
;-------------------------------------------------------------------------
  player0x = 77
  player0y = 72
  player1x = 77
  player1y = 22

  missile1x = 60;
  missile1y = 60;
;-------------------------------------------------------------------------------
; MAINLOOP
;-------------------------------------------------------------------------------
mainloop
  COLUPF = 14
  COLUP0 = $24
  COLUP1 = 128
  ;  Moves player1 sprite with the joystick while keeping the
  ;  sprite within the playfield area.
  if joy0up && player1y > _P_Edge_Top then player1y = player1y - 1

  if joy0down && player1y < _P_Edge_Bottom then player1y = player1y + 1

  if joy0left && player1x > _P_Edge_Left then player1x = player1x - 1

  if joy0right && player1x < _P_Edge_Right then player1x = player1x + 1

  drawscreen
  goto mainloop
