 rem Short for remark means comment
 rem Hello World Program

 rem We define a playfield using 12 lines of 32px (12th is used for vert. scroll)
 rem . is blank
 rem X is filled

 playfield:
 ................................
 X..XXX.XX..X.X.XX..X.XX..XXX.X.X
 X..X.X.X.X.X.X.X.X.X.X.X..X..X.X
 X..XXX.XX...X..XX..X.X.X..X..XXX
 X..X.X.X.X..X..X.X.X.X.X..X..X.X
 XX.X.X.XX...X..X.X.X.X.X..X..X.X
 ................................
 ...XXXXXXXXXXXXXXXXXXXXXXXXXX...
 ....X.X.X.X.X.X..X.X.X.X.X.X....
 ...XXXXXXXXXXXX..XXXXXXXXXXXX...
 ................................
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
 %11111111
 %00000000
 %11111111
 %10000001
 %01111110
 %11000011
 %00111100
 %11100111
end
 player1x = 77
 player1y = 80
 player0x = 77
 player0y = 72
 COLUPF = 14

mainloop
 COLUP0 = $24
 COLUP1 = 14
 drawscreen
 goto mainloop
