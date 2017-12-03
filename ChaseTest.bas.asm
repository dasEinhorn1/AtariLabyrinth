 processor 6502
 include "vcs.h"
 include "macro.h"
 include "2600basic.h"
 include "2600basic_variable_redefs.h"
 ifconst bankswitch
  if bankswitch == 8
     ORG $1000
     RORG $D000
  endif
  if bankswitch == 16
     ORG $1000
     RORG $9000
  endif
  if bankswitch == 32
     ORG $1000
     RORG $1000
  endif
  if bankswitch == 64
     ORG $1000
     RORG $1000
  endif
 else
   ORG $F000
 endif

 ifconst bankswitch_hotspot
 if bankswitch_hotspot = $083F ; 0840 bankswitching hotspot
   .byte 0 ; stop unexpected bankswitches
 endif
 endif
start
 sei
 cld
 ldy #0
 lda $D0
 cmp #$2C               ;check RAM location #1
 bne MachineIs2600
 lda $D1
 cmp #$A9               ;check RAM location #2
 bne MachineIs2600
 dey
MachineIs2600
 ldx #0
 txa
clearmem
 inx
 txs
 pha
 bne clearmem
 sty temp1
 ifnconst multisprite
 ifconst pfrowheight
 lda #pfrowheight
 else
 ifconst pfres
 lda #(96/pfres)
 else
 lda #8
 endif
 endif
 sta playfieldpos
 endif
 ldx #5
initscore
 lda #<scoretable
 sta scorepointers,x 
 dex
 bpl initscore
 lda #1
 sta CTRLPF
 ora INTIM
 sta rand

 ifconst multisprite
   jsr multisprite_setup
 endif

 ifnconst bankswitch
   jmp game
 else
   lda #>(game-1)
   pha
   lda #<(game-1)
   pha
   pha
   pha
   ldx #1
   jmp BS_jsr
 endif
     ; This is a 2-line kernel!
     ifnconst vertical_reflect
kernel
     endif
     sta WSYNC
     lda #255
     sta TIM64T

     lda #1
     sta VDELBL
     sta VDELP0
     ldx ballheight
     inx
     inx
     stx temp4
     lda player1y
     sta temp3

     ifconst shakescreen
         jsr doshakescreen
     else
         ldx missile0height
         inx
     endif

     inx
     stx stack1

     lda bally
     sta stack2

     lda player0y
     ldx #0
     sta WSYNC
     stx GRP0
     stx GRP1
     stx PF1L
     stx PF2
     stx CXCLR
     ifconst readpaddle
         stx paddle
     else
         sleep 3
     endif

     sta temp2,x

     ;store these so they can be retrieved later
     ifnconst pfres
         ldx #128-44+(4-pfwidth)*12
     else
         ldx #132-pfres*pfwidth
     endif

     dec player0y

     lda missile0y
     sta temp5
     lda missile1y
     sta temp6

     lda playfieldpos
     sta temp1
     
     ifconst pfrowheight
         lda #pfrowheight+2
     else
         ifnconst pfres
             lda #10
         else
             lda #(96/pfres)+2 ; try to come close to the real size
         endif
     endif
     clc
     sbc playfieldpos
     sta playfieldpos
     jmp .startkernel

.skipDrawP0
     lda #0
     tay
     jmp .continueP0

.skipDrawP1
     lda #0
     tay
     jmp .continueP1

.kerloop     ; enter at cycle 59??

continuekernel
     sleep 2
continuekernel2
     lda ballheight
     
     ifconst pfres
         ldy playfield+pfres*pfwidth-132,x
         sty PF1L ;3
         ldy playfield+pfres*pfwidth-131-pfadjust,x
         sty PF2L ;3
         ldy playfield+pfres*pfwidth-129,x
         sty PF1R ; 3 too early?
         ldy playfield+pfres*pfwidth-130-pfadjust,x
         sty PF2R ;3
     else
         ldy playfield-48+pfwidth*12+44-128,x
         sty PF1L ;3
         ldy playfield-48+pfwidth*12+45-128-pfadjust,x ;4
         sty PF2L ;3
         ldy playfield-48+pfwidth*12+47-128,x ;4
         sty PF1R ; 3 too early?
         ldy playfield-48+pfwidth*12+46-128-pfadjust,x;4
         sty PF2R ;3
     endif

     ; should be playfield+$38 for width=2

     dcp bally
     rol
     rol
     ; rol
     ; rol
goback
     sta ENABL 
.startkernel
     lda player1height ;3
     dcp player1y ;5
     bcc .skipDrawP1 ;2
     ldy player1y ;3
     lda (player1pointer),y ;5; player0pointer must be selected carefully by the compiler
     ; so it doesn't cross a page boundary!

.continueP1
     sta GRP1 ;3

     ifnconst player1colors
         lda missile1height ;3
         dcp missile1y ;5
         rol;2
         rol;2
         sta ENAM1 ;3
     else
         lda (player1color),y
         sta COLUP1
         ifnconst playercolors
             sleep 7
         else
             lda.w player0colorstore
             sta COLUP0
         endif
     endif

     ifconst pfres
         lda playfield+pfres*pfwidth-132,x 
         sta PF1L ;3
         lda playfield+pfres*pfwidth-131-pfadjust,x 
         sta PF2L ;3
         lda playfield+pfres*pfwidth-129,x 
         sta PF1R ; 3 too early?
         lda playfield+pfres*pfwidth-130-pfadjust,x 
         sta PF2R ;3
     else
         lda playfield-48+pfwidth*12+44-128,x ;4
         sta PF1L ;3
         lda playfield-48+pfwidth*12+45-128-pfadjust,x ;4
         sta PF2L ;3
         lda playfield-48+pfwidth*12+47-128,x ;4
         sta PF1R ; 3 too early?
         lda playfield-48+pfwidth*12+46-128-pfadjust,x;4
         sta PF2R ;3
     endif 
     ; sleep 3

     lda player0height
     dcp player0y
     bcc .skipDrawP0
     ldy player0y
     lda (player0pointer),y
.continueP0
     sta GRP0

     ifnconst no_blank_lines
         ifnconst playercolors
             lda missile0height ;3
             dcp missile0y ;5
             sbc stack1
             sta ENAM0 ;3
         else
             lda (player0color),y
             sta player0colorstore
             sleep 6
         endif
         dec temp1
         bne continuekernel
     else
         dec temp1
         beq altkernel2
         ifconst readpaddle
             ldy currentpaddle
             lda INPT0,y
             bpl noreadpaddle
             inc paddle
             jmp continuekernel2
noreadpaddle
             sleep 2
             jmp continuekernel
         else
             ifnconst playercolors 
                 ifconst PFcolors
                     txa
                     tay
                     lda (pfcolortable),y
                     ifnconst backgroundchange
                         sta COLUPF
                     else
                         sta COLUBK
                     endif
                     jmp continuekernel
                 else
                     ifconst kernelmacrodef
                         kernelmacro
                     else
                         sleep 12
                     endif
                 endif
             else
                 lda (player0color),y
                 sta player0colorstore
                 sleep 4
             endif
             jmp continuekernel
         endif
altkernel2
         txa
         ifnconst vertical_reflect
             sbx #256-pfwidth
         else
             sbx #256-pfwidth/2
         endif
         bmi lastkernelline
         ifconst pfrowheight
             lda #pfrowheight
         else
             ifnconst pfres
                 lda #8
             else
                 lda #(96/pfres) ; try to come close to the real size
             endif
         endif
         sta temp1
         jmp continuekernel
     endif

altkernel

     ifconst PFmaskvalue
         lda #PFmaskvalue
     else
         lda #0
     endif
     sta PF1L
     sta PF2


     ;sleep 3

     ;28 cycles to fix things
     ;minus 11=17

     ; lax temp4
     ; clc
     txa
     ifnconst vertical_reflect
         sbx #256-pfwidth
     else
         sbx #256-pfwidth/2
     endif

     bmi lastkernelline

     ifconst PFcolorandheight
         ifconst pfres
             ldy playfieldcolorandheight-131+pfres*pfwidth,x
         else
             ldy playfieldcolorandheight-87,x
         endif
         ifnconst backgroundchange
             sty COLUPF
         else
             sty COLUBK
         endif
         ifconst pfres
             lda playfieldcolorandheight-132+pfres*pfwidth,x
         else
             lda playfieldcolorandheight-88,x
         endif
         sta.w temp1
     endif
     ifconst PFheights
         lsr
         lsr
         tay
         lda (pfheighttable),y
         sta.w temp1
     endif
     ifconst PFcolors
         tay
         lda (pfcolortable),y
         ifnconst backgroundchange
             sta COLUPF
         else
             sta COLUBK
         endif
         ifconst pfrowheight
             lda #pfrowheight
         else
             ifnconst pfres
                 lda #8
             else
                 lda #(96/pfres) ; try to come close to the real size
             endif
         endif
         sta temp1
     endif
     ifnconst PFcolorandheight
         ifnconst PFcolors
             ifnconst PFheights
                 ifnconst no_blank_lines
                     ; read paddle 0
                     ; lo-res paddle read
                     ; bit INPT0
                     ; bmi paddleskipread
                     ; inc paddle0
                     ;donepaddleskip
                     sleep 10
                     ifconst pfrowheight
                         lda #pfrowheight
                     else
                         ifnconst pfres
                             lda #8
                         else
                             lda #(96/pfres) ; try to come close to the real size
                         endif
                     endif
                     sta temp1
                 endif
             endif
         endif
     endif
     

     lda ballheight
     dcp bally
     sbc temp4


     jmp goback


     ifnconst no_blank_lines
lastkernelline
         ifnconst PFcolors
             sleep 10
         else
             ldy #124
             lda (pfcolortable),y
             sta COLUPF
         endif

         ifconst PFheights
             ldx #1
             ;sleep 4
             sleep 3 ; REVENG - this was over 1 cycle
         else
             ldx playfieldpos
             ;sleep 3
             sleep 2 ; REVENG - this was over 1 cycle
         endif

         jmp enterlastkernel

     else
lastkernelline
         
         ifconst PFheights
             ldx #1
             ;sleep 5
             sleep 4 ; REVENG - this was over 1 cycle
         else
             ldx playfieldpos
             ;sleep 4
             sleep 3 ; REVENG - this was over 1 cycle
         endif

         cpx #0
         bne .enterfromNBL
         jmp no_blank_lines_bailout
     endif

     if ((<*)>$d5)
         align 256
     endif
     ; this is a kludge to prevent page wrapping - fix!!!

.skipDrawlastP1
     lda #0
     tay ; REVENG - added so we don't cross a page
     jmp .continuelastP1

.endkerloop     ; enter at cycle 59??
     
     nop

.enterfromNBL
     ifconst pfres
         ldy.w playfield+pfres*pfwidth-4
         sty PF1L ;3
         ldy.w playfield+pfres*pfwidth-3-pfadjust
         sty PF2L ;3
         ldy.w playfield+pfres*pfwidth-1
         sty PF1R ; possibly too early?
         ldy.w playfield+pfres*pfwidth-2-pfadjust
         sty PF2R ;3
     else
         ldy.w playfield-48+pfwidth*12+44
         sty PF1L ;3
         ldy.w playfield-48+pfwidth*12+45-pfadjust
         sty PF2L ;3
         ldy.w playfield-48+pfwidth*12+47
         sty PF1R ; possibly too early?
         ldy.w playfield-48+pfwidth*12+46-pfadjust
         sty PF2R ;3
     endif

enterlastkernel
     lda ballheight

     ; tya
     dcp bally
     ; sleep 4

     ; sbc stack3
     rol
     rol
     sta ENABL 

     lda player1height ;3
     dcp player1y ;5
     bcc .skipDrawlastP1
     ldy player1y ;3
     lda (player1pointer),y ;5; player0pointer must be selected carefully by the compiler
     ; so it doesn't cross a page boundary!

.continuelastP1
     sta GRP1 ;3

     ifnconst player1colors
         lda missile1height ;3
         dcp missile1y ;5
     else
         lda (player1color),y
         sta COLUP1
     endif

     dex
     ;dec temp4 ; might try putting this above PF writes
     beq endkernel


     ifconst pfres
         ldy.w playfield+pfres*pfwidth-4
         sty PF1L ;3
         ldy.w playfield+pfres*pfwidth-3-pfadjust
         sty PF2L ;3
         ldy.w playfield+pfres*pfwidth-1
         sty PF1R ; possibly too early?
         ldy.w playfield+pfres*pfwidth-2-pfadjust
         sty PF2R ;3
     else
         ldy.w playfield-48+pfwidth*12+44
         sty PF1L ;3
         ldy.w playfield-48+pfwidth*12+45-pfadjust
         sty PF2L ;3
         ldy.w playfield-48+pfwidth*12+47
         sty PF1R ; possibly too early?
         ldy.w playfield-48+pfwidth*12+46-pfadjust
         sty PF2R ;3
     endif

     ifnconst player1colors
         rol;2
         rol;2
         sta ENAM1 ;3
     else
         ifnconst playercolors
             sleep 7
         else
             lda.w player0colorstore
             sta COLUP0
         endif
     endif
     
     lda.w player0height
     dcp player0y
     bcc .skipDrawlastP0
     ldy player0y
     lda (player0pointer),y
.continuelastP0
     sta GRP0



     ifnconst no_blank_lines
         lda missile0height ;3
         dcp missile0y ;5
         sbc stack1
         sta ENAM0 ;3
         jmp .endkerloop
     else
         ifconst readpaddle
             ldy currentpaddle
             lda INPT0,y
             bpl noreadpaddle2
             inc paddle
             jmp .endkerloop
noreadpaddle2
             sleep 4
             jmp .endkerloop
         else ; no_blank_lines and no paddle reading
             pla
             pha ; 14 cycles in 4 bytes
             pla
             pha
             ; sleep 14
             jmp .endkerloop
         endif
     endif


     ; ifconst donepaddleskip
         ;paddleskipread
         ; this is kind of lame, since it requires 4 cycles from a page boundary crossing
         ; plus we get a lo-res paddle read
         ; bmi donepaddleskip
     ; endif

.skipDrawlastP0
     lda #0
     tay
     jmp .continuelastP0

     ifconst no_blank_lines
no_blank_lines_bailout
         ldx #0
     endif

endkernel
     ; 6 digit score routine
     stx PF1
     stx PF2
     stx PF0
     clc

     ifconst pfrowheight
         lda #pfrowheight+2
     else
         ifnconst pfres
             lda #10
         else
             lda #(96/pfres)+2 ; try to come close to the real size
         endif
     endif

     sbc playfieldpos
     sta playfieldpos
     txa

     ifconst shakescreen
         bit shakescreen
         bmi noshakescreen2
         ldx #$3D
noshakescreen2
     endif

     sta WSYNC,x

     ; STA WSYNC ;first one, need one more
     sta REFP0
     sta REFP1
     STA GRP0
     STA GRP1
     ; STA PF1
     ; STA PF2
     sta HMCLR
     sta ENAM0
     sta ENAM1
     sta ENABL

     lda temp2 ;restore variables that were obliterated by kernel
     sta player0y
     lda temp3
     sta player1y
     ifnconst player1colors
         lda temp6
         sta missile1y
     endif
     ifnconst playercolors
         ifnconst readpaddle
             lda temp5
             sta missile0y
         endif
     endif
     lda stack2
     sta bally

     ; REVENG - strangely, this isn't required any more. might have
     ; resulted from the no_blank_lines score bounce fix
     ;ifconst no_blank_lines
         ;sta WSYNC
     ;endif

     lda INTIM
     clc
     ifnconst vblank_time
         adc #43+12+87
     else
         adc #vblank_time+12+87

     endif
     ; sta WSYNC
     sta TIM64T

     ifconst minikernel
         jsr minikernel
     endif

     ; now reassign temp vars for score pointers

     ; score pointers contain:
     ; score1-5: lo1,lo2,lo3,lo4,lo5,lo6
     ; swap lo2->temp1
     ; swap lo4->temp3
     ; swap lo6->temp5
     ifnconst noscore
         lda scorepointers+1
         ; ldy temp1
         sta temp1
         ; sty scorepointers+1

         lda scorepointers+3
         ; ldy temp3
         sta temp3
         ; sty scorepointers+3


         sta HMCLR
         tsx
         stx stack1 
         ldx #$E0
         stx HMP0

         LDA scorecolor 
         STA COLUP0
         STA COLUP1
         ifconst scorefade
             STA stack2
         endif
         ifconst pfscore
             lda pfscorecolor
             sta COLUPF
         endif
         sta WSYNC
         ldx #0
         STx GRP0
         STx GRP1 ; seems to be needed because of vdel

         lda scorepointers+5
         ; ldy temp5
         sta temp5,x
         ; sty scorepointers+5
         lda #>scoretable
         sta scorepointers+1
         sta scorepointers+3
         sta scorepointers+5
         sta temp2
         sta temp4
         sta temp6
         LDY #7
         STY VDELP0
         STA RESP0
         STA RESP1


         LDA #$03
         STA NUSIZ0
         STA NUSIZ1
         STA VDELP1
         LDA #$F0
         STA HMP1
         lda (scorepointers),y
         sta GRP0
         STA HMOVE ; cycle 73 ?
         jmp beginscore


         if ((<*)>$d4)
             align 256 ; kludge that potentially wastes space! should be fixed!
         endif

loop2
         lda (scorepointers),y ;+5 68 204
         sta GRP0 ;+3 71 213 D1 -- -- --
         ifconst pfscore
             lda.w pfscore1
             sta PF1
         else
             ifconst scorefade
                 sleep 2
                 dec stack2 ; decrement the temporary scorecolor
             else
                 sleep 7
             endif
         endif
         ; cycle 0
beginscore
         lda (scorepointers+$8),y ;+5 5 15
         sta GRP1 ;+3 8 24 D1 D1 D2 --
         lda (scorepointers+$6),y ;+5 13 39
         sta GRP0 ;+3 16 48 D3 D1 D2 D2
         lax (scorepointers+$2),y ;+5 29 87
         txs
         lax (scorepointers+$4),y ;+5 36 108
         ifconst scorefade
             lda stack2
         else
             sleep 3
         endif

         ifconst pfscore
             lda pfscore2
             sta PF1
         else
             ifconst scorefade
                 sta COLUP0
                 sta COLUP1
             else
                 sleep 6
             endif
         endif

         lda (scorepointers+$A),y ;+5 21 63
         stx GRP1 ;+3 44 132 D3 D3 D4 D2!
         tsx
         stx GRP0 ;+3 47 141 D5 D3! D4 D4
         sta GRP1 ;+3 50 150 D5 D5 D6 D4!
         sty GRP0 ;+3 53 159 D4* D5! D6 D6
         dey
         bpl loop2 ;+2 60 180

         ldx stack1 
         txs
         ; lda scorepointers+1
         ldy temp1
         ; sta temp1
         sty scorepointers+1

         LDA #0 
         sta PF1
         STA GRP0
         STA GRP1
         STA VDELP0
         STA VDELP1;do we need these
         STA NUSIZ0
         STA NUSIZ1

         ; lda scorepointers+3
         ldy temp3
         ; sta temp3
         sty scorepointers+3

         ; lda scorepointers+5
         ldy temp5
         ; sta temp5
         sty scorepointers+5
     endif ;noscore
     LDA #%11000010
     sta WSYNC
     STA VBLANK
     RETURN

     ifconst shakescreen
doshakescreen
         bit shakescreen
         bmi noshakescreen
         sta WSYNC
noshakescreen
         ldx missile0height
         inx
         rts
     endif

; playfield drawing routines
; you get a 32x12 bitmapped display in a single color :)
; 0-31 and 0-11

pfclear ; clears playfield - or fill with pattern
 ifconst pfres
 ldx #pfres*pfwidth-1
 else
 ldx #47-(4-pfwidth)*12 ; will this work?
 endif
pfclear_loop
 ifnconst superchip
 sta playfield,x
 else
 sta playfield-128,x
 endif
 dex
 bpl pfclear_loop
 RETURN
 
setuppointers
 stx temp2 ; store on.off.flip value
 tax ; put x-value in x 
 lsr
 lsr
 lsr ; divide x pos by 8 
 sta temp1
 tya
 asl
 if pfwidth=4
  asl ; multiply y pos by 4
 endif ; else multiply by 2
 clc
 adc temp1 ; add them together to get actual memory location offset
 tay ; put the value in y
 lda temp2 ; restore on.off.flip value
 rts

pfread
;x=xvalue, y=yvalue
 jsr setuppointers
 lda setbyte,x
 and playfield,y
 eor setbyte,x
; beq readzero
; lda #1
; readzero
 RETURN

pfpixel
;x=xvalue, y=yvalue, a=0,1,2
 jsr setuppointers

 ifconst bankswitch
 lda temp2 ; load on.off.flip value (0,1, or 2)
 beq pixelon_r  ; if "on" go to on
 lsr
 bcs pixeloff_r ; value is 1 if true
 lda playfield,y ; if here, it's "flip"
 eor setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN
pixelon_r
 lda playfield,y
 ora setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN
pixeloff_r
 lda setbyte,x
 eor #$ff
 and playfield,y
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN

 else
 jmp plotpoint
 endif

pfhline
;x=xvalue, y=yvalue, a=0,1,2, temp3=endx
 jsr setuppointers
 jmp noinc
keepgoing
 inx
 txa
 and #7
 bne noinc
 iny
noinc
 jsr plotpoint
 cpx temp3
 bmi keepgoing
 RETURN

pfvline
;x=xvalue, y=yvalue, a=0,1,2, temp3=endx
 jsr setuppointers
 sty temp1 ; store memory location offset
 inc temp3 ; increase final x by 1 
 lda temp3
 asl
 if pfwidth=4
   asl ; multiply by 4
 endif ; else multiply by 2
 sta temp3 ; store it
 ; Thanks to Michael Rideout for fixing a bug in this code
 ; right now, temp1=y=starting memory location, temp3=final
 ; x should equal original x value
keepgoingy
 jsr plotpoint
 iny
 iny
 if pfwidth=4
   iny
   iny
 endif
 cpy temp3
 bmi keepgoingy
 RETURN

plotpoint
 lda temp2 ; load on.off.flip value (0,1, or 2)
 beq pixelon  ; if "on" go to on
 lsr
 bcs pixeloff ; value is 1 if true
 lda playfield,y ; if here, it's "flip"
 eor setbyte,x
  ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts
pixelon
 lda playfield,y
 ora setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts
pixeloff
 lda setbyte,x
 eor #$ff
 and playfield,y
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts

setbyte
 ifnconst pfcenter
 .byte $80
 .byte $40
 .byte $20
 .byte $10
 .byte $08
 .byte $04
 .byte $02
 .byte $01
 endif
 .byte $01
 .byte $02
 .byte $04
 .byte $08
 .byte $10
 .byte $20
 .byte $40
 .byte $80
 .byte $80
 .byte $40
 .byte $20
 .byte $10
 .byte $08
 .byte $04
 .byte $02
 .byte $01
 .byte $01
 .byte $02
 .byte $04
 .byte $08
 .byte $10
 .byte $20
 .byte $40
 .byte $80
pfscroll ;(a=0 left, 1 right, 2 up, 4 down, 6=upup, 12=downdown)
 bne notleft
;left
 ifconst pfres
 ldx #pfres*4
 else
 ldx #48
 endif
leftloop
 lda playfield-1,x
 lsr

 ifconst superchip
 lda playfield-2,x
 rol
 sta playfield-130,x
 lda playfield-3,x
 ror
 sta playfield-131,x
 lda playfield-4,x
 rol
 sta playfield-132,x
 lda playfield-1,x
 ror
 sta playfield-129,x
 else
 rol playfield-2,x
 ror playfield-3,x
 rol playfield-4,x
 ror playfield-1,x
 endif

 txa
 sbx #4
 bne leftloop
 RETURN

notleft
 lsr
 bcc notright
;right

 ifconst pfres
 ldx #pfres*4
 else
 ldx #48
 endif
rightloop
 lda playfield-4,x
 lsr
 ifconst superchip
 lda playfield-3,x
 rol
 sta playfield-131,x
 lda playfield-2,x
 ror
 sta playfield-130,x
 lda playfield-1,x
 rol
 sta playfield-129,x
 lda playfield-4,x
 ror
 sta playfield-132,x
 else
 rol playfield-3,x
 ror playfield-2,x
 rol playfield-1,x
 ror playfield-4,x
 endif
 txa
 sbx #4
 bne rightloop
  RETURN

notright
 lsr
 bcc notup
;up
 lsr
 bcc onedecup
 dec playfieldpos
onedecup
 dec playfieldpos
 beq shiftdown 
 bpl noshiftdown2 
shiftdown
  ifconst pfrowheight
 lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif

 sta playfieldpos
 lda playfield+3
 sta temp4
 lda playfield+2
 sta temp3
 lda playfield+1
 sta temp2
 lda playfield
 sta temp1
 ldx #0
up2
 lda playfield+4,x
 ifconst superchip
 sta playfield-128,x
 lda playfield+5,x
 sta playfield-127,x
 lda playfield+6,x
 sta playfield-126,x
 lda playfield+7,x
 sta playfield-125,x
 else
 sta playfield,x
 lda playfield+5,x
 sta playfield+1,x
 lda playfield+6,x
 sta playfield+2,x
 lda playfield+7,x
 sta playfield+3,x
 endif
 txa
 sbx #252
 ifconst pfres
 cpx #(pfres-1)*4
 else
 cpx #44
 endif
 bne up2

 lda temp4
 
 ifconst superchip
 ifconst pfres
 sta playfield+pfres*4-129
 lda temp3
 sta playfield+pfres*4-130
 lda temp2
 sta playfield+pfres*4-131
 lda temp1
 sta playfield+pfres*4-132
 else
 sta playfield+47-128
 lda temp3
 sta playfield+46-128
 lda temp2
 sta playfield+45-128
 lda temp1
 sta playfield+44-128
 endif
 else
 ifconst pfres
 sta playfield+pfres*4-1
 lda temp3
 sta playfield+pfres*4-2
 lda temp2
 sta playfield+pfres*4-3
 lda temp1
 sta playfield+pfres*4-4
 else
 sta playfield+47
 lda temp3
 sta playfield+46
 lda temp2
 sta playfield+45
 lda temp1
 sta playfield+44
 endif
 endif
noshiftdown2
 RETURN


notup
;down
 lsr
 bcs oneincup
 inc playfieldpos
oneincup
 inc playfieldpos
 lda playfieldpos

  ifconst pfrowheight
 cmp #pfrowheight+1
 else
 ifnconst pfres
   cmp #9
 else
   cmp #(96/pfres)+1 ; try to come close to the real size
 endif
 endif

 bcc noshiftdown 
 lda #1
 sta playfieldpos

 ifconst pfres
 lda playfield+pfres*4-1
 sta temp4
 lda playfield+pfres*4-2
 sta temp3
 lda playfield+pfres*4-3
 sta temp2
 lda playfield+pfres*4-4
 else
 lda playfield+47
 sta temp4
 lda playfield+46
 sta temp3
 lda playfield+45
 sta temp2
 lda playfield+44
 endif

 sta temp1

 ifconst pfres
 ldx #(pfres-1)*4
 else
 ldx #44
 endif
down2
 lda playfield-1,x
 ifconst superchip
 sta playfield-125,x
 lda playfield-2,x
 sta playfield-126,x
 lda playfield-3,x
 sta playfield-127,x
 lda playfield-4,x
 sta playfield-128,x
 else
 sta playfield+3,x
 lda playfield-2,x
 sta playfield+2,x
 lda playfield-3,x
 sta playfield+1,x
 lda playfield-4,x
 sta playfield,x
 endif
 txa
 sbx #4
 bne down2

 lda temp4
 ifconst superchip
 sta playfield-125
 lda temp3
 sta playfield-126
 lda temp2
 sta playfield-127
 lda temp1
 sta playfield-128
 else
 sta playfield+3
 lda temp3
 sta playfield+2
 lda temp2
 sta playfield+1
 lda temp1
 sta playfield
 endif
noshiftdown
 RETURN
;standard routines needed for pretty much all games
; just the random number generator is left - maybe we should remove this asm file altogether?
; repositioning code and score pointer setup moved to overscan
; read switches, joysticks now compiler generated (more efficient)

randomize
	lda rand
	lsr
 ifconst rand16
	rol rand16
 endif
	bcc noeor
	eor #$B4
noeor
	sta rand
 ifconst rand16
	eor rand16
 endif
	RETURN
drawscreen
     ifconst debugscore
         ldx #14
         lda INTIM ; display # cycles left in the score

         ifconst mincycles
             lda mincycles 
             cmp INTIM
             lda mincycles
             bcc nochange
             lda INTIM
             sta mincycles
nochange
         endif

         ; cmp #$2B
         ; bcs no_cycles_left
         bmi cycles_left
         ldx #64
         eor #$ff ;make negative
cycles_left
         stx scorecolor
         and #$7f ; clear sign bit
         tax
         lda scorebcd,x
         sta score+2
         lda scorebcd1,x
         sta score+1
         jmp done_debugscore 
scorebcd
         .byte $00, $64, $28, $92, $56, $20, $84, $48, $12, $76, $40
         .byte $04, $68, $32, $96, $60, $24, $88, $52, $16, $80, $44
         .byte $08, $72, $36, $00, $64, $28, $92, $56, $20, $84, $48
         .byte $12, $76, $40, $04, $68, $32, $96, $60, $24, $88
scorebcd1
         .byte 0, 0, 1, 1, 2, 3, 3, 4, 5, 5, 6
         .byte 7, 7, 8, 8, 9, $10, $10, $11, $12, $12, $13
         .byte $14, $14, $15, $16, $16, $17, $17, $18, $19, $19, $20
         .byte $21, $21, $22, $23, $23, $24, $24, $25, $26, $26
done_debugscore
     endif

     ifconst debugcycles
         lda INTIM ; if we go over, it mucks up the background color
         ; cmp #$2B
         ; BCC overscan
         bmi overscan
         sta COLUBK
         bcs doneoverscan
     endif

overscan
     ifconst interlaced
         PHP
         PLA 
         EOR #4 ; flip interrupt bit
         PHA
         PLP
         AND #4 ; isolate the interrupt bit
         TAX ; save it for later
     endif

overscanloop
     lda INTIM ;wait for sync
     bmi overscanloop
doneoverscan

     ;do VSYNC

     ifconst interlaced
         CPX #4
         BNE oddframevsync
     endif

     lda #2
     sta WSYNC
     sta VSYNC
     STA WSYNC
     STA WSYNC
     lsr
     STA WSYNC
     STA VSYNC
     sta VBLANK
     ifnconst overscan_time
         lda #37+128
     else
         lda #overscan_time+128
     endif
     sta TIM64T

     ifconst interlaced
         jmp postsync 

oddframevsync
         sta WSYNC

         LDA ($80,X) ; 11 waste
         LDA ($80,X) ; 11 waste
         LDA ($80,X) ; 11 waste

         lda #2
         sta VSYNC
         sta WSYNC
         sta WSYNC
         sta WSYNC

         LDA ($80,X) ; 11 waste
         LDA ($80,X) ; 11 waste
         LDA ($80,X) ; 11 waste

         lda #0
         sta VSYNC
         sta VBLANK
         ifnconst overscan_time
             lda #37+128
         else
             lda #overscan_time+128
         endif
         sta TIM64T

postsync
     endif

     ifconst legacy
         if legacy < 100
             ldx #4
adjustloop
             lda player0x,x
             sec
             sbc #14 ;?
             sta player0x,x
             dex
             bpl adjustloop
         endif
     endif
     if ((<*)>$e9)&&((<*)<$fa)
         repeat ($fa-(<*))
         nop
         repend
     endif
     sta WSYNC
     ldx #4
     SLEEP 3
HorPosLoop     ; 5
     lda player0x,X ;+4 9
     sec ;+2 11
DivideLoop
     sbc #15
     bcs DivideLoop;+4 15
     sta temp1,X ;+4 19
     sta RESP0,X ;+4 23
     sta WSYNC
     dex
     bpl HorPosLoop;+5 5
     ; 4

     ldx #4
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 18

     dex
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 32

     dex
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 46

     dex
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 60

     dex
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 74

     sta WSYNC
     
     sta HMOVE ;+3 3


     ifconst legacy
         if legacy < 100
             ldx #4
adjustloop2
             lda player0x,x
             clc
             adc #14 ;?
             sta player0x,x
             dex
             bpl adjustloop2
         endif
     endif




     ;set score pointers
     lax score+2
     jsr scorepointerset
     sty scorepointers+5
     stx scorepointers+2
     lax score+1
     jsr scorepointerset
     sty scorepointers+4
     stx scorepointers+1
     lax score
     jsr scorepointerset
     sty scorepointers+3
     stx scorepointers

vblk
     ; run possible vblank bB code
     ifconst vblank_bB_code
         jsr vblank_bB_code
     endif
vblk2
     LDA INTIM
     bmi vblk2
     jmp kernel
     

     .byte $80,$70,$60,$50,$40,$30,$20,$10,$00
     .byte $F0,$E0,$D0,$C0,$B0,$A0,$90
repostable

scorepointerset
     and #$0F
     asl
     asl
     asl
     adc #<scoretable
     tay 
     txa
     ; and #$F0
     ; lsr
     asr #$F0
     adc #<scoretable
     tax
     rts
game
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L00 ;  set smartbranching on

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L01 ;  set optimization noinlinedata

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L02 ;  dim _error_accumulator  =  a

.L03 ;  dim _delta_y  =  b

.L04 ;  dim _delta_x  =  c

.L05 ;  dim _octant  =  d

.L06 ;  dim _Chase_Delay  =  e

.
 ; 

.
 ; 

.
 ; 

.L07 ;  dim _BitOp_P1_P0_Dir  =  f

.L08 ;  dim _Bit0_P1_Dir_Up  =  f

.L09 ;  dim _Bit1_P1_Dir_Down  =  f

.L010 ;  dim _Bit2_P1_Dir_Left  =  f

.L011 ;  dim _Bit3_P1_Dir_Right  =  f

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L012 ;  dim _Bit0_P0_Col_Up  =  g

.L013 ;  dim _Bit1_P0_Col_Down  =  g

.L014 ;  dim _Bit2_P0_Col_Left  =  g

.L015 ;  dim _Bit3_P0_Col_Right  =  g

.L016 ;  dim _Bit4_P0_Dir_Up  =  g

.L017 ;  dim _Bit5_P0_Dir_Down  =  g

.L018 ;  dim _Bit6_P0_Dir_Left  =  g

.L019 ;  dim _Bit7_P0_Dir_Right  =  g

.
 ; 

.
 ; 

.
 ; 

.L020 ;  dim _Bit0_Reset_Restrainer  =  y

.L021 ;  dim _Bit5_EA  =  y

.L022 ;  dim _BitOp_01  =  y

.L023 ;  dim _Bit4_Toggle_Screen  =  y

.L024 ;  dim _Bit7_M0_Moving  =  y

.
 ; 

.
 ; 

.
 ; 

.L025 ;  dim rand16  =  z

.
 ; 

.L026 ;  dim _P0_TEMPX  =  h

.L027 ;  dim _P0_TEMPY  =  i

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L028 ;  const _P_Edge_Top  =  9

.L029 ;  const _P_Edge_Bottom  =  88

.L030 ;  const _P_Edge_Left  =  1

.L031 ;  const _P_Edge_Right  =  153

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L032 ;  const noscore  =  1

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.__Start_Restart
 ; __Start_Restart

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L033 ;  AUDV0  =  0  :  AUDV1  =  0

	LDA #0
	STA AUDV0
	STA AUDV1
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L034 ;  a  =  0  :  b  =  0  :  c  =  0  :  d  =  0  :  e  =  0  :  f  =  0  :  g  =  0  :  h  =  0  :  i  =  0

	LDA #0
	STA a
	STA b
	STA c
	STA d
	STA e
	STA f
	STA g
	STA h
	STA i
.L035 ;  j  =  0  :  k  =  0  :  l  =  0  :  m  =  0  :  n  =  0  :  o  =  0  :  p  =  0  :  q  =  0  :  r  =  0

	LDA #0
	STA j
	STA k
	STA l
	STA m
	STA n
	STA o
	STA p
	STA q
	STA r
.L036 ;  s  =  0  :  t  =  0  :  u  =  0  :  v  =  0  :  w  =  0  :  x  =  0  :  y  =  0

	LDA #0
	STA s
	STA t
	STA u
	STA v
	STA w
	STA x
	STA y
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L037 ;  _BitOp_01  =  _BitOp_01  &  %00010000

	LDA _BitOp_01
	AND #%00010000
	STA _BitOp_01
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L038 ;  player1x  =  80  :  player1y  =  53

	LDA #80
	STA player1x
	LDA #53
	STA player1y
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L039 ;  player0x  =   ( rand / 2 )   +   ( rand & 15 )   +  5  :  player0y  =  9

; complex statement detected
 jsr randomize
	lsr
	PHA
 jsr randomize
	AND #15
	TSX
	INX
	TXS
	CLC
	ADC $00,x
	CLC
	ADC #5
	STA player0x
	LDA #9
	STA player0y
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L040 ;  COLUPF  =  $FC

	LDA #$FC
	STA COLUPF
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L041 ;  COLUBK  =  0

	LDA #0
	STA COLUBK
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L042 ;  _Bit3_P1_Dir_Right{3}  =  1

	LDA _Bit3_P1_Dir_Right
	ORA #8
	STA _Bit3_P1_Dir_Right
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L043 ;  _Bit0_Reset_Restrainer{0}  =  1

	LDA _Bit0_Reset_Restrainer
	ORA #1
	STA _Bit0_Reset_Restrainer
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L044 ;  playfield:

  ifconst pfres
	  ldx #(11>pfres)*(pfres*pfwidth-1)+(11<=pfres)*43
  else
	  ldx #((11*pfwidth-1)*((11*pfwidth-1)<47))+(47*((11*pfwidth-1)>=47))
  endif
	jmp pflabel0
PF_data0
	.byte %11111111, %11111111
	if (pfwidth>2)
	.byte %11111111, %11111111
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %10011111, %01100001
	if (pfwidth>2)
	.byte %01100001, %10011111
 endif
	.byte %10010000, %01110000
	if (pfwidth>2)
	.byte %01110000, %10010000
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %10011001, %10011001
	if (pfwidth>2)
	.byte %10011001, %10011001
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %10010000, %01110000
	if (pfwidth>2)
	.byte %01110000, %10010000
 endif
	.byte %10011111, %01100001
	if (pfwidth>2)
	.byte %01100001, %10011111
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %11111111, %11111111
	if (pfwidth>2)
	.byte %11111111, %11111111
 endif
pflabel0
	lda PF_data0,x
	sta playfield,x
	dex
	bpl pflabel0
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L045 ;  player1:

	LDX #<playerL045_1
	STX player1pointerlo
	LDA #>playerL045_1
	STA player1pointerhi
	LDA #7
	STA player1height
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L046 ;  player0:

	LDX #<playerL046_0
	STX player0pointerlo
	LDA #>playerL046_0
	STA player0pointerhi
	LDA #7
	STA player0height
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L047 ;  goto __Chase_Setup

 jmp .__Chase_Setup

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.__Main_Loop
 ; __Main_Loop

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L048 ;  _Chase_Delay  =  _Chase_Delay  -  1

	DEC _Chase_Delay
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L049 ;  if !joy0up  &&  !joy0down  &&  !joy0left  &&  !joy0right then goto __Skip_Joystick_Precheck

 lda #$10
 bit SWCHA
	BEQ .skipL049
.condpart0
 lda #$20
 bit SWCHA
	BEQ .skip0then
.condpart1
 bit SWCHA
	BVC .skip1then
.condpart2
 bit SWCHA
	BPL .skip2then
.condpart3
 jmp .__Skip_Joystick_Precheck

.skip2then
.skip1then
.skip0then
.skipL049
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L050 ;  _BitOp_P1_P0_Dir  =  _BitOp_P1_P0_Dir  &  %11110000

	LDA _BitOp_P1_P0_Dir
	AND #%11110000
	STA _BitOp_P1_P0_Dir
.
 ; 

.__Skip_Joystick_Precheck
 ; __Skip_Joystick_Precheck

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L051 ;  if !joy0up then goto __Skip_Joy0_Up

 lda #$10
 bit SWCHA
	BEQ .skipL051
.condpart4
 jmp .__Skip_Joy0_Up

.skipL051
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L052 ;  _Bit0_P1_Dir_Up{0}  =  1

	LDA _Bit0_P1_Dir_Up
	ORA #1
	STA _Bit0_P1_Dir_Up
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L053 ;  if player1y  <=  _P_Edge_Top then goto __Skip_Joy0_Up

	LDA #_P_Edge_Top
	CMP player1y
     BCC .skipL053
.condpart5
 jmp .__Skip_Joy0_Up

.skipL053
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L054 ;  temp5  =   ( player1x - 10 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #10
	lsr
	lsr
	STA temp5
.
 ; 

.L055 ;  temp6  =   ( player1y - 9 )  / 8

; complex statement detected
	LDA player1y
	SEC
	SBC #9
	lsr
	lsr
	lsr
	STA temp6
.
 ; 

.L056 ;  if temp5  <  34 then if pfread ( temp5 , temp6 )  then goto __Skip_Joy0_Up

	LDA temp5
	CMP #34
     BCS .skipL056
.condpart6
	LDA temp5
	LDY temp6
 jsr pfread
	BNE .skip6then
.condpart7
 jmp .__Skip_Joy0_Up

.skip6then
.skipL056
.
 ; 

.L057 ;  temp4  =   ( player1x - 17 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #17
	lsr
	lsr
	STA temp4
.
 ; 

.L058 ;  if temp4  <  34 then if pfread ( temp4 , temp6 )  then goto __Skip_Joy0_Up

	LDA temp4
	CMP #34
     BCS .skipL058
.condpart8
	LDA temp4
	LDY temp6
 jsr pfread
	BNE .skip8then
.condpart9
 jmp .__Skip_Joy0_Up

.skip8then
.skipL058
.
 ; 

.L059 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L060 ;  if temp3  <  34 then if pfread ( temp3 , temp6 )  then goto __Skip_Joy0_Up

	LDA temp3
	CMP #34
     BCS .skipL060
.condpart10
	LDA temp3
	LDY temp6
 jsr pfread
	BNE .skip10then
.condpart11
 jmp .__Skip_Joy0_Up

.skip10then
.skipL060
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L061 ;  player1y  =  player1y  -  1

	DEC player1y
.
 ; 

.__Skip_Joy0_Up
 ; __Skip_Joy0_Up

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L062 ;  if !joy0down then goto __Skip_Joy0_Down

 lda #$20
 bit SWCHA
	BEQ .skipL062
.condpart12
 jmp .__Skip_Joy0_Down

.skipL062
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L063 ;  _Bit1_P1_Dir_Down{1}  =  1

	LDA _Bit1_P1_Dir_Down
	ORA #2
	STA _Bit1_P1_Dir_Down
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L064 ;  if player1y  >=  _P_Edge_Bottom then goto __Skip_Joy0_Down

	LDA player1y
	CMP #_P_Edge_Bottom
     BCC .skipL064
.condpart13
 jmp .__Skip_Joy0_Down

.skipL064
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L065 ;  temp5  =   ( player1x - 10 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #10
	lsr
	lsr
	STA temp5
.
 ; 

.L066 ;  temp6  =   ( player1y )  / 8

; complex statement detected
	LDA player1y
	lsr
	lsr
	lsr
	STA temp6
.
 ; 

.L067 ;  if temp5  <  34 then if pfread ( temp5 , temp6 )  then goto __Skip_Joy0_Down

	LDA temp5
	CMP #34
     BCS .skipL067
.condpart14
	LDA temp5
	LDY temp6
 jsr pfread
	BNE .skip14then
.condpart15
 jmp .__Skip_Joy0_Down

.skip14then
.skipL067
.
 ; 

.L068 ;  temp4  =   ( player1x - 17 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #17
	lsr
	lsr
	STA temp4
.
 ; 

.L069 ;  if temp4  <  34 then if pfread ( temp4 , temp6 )  then goto __Skip_Joy0_Down

	LDA temp4
	CMP #34
     BCS .skipL069
.condpart16
	LDA temp4
	LDY temp6
 jsr pfread
	BNE .skip16then
.condpart17
 jmp .__Skip_Joy0_Down

.skip16then
.skipL069
.
 ; 

.L070 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L071 ;  if temp3  <  34 then if pfread ( temp3 , temp6 )  then goto __Skip_Joy0_Down

	LDA temp3
	CMP #34
     BCS .skipL071
.condpart18
	LDA temp3
	LDY temp6
 jsr pfread
	BNE .skip18then
.condpart19
 jmp .__Skip_Joy0_Down

.skip18then
.skipL071
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L072 ;  player1y  =  player1y  +  1

	INC player1y
.
 ; 

.__Skip_Joy0_Down
 ; __Skip_Joy0_Down

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L073 ;  if !joy0left then goto __Skip_Joy0_Left

 bit SWCHA
	BVC .skipL073
.condpart20
 jmp .__Skip_Joy0_Left

.skipL073
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L074 ;  _Bit2_P1_Dir_Left{2}  =  1

	LDA _Bit2_P1_Dir_Left
	ORA #4
	STA _Bit2_P1_Dir_Left
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L075 ;  if player1x  <=  _P_Edge_Left then goto __Skip_Joy0_Left

	LDA #_P_Edge_Left
	CMP player1x
     BCC .skipL075
.condpart21
 jmp .__Skip_Joy0_Left

.skipL075
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L076 ;  temp5  =   ( player1y - 1 )  / 8

; complex statement detected
	LDA player1y
	SEC
	SBC #1
	lsr
	lsr
	lsr
	STA temp5
.
 ; 

.L077 ;  temp6  =   ( player1x - 18 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #18
	lsr
	lsr
	STA temp6
.
 ; 

.L078 ;  if temp6  <  34 then if pfread ( temp6 , temp5 )  then goto __Skip_Joy0_Left

	LDA temp6
	CMP #34
     BCS .skipL078
.condpart22
	LDA temp6
	LDY temp5
 jsr pfread
	BNE .skip22then
.condpart23
 jmp .__Skip_Joy0_Left

.skip22then
.skipL078
.
 ; 

.L079 ;  temp3  =   ( player1y - 8 )  / 8

; complex statement detected
	LDA player1y
	SEC
	SBC #8
	lsr
	lsr
	lsr
	STA temp3
.
 ; 

.L080 ;  if temp6  <  34 then if pfread ( temp6 , temp3 )  then goto __Skip_Joy0_Left

	LDA temp6
	CMP #34
     BCS .skipL080
.condpart24
	LDA temp6
	LDY temp3
 jsr pfread
	BNE .skip24then
.condpart25
 jmp .__Skip_Joy0_Left

.skip24then
.skipL080
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L081 ;  player1x  =  player1x  -  1

	DEC player1x
.
 ; 

.__Skip_Joy0_Left
 ; __Skip_Joy0_Left

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L082 ;  if !joy0right then goto __Skip_Joy0_Right

 bit SWCHA
	BPL .skipL082
.condpart26
 jmp .__Skip_Joy0_Right

.skipL082
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L083 ;  _Bit3_P1_Dir_Right{3}  =  1

	LDA _Bit3_P1_Dir_Right
	ORA #8
	STA _Bit3_P1_Dir_Right
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L084 ;  if player1x  >=  _P_Edge_Right then goto __Skip_Joy0_Right

	LDA player1x
	CMP #_P_Edge_Right
     BCC .skipL084
.condpart27
 jmp .__Skip_Joy0_Right

.skipL084
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L085 ;  temp5  =   ( player1y - 1 )  / 8

; complex statement detected
	LDA player1y
	SEC
	SBC #1
	lsr
	lsr
	lsr
	STA temp5
.
 ; 

.L086 ;  temp6  =   ( player1x - 9 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #9
	lsr
	lsr
	STA temp6
.
 ; 

.L087 ;  if temp6  <  34 then if pfread ( temp6 , temp5 )  then goto __Skip_Joy0_Right

	LDA temp6
	CMP #34
     BCS .skipL087
.condpart28
	LDA temp6
	LDY temp5
 jsr pfread
	BNE .skip28then
.condpart29
 jmp .__Skip_Joy0_Right

.skip28then
.skipL087
.
 ; 

.L088 ;  temp3  =   ( player1y - 8 )  / 8

; complex statement detected
	LDA player1y
	SEC
	SBC #8
	lsr
	lsr
	lsr
	STA temp3
.
 ; 

.L089 ;  if temp6  <  34 then if pfread ( temp6 , temp3 )  then goto __Skip_Joy0_Right

	LDA temp6
	CMP #34
     BCS .skipL089
.condpart30
	LDA temp6
	LDY temp3
 jsr pfread
	BNE .skip30then
.condpart31
 jmp .__Skip_Joy0_Right

.skip30then
.skipL089
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L090 ;  player1x  =  player1x  +  1

	INC player1x
.
 ; 

.__Skip_Joy0_Right
 ; __Skip_Joy0_Right

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L091 ;  if !collision(player0,player1) then goto __Skip_p0_p1_Collision

	bit 	CXPPMM
	BMI .skipL091
.condpart32
 jmp .__Skip_p0_p1_Collision

.skipL091
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L092 ;  player0x  =  100

	LDA #100
	STA player0x
.L093 ;  player0y  =  50

	LDA #50
	STA player0y
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L094 ;  if player1y  <=  48 then player0y  =  88

	LDA #48
	CMP player1y
     BCC .skipL094
.condpart33
	LDA #88
	STA player0y
.skipL094
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L095 ;  _Bit5_EA{5}  =  1

	LDA _Bit5_EA
	ORA #32
	STA _Bit5_EA
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L096 ;  goto __Chase_Setup

 jmp .__Chase_Setup

.
 ; 

.__Skip_p0_p1_Collision
 ; __Skip_p0_p1_Collision

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L097 ;  if !joy0fire  &&  ! ( _Chase_Delay  &  %00000001 )  then goto __Skip_Chase2

 bit INPT4
	BPL .skipL097
.condpart34
; complex statement detected
	LDA _Chase_Delay
	AND #%00000001
	BNE .skip34then
.condpart35
 jmp .__Skip_Chase2

.skip34then
.skipL097
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L098 ;  temp1  =  _error_accumulator

	LDA _error_accumulator
	STA temp1
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L099 ;  if _octant{0} then goto __Skip_Chase1

	LDA _octant
	LSR
	BCC .skipL099
.condpart36
 jmp .__Skip_Chase1

.skipL099
.
 ; 

.__Reenter_Chase1
 ; __Reenter_Chase1

.L0100 ;  _P0_TEMPY  =  player0y  +  _Data_yinc[_octant]

	LDA player0y
	LDX _octant
	CLC
	ADC _Data_yinc,x
	STA _P0_TEMPY
.L0101 ;  if _P0_TEMPY  <  0  &&  _Bit0_P0_Col_Up{0} then _Bit0_P0_Col_Up{0} = 0  :  goto __Skip_Chase1

	LDA _P0_TEMPY
	CMP #0
     BCS .skipL0101
.condpart37
	LDA _Bit0_P0_Col_Up
	LSR
	BCC .skip37then
.condpart38
	LDA _Bit0_P0_Col_Up
	AND #254
	STA _Bit0_P0_Col_Up
 jmp .__Skip_Chase1

.skip37then
.skipL0101
.L0102 ;  if _P0_TEMPY  >  0  &&  _Bit1_P0_Col_Down{1} then _Bit1_P0_Col_Down{1} = 0  :  goto __Skip_Chase1

	LDA #0
	CMP _P0_TEMPY
     BCS .skipL0102
.condpart39
	LDA _Bit1_P0_Col_Down
	AND #2
	BEQ .skip39then
.condpart40
	LDA _Bit1_P0_Col_Down
	AND #253
	STA _Bit1_P0_Col_Down
 jmp .__Skip_Chase1

.skip39then
.skipL0102
.L0103 ;  _error_accumulator  =  _error_accumulator  -  _delta_x

	LDA _error_accumulator
	SEC
	SBC _delta_x
	STA _error_accumulator
.
 ; 

.L0104 ;  if temp1  <  _error_accumulator then _error_accumulator  =  _error_accumulator  +  _delta_y  :  player0x  =  player0x  +  _Data_xinc[_octant]

	LDA temp1
	CMP _error_accumulator
     BCS .skipL0104
.condpart41
	LDA _error_accumulator
	CLC
	ADC _delta_y
	STA _error_accumulator
	LDA player0x
	LDX _octant
	CLC
	ADC _Data_xinc,x
	STA player0x
.skipL0104
.L0105 ;  goto __Skip_Chase2

 jmp .__Skip_Chase2

.
 ; 

.__Skip_Chase1
 ; __Skip_Chase1

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0106 ;  _P0_TEMPX  =  player0x  +  _Data_xinc[_octant]

	LDA player0x
	LDX _octant
	CLC
	ADC _Data_xinc,x
	STA _P0_TEMPX
.L0107 ;  if _P0_TEMPX  <  0  &&  _Bit2_P0_Col_Left{0} then _Bit2_P0_Col_Left{0} = 0  :  goto __Reenter_Chase1

	LDA _P0_TEMPX
	CMP #0
     BCS .skipL0107
.condpart42
	LDA _Bit2_P0_Col_Left
	LSR
	BCC .skip42then
.condpart43
	LDA _Bit2_P0_Col_Left
	AND #254
	STA _Bit2_P0_Col_Left
 jmp .__Reenter_Chase1

.skip42then
.skipL0107
.L0108 ;  if _P0_TEMPX  >  0  &&  _Bit3_P0_Col_Right{1} then _Bit3_P0_Col_Right{1} = 0  :  goto __Reenter_Chase1

	LDA #0
	CMP _P0_TEMPX
     BCS .skipL0108
.condpart44
	LDA _Bit3_P0_Col_Right
	AND #2
	BEQ .skip44then
.condpart45
	LDA _Bit3_P0_Col_Right
	AND #253
	STA _Bit3_P0_Col_Right
 jmp .__Reenter_Chase1

.skip44then
.skipL0108
.L0109 ;  _error_accumulator  =  _error_accumulator  -  _delta_y

	LDA _error_accumulator
	SEC
	SBC _delta_y
	STA _error_accumulator
.
 ; 

.L0110 ;  if temp1  <  _error_accumulator then _error_accumulator  =  _error_accumulator  +  _delta_x  :  _P0_TEMPY  =  player0y  +  _Data_yinc[_octant]

	LDA temp1
	CMP _error_accumulator
     BCS .skipL0110
.condpart46
	LDA _error_accumulator
	CLC
	ADC _delta_x
	STA _error_accumulator
	LDA player0y
	LDX _octant
	CLC
	ADC _Data_yinc,x
	STA _P0_TEMPY
.skipL0110
.
 ; 

.__Skip_Chase2
 ; __Skip_Chase2

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0111 ;  if _P0_TEMPY  >=  player0y then __Skip_P0_Up

	LDA _P0_TEMPY
	CMP player0y
 if ((* - .__Skip_P0_Up) < 127) && ((* - .__Skip_P0_Up) > -128)
	bcs .__Skip_P0_Up
 else
	bcc .0skip__Skip_P0_Up
	jmp .__Skip_P0_Up
.0skip__Skip_P0_Up
 endif
.
 ; 

.
 ; 

.
 ; 

.L0112 ;  _Bit4_P0_Dir_Up{0}  =  1

	LDA _Bit4_P0_Dir_Up
	ORA #1
	STA _Bit4_P0_Dir_Up
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0113 ;  if player0y  <=  _P_Edge_Top then _Bit0_P0_Col_Up{0} = 1  :  goto __Skip_P0_Up

	LDA #_P_Edge_Top
	CMP player0y
     BCC .skipL0113
.condpart47
	LDA _Bit0_P0_Col_Up
	ORA #1
	STA _Bit0_P0_Col_Up
 jmp .__Skip_P0_Up

.skipL0113
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0114 ;  temp5  =   ( player0x - 10 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #10
	lsr
	lsr
	STA temp5
.
 ; 

.L0115 ;  temp6  =   ( player0y - 9 )  / 8

; complex statement detected
	LDA player0y
	SEC
	SBC #9
	lsr
	lsr
	lsr
	STA temp6
.
 ; 

.L0116 ;  if temp5  <  34 then if pfread ( temp5 , temp6 )  then _Bit0_P0_Col_Up{0} = 1  :  goto __Skip_P0_Up

	LDA temp5
	CMP #34
     BCS .skipL0116
.condpart48
	LDA temp5
	LDY temp6
 jsr pfread
	BNE .skip48then
.condpart49
	LDA _Bit0_P0_Col_Up
	ORA #1
	STA _Bit0_P0_Col_Up
 jmp .__Skip_P0_Up

.skip48then
.skipL0116
.
 ; 

.L0117 ;  temp4  =   ( player0x - 17 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #17
	lsr
	lsr
	STA temp4
.
 ; 

.L0118 ;  if temp4  <  34 then if pfread ( temp4 , temp6 )  then _Bit0_P0_Col_Up{0} = 1  :  goto __Skip_P0_Up

	LDA temp4
	CMP #34
     BCS .skipL0118
.condpart50
	LDA temp4
	LDY temp6
 jsr pfread
	BNE .skip50then
.condpart51
	LDA _Bit0_P0_Col_Up
	ORA #1
	STA _Bit0_P0_Col_Up
 jmp .__Skip_P0_Up

.skip50then
.skipL0118
.
 ; 

.L0119 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L0120 ;  if temp3  <  34 then if pfread ( temp3 , temp6 )  then _Bit0_P0_Col_Up{0} = 1  :  goto __Skip_P0_Up

	LDA temp3
	CMP #34
     BCS .skipL0120
.condpart52
	LDA temp3
	LDY temp6
 jsr pfread
	BNE .skip52then
.condpart53
	LDA _Bit0_P0_Col_Up
	ORA #1
	STA _Bit0_P0_Col_Up
 jmp .__Skip_P0_Up

.skip52then
.skipL0120
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0121 ;  player0y  =  _P0_TEMPY

	LDA _P0_TEMPY
	STA player0y
.
 ; 

.__Skip_P0_Up
 ; __Skip_P0_Up

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0122 ;  if _P0_TEMPY  <=  player0y then goto __Skip_P0_Down

	LDA player0y
	CMP _P0_TEMPY
     BCC .skipL0122
.condpart54
 jmp .__Skip_P0_Down

.skipL0122
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0123 ;  _Bit5_P0_Dir_Down{1}  =  1

	LDA _Bit5_P0_Dir_Down
	ORA #2
	STA _Bit5_P0_Dir_Down
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0124 ;  if player0y  >=  _P_Edge_Bottom then _Bit1_P0_Col_Down{1} = 1  :  goto __Skip_P0_Down

	LDA player0y
	CMP #_P_Edge_Bottom
     BCC .skipL0124
.condpart55
	LDA _Bit1_P0_Col_Down
	ORA #2
	STA _Bit1_P0_Col_Down
 jmp .__Skip_P0_Down

.skipL0124
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0125 ;  temp5  =   ( player0x - 10 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #10
	lsr
	lsr
	STA temp5
.
 ; 

.L0126 ;  temp6  =   ( player0y )  / 8

; complex statement detected
	LDA player0y
	lsr
	lsr
	lsr
	STA temp6
.
 ; 

.L0127 ;  if temp5  <  34 then if pfread ( temp5 , temp6 )  then _Bit1_P0_Col_Down{1} = 1  :  goto __Skip_P0_Down

	LDA temp5
	CMP #34
     BCS .skipL0127
.condpart56
	LDA temp5
	LDY temp6
 jsr pfread
	BNE .skip56then
.condpart57
	LDA _Bit1_P0_Col_Down
	ORA #2
	STA _Bit1_P0_Col_Down
 jmp .__Skip_P0_Down

.skip56then
.skipL0127
.
 ; 

.L0128 ;  temp4  =   ( player0x - 17 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #17
	lsr
	lsr
	STA temp4
.
 ; 

.L0129 ;  if temp4  <  34 then if pfread ( temp4 , temp6 )  then _Bit1_P0_Col_Down{1} = 1  :  goto __Skip_P0_Down

	LDA temp4
	CMP #34
     BCS .skipL0129
.condpart58
	LDA temp4
	LDY temp6
 jsr pfread
	BNE .skip58then
.condpart59
	LDA _Bit1_P0_Col_Down
	ORA #2
	STA _Bit1_P0_Col_Down
 jmp .__Skip_P0_Down

.skip58then
.skipL0129
.
 ; 

.L0130 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L0131 ;  if temp3  <  34 then if pfread ( temp3 , temp6 )  then _Bit1_P0_Col_Down{1} = 1  :  goto __Skip_P0_Down

	LDA temp3
	CMP #34
     BCS .skipL0131
.condpart60
	LDA temp3
	LDY temp6
 jsr pfread
	BNE .skip60then
.condpart61
	LDA _Bit1_P0_Col_Down
	ORA #2
	STA _Bit1_P0_Col_Down
 jmp .__Skip_P0_Down

.skip60then
.skipL0131
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0132 ;  player0y  =  _P0_TEMPY

	LDA _P0_TEMPY
	STA player0y
.
 ; 

.__Skip_P0_Down
 ; __Skip_P0_Down

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0133 ;  if _P0_TEMPX  >=  player0x then goto __Skip_P0_Left

	LDA _P0_TEMPX
	CMP player0x
     BCC .skipL0133
.condpart62
 jmp .__Skip_P0_Left

.skipL0133
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0134 ;  _Bit6_P0_Dir_Left{2}  =  1

	LDA _Bit6_P0_Dir_Left
	ORA #4
	STA _Bit6_P0_Dir_Left
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0135 ;  if player0x  <=  _P_Edge_Left then _Bit2_P0_Col_Left{2} = 1  :  goto __Skip_P0_Left

	LDA #_P_Edge_Left
	CMP player0x
     BCC .skipL0135
.condpart63
	LDA _Bit2_P0_Col_Left
	ORA #4
	STA _Bit2_P0_Col_Left
 jmp .__Skip_P0_Left

.skipL0135
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0136 ;  temp5  =   ( player0y - 1 )  / 8

; complex statement detected
	LDA player0y
	SEC
	SBC #1
	lsr
	lsr
	lsr
	STA temp5
.
 ; 

.L0137 ;  temp6  =   ( player0x - 18 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #18
	lsr
	lsr
	STA temp6
.
 ; 

.L0138 ;  if temp6  <  34 then if pfread ( temp6 , temp5 )  then _Bit2_P0_Col_Left{2} = 1  :  goto __Skip_P0_Left

	LDA temp6
	CMP #34
     BCS .skipL0138
.condpart64
	LDA temp6
	LDY temp5
 jsr pfread
	BNE .skip64then
.condpart65
	LDA _Bit2_P0_Col_Left
	ORA #4
	STA _Bit2_P0_Col_Left
 jmp .__Skip_P0_Left

.skip64then
.skipL0138
.
 ; 

.L0139 ;  temp3  =   ( player0y - 8 )  / 8

; complex statement detected
	LDA player0y
	SEC
	SBC #8
	lsr
	lsr
	lsr
	STA temp3
.
 ; 

.L0140 ;  if temp6  <  34 then if pfread ( temp6 , temp3 )  then _Bit2_P0_Col_Left{2} = 1  :  goto __Skip_P0_Left

	LDA temp6
	CMP #34
     BCS .skipL0140
.condpart66
	LDA temp6
	LDY temp3
 jsr pfread
	BNE .skip66then
.condpart67
	LDA _Bit2_P0_Col_Left
	ORA #4
	STA _Bit2_P0_Col_Left
 jmp .__Skip_P0_Left

.skip66then
.skipL0140
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0141 ;  player0x  =  _P0_TEMPX

	LDA _P0_TEMPX
	STA player0x
.
 ; 

.__Skip_P0_Left
 ; __Skip_P0_Left

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0142 ;  if _P0_TEMPX  <=  player0x then goto __Skip_P0_Right

	LDA player0x
	CMP _P0_TEMPX
     BCC .skipL0142
.condpart68
 jmp .__Skip_P0_Right

.skipL0142
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0143 ;  _Bit7_P0_Dir_Right{3}  =  1

	LDA _Bit7_P0_Dir_Right
	ORA #8
	STA _Bit7_P0_Dir_Right
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0144 ;  if player0x  >=  _P_Edge_Right then _Bit3_P0_Col_Right{3} = 1  :  goto __Skip_P0_Right

	LDA player0x
	CMP #_P_Edge_Right
     BCC .skipL0144
.condpart69
	LDA _Bit3_P0_Col_Right
	ORA #8
	STA _Bit3_P0_Col_Right
 jmp .__Skip_P0_Right

.skipL0144
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0145 ;  temp5  =   ( player0y - 1 )  / 8

; complex statement detected
	LDA player0y
	SEC
	SBC #1
	lsr
	lsr
	lsr
	STA temp5
.
 ; 

.L0146 ;  temp6  =   ( player0x - 9 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #9
	lsr
	lsr
	STA temp6
.
 ; 

.L0147 ;  if temp6  <  34 then if pfread ( temp6 , temp5 )  then _Bit3_P0_Col_Right{3} = 1  :  goto __Skip_P0_Right

	LDA temp6
	CMP #34
     BCS .skipL0147
.condpart70
	LDA temp6
	LDY temp5
 jsr pfread
	BNE .skip70then
.condpart71
	LDA _Bit3_P0_Col_Right
	ORA #8
	STA _Bit3_P0_Col_Right
 jmp .__Skip_P0_Right

.skip70then
.skipL0147
.
 ; 

.L0148 ;  temp3  =   ( player0y - 8 )  / 8

; complex statement detected
	LDA player0y
	SEC
	SBC #8
	lsr
	lsr
	lsr
	STA temp3
.
 ; 

.L0149 ;  if temp6  <  34 then if pfread ( temp6 , temp3 )  then _Bit3_P0_Col_Right{3} = 1  :  goto __Skip_P0_Right

	LDA temp6
	CMP #34
     BCS .skipL0149
.condpart72
	LDA temp6
	LDY temp3
 jsr pfread
	BNE .skip72then
.condpart73
	LDA _Bit3_P0_Col_Right
	ORA #8
	STA _Bit3_P0_Col_Right
 jmp .__Skip_P0_Right

.skip72then
.skipL0149
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0150 ;  player0x  =  _P0_TEMPX

	LDA _P0_TEMPX
	STA player0x
.
 ; 

.__Skip_P0_Right
 ; __Skip_P0_Right

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0151 ;  if !joy0up  &&  !joy0down  &&  !joy0left  &&  !joy0right then goto __Skip_Bresenham_Setup

 lda #$10
 bit SWCHA
	BEQ .skipL0151
.condpart74
 lda #$20
 bit SWCHA
	BEQ .skip74then
.condpart75
 bit SWCHA
	BVC .skip75then
.condpart76
 bit SWCHA
	BPL .skip76then
.condpart77
 jmp .__Skip_Bresenham_Setup

.skip76then
.skip75then
.skip74then
.skipL0151
.
 ; 

.__Chase_Setup
 ; __Chase_Setup

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0152 ;  if player0x  <  player1x then _octant{2}  =  1  :  _delta_x  =  player1x  -  player0x else _octant{2}  =  0  :  _delta_x  =  player0x  -  player1x

	LDA player0x
	CMP player1x
     BCS .skipL0152
.condpart78
	LDA _octant
	ORA #4
	STA _octant
	LDA player1x
	SEC
	SBC player0x
	STA _delta_x
 jmp .skipelse0
.skipL0152
	LDA _octant
	AND #251
	STA _octant
	LDA player0x
	SEC
	SBC player1x
	STA _delta_x
.skipelse0
.L0153 ;  if player0y  <  player1y then _octant{1}  =  1  :  _delta_y  =  player1y  -  player0y else _octant{1}  =  0  :  _delta_y  =  player0y  -  player1y

	LDA player0y
	CMP player1y
     BCS .skipL0153
.condpart79
	LDA _octant
	ORA #2
	STA _octant
	LDA player1y
	SEC
	SBC player0y
	STA _delta_y
 jmp .skipelse1
.skipL0153
	LDA _octant
	AND #253
	STA _octant
	LDA player0y
	SEC
	SBC player1y
	STA _delta_y
.skipelse1
.
 ; 

.L0154 ;  if _delta_x  <  $80 then _delta_y  =  _delta_y  *  2  :  _delta_x  =  _delta_x  *  2

	LDA _delta_x
	CMP #$80
     BCS .skipL0154
.condpart80
	LDA _delta_y
	asl
	STA _delta_y
	LDA _delta_x
	asl
	STA _delta_x
.skipL0154
.
 ; 

.L0155 ;  if _delta_x  >  _delta_y then goto __dx_gt

	LDA _delta_y
	CMP _delta_x
     BCS .skipL0155
.condpart81
 jmp .__dx_gt

.skipL0155
.L0156 ;  _octant{0}  =  0

	LDA _octant
	AND #254
	STA _octant
.L0157 ;  if _error_accumulator  >  _delta_y then _error_accumulator  =  _delta_y  /  2

	LDA _delta_y
	CMP _error_accumulator
     BCS .skipL0157
.condpart82
	LDA _delta_y
	lsr
	STA _error_accumulator
.skipL0157
.L0158 ;  goto __set_ea

 jmp .__set_ea

.
 ; 

.__dx_gt
 ; __dx_gt

.L0159 ;  _octant{0}  =  1

	LDA _octant
	ORA #1
	STA _octant
.L0160 ;  if _error_accumulator  >  _delta_x then _error_accumulator  =  _delta_x  /  2

	LDA _delta_x
	CMP _error_accumulator
     BCS .skipL0160
.condpart83
	LDA _delta_x
	lsr
	STA _error_accumulator
.skipL0160
.
 ; 

.__set_ea
 ; __set_ea

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0161 ;  if !_Bit5_EA{5} then goto __Skip_Bresenham_Setup

	LDA _Bit5_EA
	AND #32
	BNE .skipL0161
.condpart84
 jmp .__Skip_Bresenham_Setup

.skipL0161
.
 ; 

.L0162 ;  _Bit5_EA{5}  =  0

	LDA _Bit5_EA
	AND #223
	STA _Bit5_EA
.L0163 ;  if _octant{0} then _error_accumulator  =  _delta_x  /  2 else _error_accumulator  =  _delta_y  /  2

	LDA _octant
	LSR
	BCC .skipL0163
.condpart85
	LDA _delta_x
	lsr
	STA _error_accumulator
 jmp .skipelse2
.skipL0163
	LDA _delta_y
	lsr
	STA _error_accumulator
.skipelse2
.
 ; 

.__Skip_Bresenham_Setup
 ; __Skip_Bresenham_Setup

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0164 ;  COLUP1  =  $9C

	LDA #$9C
	STA COLUP1
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0165 ;  COLUP0  =  $46

	LDA #$46
	STA COLUP0
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0166 ;  drawscreen

 jsr drawscreen
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0167 ;  if !switchreset then _Bit0_Reset_Restrainer{0}  =  0  :  goto __Main_Loop

 lda #1
 bit SWCHB
	BEQ .skipL0167
.condpart86
	LDA _Bit0_Reset_Restrainer
	AND #254
	STA _Bit0_Reset_Restrainer
 jmp .__Main_Loop

.skipL0167
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0168 ;  if _Bit0_Reset_Restrainer{0} then goto __Main_Loop

	LDA _Bit0_Reset_Restrainer
	LSR
	BCC .skipL0168
.condpart87
 jmp .__Main_Loop

.skipL0168
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0169 ;  goto __Start_Restart

 jmp .__Start_Restart

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0170 ;  data _Data_yinc

_Data_yinc
	.byte   $FF, $FF, $01, $01, $FF, $FF,$01, $01

.skipL0170
.
 ; 

.L0171 ;  data _Data_xinc

_Data_xinc
	.byte   $FF, $FF, $FF, $FF, $01, $01, $01, $01

.skipL0171
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL045_1
	.byte   %00100100
	.byte   %00100100
	.byte   %00100100
	.byte   %10011001
	.byte   %01011010
	.byte   %00111100
	.byte   %00011000
	.byte   %00011000
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL046_0
	.byte   %01100110
	.byte   %00100100
	.byte   %10011001
	.byte   %10111101
	.byte   %01111110
	.byte   %00011000
	.byte   %00111100
	.byte   %00100100
 if ECHOFIRST
       echo "    ",[(scoretable - *)]d , "bytes of ROM space left")
 endif 
ECHOFIRST = 1
 
 
 
; feel free to modify the score graphics - just keep each digit 8 high
; and keep the conditional compilation stuff intact
 ifconst ROM2k
   ORG $F7AC-8
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 16
       ORG $4F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 32
       ORG $8F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 64
       ORG  $10F80-bscode_length
       RORG $1FF80-bscode_length
     endif
   else
     ORG $FF9C
   endif
 endif

; font equates
.21stcentury = 1
alarmclock = 2     
handwritten = 3    
interrupted = 4    
retroputer = 5    
whimsey = 6
tiny = 7

scoretable

 ifconst font
  if font == .21stcentury
    include "score_graphics.asm.21stcentury"
  endif
  if font == alarmclock
    include "score_graphics.asm.alarmclock"
  endif
  if font == handwritten
    include "score_graphics.asm.handwritten"
  endif
  if font == interrupted
    include "score_graphics.asm.interrupted"
  endif
  if font == retroputer
    include "score_graphics.asm.retroputer"
  endif
  if font == whimsey
    include "score_graphics.asm.whimsey"
  endif
  if font == tiny
    include "score_graphics.asm.tiny"
  endif
 else ; default font

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %01111110
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00111000
       .byte %00011000
       .byte %00001000

       .byte %01111110
       .byte %01100000
       .byte %01100000
       .byte %00111100
       .byte %00000110
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00011100
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00001100
       .byte %00001100
       .byte %01111110
       .byte %01001100
       .byte %01001100
       .byte %00101100
       .byte %00011100
       .byte %00001100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00111100
       .byte %01100000
       .byte %01100000
       .byte %01111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01111100
       .byte %01100000
       .byte %01100010
       .byte %00111100

       .byte %00110000
       .byte %00110000
       .byte %00110000
       .byte %00011000
       .byte %00001100
       .byte %00000110
       .byte %01000010
       .byte %00111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00111110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100 

       ifnconst DPC_kernel_options
 
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000 

       endif

 endif

 ifconst ROM2k
   ORG $F7FC
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 16
       ORG $4FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 32
       ORG $8FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 64
       ORG  $10FE0-bscode_length
       RORG $1FFE0-bscode_length
     endif
   else
     ORG $FFFC
   endif
 endif
 ifconst bankswitch
   if bankswitch == 8
     ORG $2FFC
     RORG $FFFC
   endif
   if bankswitch == 16
     ORG $4FFC
     RORG $FFFC
   endif
   if bankswitch == 32
     ORG $8FFC
     RORG $FFFC
   endif
   if bankswitch == 64
     ORG  $10FF0
     RORG $1FFF0
     lda $ffe0 ; we use wasted space to assist stella with EF format auto-detection
     ORG  $10FF8
     RORG $1FFF8
     ifconst superchip 
       .byte "E","F","S","C"
     else
       .byte "E","F","E","F"
     endif
     ORG  $10FFC
     RORG $1FFFC
   endif
 else
   ifconst ROM2k
     ORG $F7FC
   else
     ORG $FFFC
   endif
 endif
 .word start
 .word start
