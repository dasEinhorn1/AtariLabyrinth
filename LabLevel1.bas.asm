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

.L01 ;  dim _BitOp_P0_P1_Dir  =  g

.L02 ;  dim _Bit0_P0_Dir_Up  =  g

.L03 ;  dim _Bit1_P0_Dir_Down  =  g

.L04 ;  dim _Bit2_P0_Dir_Left  =  g

.L05 ;  dim _Bit3_P0_Dir_Right  =  g

.L06 ;  dim _Bit4_P1_Dir_Up  =  g

.L07 ;  dim _Bit5_P1_Dir_Down  =  g

.L08 ;  dim _Bit6_P1_Dir_Left  =  g

.L09 ;  dim _Bit7_P1_Dir_Right  =  g

.
 ; 

.L010 ;  dim _Master_Counter  =  a

.L011 ;  dim _Frame_Counter  =  b

.
 ; 

.L012 ;  dim _Level  =  m

.
 ; 

.
 ; 

.
 ; 

.L013 ;  dim _Bit0_Carrying_Gem  =  x

.L014 ;  dim _Bit1_Player_Moving  =  x

.L015 ;  dim _Bit2_Mino_Moving  =  x

.L016 ;  dim _Bit3_New_Chase_Start  =  x

.L017 ;  dim _Bit4_Game_Over  =  x

.L018 ;  dim _Bit5_Game_Over_Music_Played  =  x

.L019 ;  dim _Bit6_Game_Won  =  x

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L020 ;  dim _BitOp_01  =  y

.L021 ;  dim _Bit0_Reset_Restrainer  =  y

.L022 ;  dim _Bit1_Toggle_Screen  =  y

.
 ; 

.
 ; 

.
 ; 

.L023 ;  dim _Ch0_Sound  =  q

.L024 ;  dim _Ch0_Duration  =  r

.L025 ;  dim _Ch0_Counter  =  s

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L026 ;  dim rand16  =  z

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

.L027 ;  const _P_Edge_Top  =  9

.L028 ;  const _P_Edge_Bottom  =  88

.L029 ;  const _P_Edge_Left  =  1

.L030 ;  const _P_Edge_Right  =  153

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

.L031 ;  AUDV0  =  0  :  AUDV1  =  0

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

.
 ; 

.L032 ;  a  =  0  :  b  =  0  :  c  =  0  :  d  =  0  :  e  =  0  :  f  =  0  :  g  =  0  :  h  =  0  :  i  =  0

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
.L033 ;  j  =  0  :  k  =  0  :  l  =  0  :  m  =  0  :  n  =  0  :  o  =  0  :  p  =  0  :  q  =  0  :  r  =  0

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
.L034 ;  s  =  0  :  t  =  0  :  u  =  0  :  v  =  0  :  w  =  0  :  x  =  0

	LDA #0
	STA s
	STA t
	STA u
	STA v
	STA w
	STA x
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

.L035 ;  _BitOp_01  =  _BitOp_01  &  %00010000

	LDA _BitOp_01
	AND #%00010000
	STA _BitOp_01
.
 ; 

.
 ; 

.
 ; 

.L036 ;  _Level  =  0

	LDA #0
	STA _Level
.
 ; 

.__Title_Screen
 ; __Title_Screen

.L037 ;  playfield:

  ifconst pfres
	  ldx #(11>pfres)*(pfres*pfwidth-1)+(11<=pfres)*43
  else
	  ldx #((11*pfwidth-1)*((11*pfwidth-1)<47))+(47*((11*pfwidth-1)>=47))
  endif
	jmp pflabel0
PF_data0
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %10011101, %10101001
	if (pfwidth>2)
	.byte %10010110, %10101110
 endif
	.byte %10010101, %10101010
	if (pfwidth>2)
	.byte %01010101, %10100100
 endif
	.byte %10011101, %10010001
	if (pfwidth>2)
	.byte %10010101, %11100100
 endif
	.byte %10010101, %10010010
	if (pfwidth>2)
	.byte %01010101, %10100100
 endif
	.byte %11010101, %10010001
	if (pfwidth>2)
	.byte %01010101, %10100100
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00011111, %11111111
	if (pfwidth>2)
	.byte %11111111, %00011111
 endif
	.byte %00001010, %01010101
	if (pfwidth>2)
	.byte %01010101, %00001010
 endif
	.byte %00011111, %01111111
	if (pfwidth>2)
	.byte %01111111, %00011111
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
pflabel0
	lda PF_data0,x
	sta playfield,x
	dex
	bpl pflabel0
.L038 ;  player0:

	LDX #<playerL038_0
	STX player0pointerlo
	LDA #>playerL038_0
	STA player0pointerhi
	LDA #7
	STA player0height
.L039 ;  player1:

	LDX #<playerL039_1
	STX player1pointerlo
	LDA #>playerL039_1
	STA player1pointerhi
	LDA #7
	STA player1height
.
 ; 

.__Start_Screen_Loop
 ; __Start_Screen_Loop

.L040 ;  if _Master_Counter  <=  10 then goto __Skip_Joy_Checks

	LDA #10
	CMP _Master_Counter
     BCC .skipL040
.condpart0
 jmp .__Skip_Joy_Checks

.skipL040
.L041 ;  if joy0up then goto __Skip_Start_Screen

 lda #$10
 bit SWCHA
	BNE .skipL041
.condpart1
 jmp .__Skip_Start_Screen

.skipL041
.L042 ;  if joy0down then goto __Skip_Start_Screen

 lda #$20
 bit SWCHA
	BNE .skipL042
.condpart2
 jmp .__Skip_Start_Screen

.skipL042
.L043 ;  if joy0right then goto __Skip_Start_Screen

 bit SWCHA
	BMI .skipL043
.condpart3
 jmp .__Skip_Start_Screen

.skipL043
.L044 ;  if joy0left then goto __Skip_Start_Screen

 bit SWCHA
	BVS .skipL044
.condpart4
 jmp .__Skip_Start_Screen

.skipL044
.L045 ;  if joy0fire then goto __Skip_Start_Screen

 bit INPT4
	BMI .skipL045
.condpart5
 jmp .__Skip_Start_Screen

.skipL045
.__Skip_Joy_Checks
 ; __Skip_Joy_Checks

.L046 ;  player1x  =  77

	LDA #77
	STA player1x
.L047 ;  player1y  =  80

	LDA #80
	STA player1y
.L048 ;  player0x  =  77

	LDA #77
	STA player0x
.L049 ;  player0y  =  72

	LDA #72
	STA player0y
.L050 ;  COLUP0  =  $36

	LDA #$36
	STA COLUP0
.L051 ;  COLUP1  =  $14

	LDA #$14
	STA COLUP1
.L052 ;  COLUPF  =  $14

	LDA #$14
	STA COLUPF
.L053 ;  COLUBK  =  0

	LDA #0
	STA COLUBK
.L054 ;  scorecolor  =  0

	LDA #0
	STA scorecolor
.L055 ;  _Master_Counter  =  _Master_Counter  +  1

	INC _Master_Counter
.L056 ;  drawscreen

 jsr drawscreen
.L057 ;  goto __Start_Screen_Loop

 jmp .__Start_Screen_Loop

.
 ; 

.L058 ;  _Master_Counter  =  0  :  _Frame_Counter  =  0

	LDA #0
	STA _Master_Counter
	STA _Frame_Counter
.__Skip_Start_Screen
 ; __Skip_Start_Screen

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

.L059 ;  player0x  =  77  :  player0y  =  60

	LDA #77
	STA player0x
	LDA #60
	STA player0y
.L060 ;  player1x  =  21  :  player1y  =  16

	LDA #21
	STA player1x
	LDA #16
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

.L061 ;  missile0x  =  200  :  missile0y  =  200

	LDA #200
	STA missile0x
	STA missile0y
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L062 ;  missile0x  =  80  :  missile0y  =  79

	LDA #80
	STA missile0x
	LDA #79
	STA missile0y
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

.L063 ;  NUSIZ0  =  $10  :  missile0height  =  1

	LDA #$10
	STA NUSIZ0
	LDA #1
	STA missile0height
.L064 ;  NUSIZ1  =  $30  :  missile0height  =  4

	LDA #$30
	STA NUSIZ1
	LDA #4
	STA missile0height
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

.L065 ;  COLUBK  =  0

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

.L066 ;  _Bit3_P0_Dir_Right{3}  =  1

	LDA _Bit3_P0_Dir_Right
	ORA #8
	STA _Bit3_P0_Dir_Right
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

.L067 ;  _Bit0_Reset_Restrainer{0}  =  1

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

.L068 ;  score  =  0

	LDA #$00
	STA score+2
	LDA #$00
	STA score+1
	LDA #$00
	STA score
.
 ; 

.L069 ;  dim _sc1  =  score

.L070 ;  dim _sc2  =  score + 1

.L071 ;  dim _sc3  =  score + 2

.
 ; 

.L072 ;  t  =  $3

	LDA #$3
	STA t
.L073 ;  dim _Chase_Speed  =  t

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

.L074 ;  if _Bit4_Game_Over{4} then if joy0fire goto __Start_Restart

	LDA _Bit4_Game_Over
	AND #16
	BEQ .skipL074
.condpart6
 bit INPT4
 if ((* - .__Start_Restart) < 127) && ((* - .__Start_Restart) > -128)
	bpl .__Start_Restart
 else
	bmi .0skip__Start_Restart
	jmp .__Start_Restart
.0skip__Start_Restart
 endif
.skipL074
.
 ; 

.L075 ;  if _sc1 = %00  &&  _sc2 = %00  &&  _sc3  <  $21 then __Skip_Level_Change

	LDA _sc1
	CMP #%00
     BNE .skipL075
.condpart7
	LDA _sc2
	CMP #%00
     BNE .skip7then
.condpart8
	LDA _sc3
	CMP #$21
 if ((* - .__Skip_Level_Change) < 127) && ((* - .__Skip_Level_Change) > -128)
	bcc .__Skip_Level_Change
 else
	bcs .1skip__Skip_Level_Change
	jmp .__Skip_Level_Change
.1skip__Skip_Level_Change
 endif
.skip7then
.skipL075
.L076 ;  _sc3  =  %00

	LDA #%00
	STA _sc3
.L077 ;  _Level  =  _Level  +  $1

	LDA _Level
	CLC
	ADC #$1
	STA _Level
.L078 ;  _Chase_Speed  =  $1

	LDA #$1
	STA _Chase_Speed
.__Skip_Level_Change
 ; __Skip_Level_Change

.L079 ;  if _Level = $0 then goto __L0

	LDA _Level
	CMP #$0
     BNE .skipL079
.condpart9
 jmp .__L0

.skipL079
.L080 ;  if _Level = $1 then goto __L1

	LDA _Level
	CMP #$1
     BNE .skipL080
.condpart10
 jmp .__L1

.skipL080
.L081 ;  if _Level = $2 then goto __L2 else _Bit6_Game_Won{6} = 1  :  _Bit4_Game_Over{4} = 1  :  goto __Game_Over

	LDA _Level
	CMP #$2
     BNE .skipL081
.condpart11
 jmp .__L2
 jmp .skipelse0
.skipL081
	LDA _Bit6_Game_Won
	ORA #64
	STA _Bit6_Game_Won
	LDA _Bit4_Game_Over
	ORA #16
	STA _Bit4_Game_Over
 jmp .__Game_Over

.skipelse0
.__L_Done
 ; __L_Done

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

.L082 ;  _Master_Counter  =  _Master_Counter  +  1

	INC _Master_Counter
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L083 ;  if _Master_Counter  <  7 then goto __Skip_Counters

	LDA _Master_Counter
	CMP #7
     BCS .skipL083
.condpart12
 jmp .__Skip_Counters

.skipL083
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L084 ;  _Frame_Counter  =  _Frame_Counter  +  1  :  _Master_Counter  =  0

	INC _Frame_Counter
	LDA #0
	STA _Master_Counter
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L085 ;  if _Frame_Counter  >  3 then _Frame_Counter  =  0

	LDA #3
	CMP _Frame_Counter
     BCS .skipL085
.condpart13
	LDA #0
	STA _Frame_Counter
.skipL085
.
 ; 

.__Skip_Counters
 ; __Skip_Counters

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

.L086 ;  player0:

	LDX #<playerL086_0
	STX player0pointerlo
	LDA #>playerL086_0
	STA player0pointerhi
	LDA #7
	STA player0height
.L087 ;  if !_Bit1_Player_Moving{1} goto __Pl_Frame_Done

	LDA _Bit1_Player_Moving
	AND #2
 if ((* - .__Pl_Frame_Done) < 127) && ((* - .__Pl_Frame_Done) > -128)
	BEQ .__Pl_Frame_Done
 else
	bne .2skip__Pl_Frame_Done
	jmp .__Pl_Frame_Done
.2skip__Pl_Frame_Done
 endif
.L088 ;  on _Frame_Counter goto __Pl00 __Pl01 __Pl00 __Pl02

	LDX _Frame_Counter
	LDA .L088jumptablehi,x
	PHA
	LDA .L088jumptablelo,x
	PHA
	RTS
.L088jumptablehi
	.byte >(.__Pl00-1)
	.byte >(.__Pl01-1)
	.byte >(.__Pl00-1)
	.byte >(.__Pl02-1)
.L088jumptablelo
	.byte <(.__Pl00-1)
	.byte <(.__Pl01-1)
	.byte <(.__Pl00-1)
	.byte <(.__Pl02-1)
.
 ; 

.__Pl_Frame_Done
 ; __Pl_Frame_Done

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

.L089 ;  player1:

	LDX #<playerL089_1
	STX player1pointerlo
	LDA #>playerL089_1
	STA player1pointerhi
	LDA #7
	STA player1height
.L090 ;  if !_Bit2_Mino_Moving{2} goto __Mn_Frame_Done

	LDA _Bit2_Mino_Moving
	AND #4
 if ((* - .__Mn_Frame_Done) < 127) && ((* - .__Mn_Frame_Done) > -128)
	BEQ .__Mn_Frame_Done
 else
	bne .3skip__Mn_Frame_Done
	jmp .__Mn_Frame_Done
.3skip__Mn_Frame_Done
 endif
.L091 ;  on _Frame_Counter goto __Mn00 __Mn01 __Mn00 __Mn02

	LDX _Frame_Counter
	LDA .L091jumptablehi,x
	PHA
	LDA .L091jumptablelo,x
	PHA
	RTS
.L091jumptablehi
	.byte >(.__Mn00-1)
	.byte >(.__Mn01-1)
	.byte >(.__Mn00-1)
	.byte >(.__Mn02-1)
.L091jumptablelo
	.byte <(.__Mn00-1)
	.byte <(.__Mn01-1)
	.byte <(.__Mn00-1)
	.byte <(.__Mn02-1)
.
 ; 

.__Mn_Frame_Done
 ; __Mn_Frame_Done

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L092 ;  if !_Ch0_Sound then goto __Skip_Ch_0

	LDA _Ch0_Sound
	BNE .skipL092
.condpart14
 jmp .__Skip_Ch_0

.skipL092
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L093 ;  _Ch0_Duration  =  _Ch0_Duration  -  1

	DEC _Ch0_Duration
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

.L094 ;  if _Ch0_Duration then goto __Skip_Ch_0

	LDA _Ch0_Duration
	BEQ .skipL094
.condpart15
 jmp .__Skip_Ch_0

.skipL094
.
 ; 

.L095 ;  if _Ch0_Sound  <>  1 then goto __Skip_Ch0_Sound_001

	LDA _Ch0_Sound
	CMP #1
     BEQ .skipL095
.condpart16
 jmp .__Skip_Ch0_Sound_001

.skipL095
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L096 ;  temp4  =  _SD_Gem_Coll[_Ch0_Counter]

	LDX _Ch0_Counter
	LDA _SD_Gem_Coll,x
	STA temp4
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L097 ;  if temp4  =  255 then goto __Clear_Ch_0

	LDA temp4
	CMP #255
     BNE .skipL097
.condpart17
 jmp .__Clear_Ch_0

.skipL097
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L098 ;  _Ch0_Counter  =  _Ch0_Counter  +  1

	INC _Ch0_Counter
.L099 ;  temp5  =  _SD_Gem_Coll[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

	LDX _Ch0_Counter
	LDA _SD_Gem_Coll,x
	STA temp5
	INC _Ch0_Counter
.L0100 ;  temp6  =  _SD_Gem_Coll[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

	LDX _Ch0_Counter
	LDA _SD_Gem_Coll,x
	STA temp6
	INC _Ch0_Counter
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0101 ;  AUDV0  =  temp4

	LDA temp4
	STA AUDV0
.L0102 ;  AUDC0  =  temp5

	LDA temp5
	STA AUDC0
.L0103 ;  AUDF0  =  temp6

	LDA temp6
	STA AUDF0
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0104 ;  _Ch0_Duration  =  _SD_Gem_Coll[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

	LDX _Ch0_Counter
	LDA _SD_Gem_Coll,x
	STA _Ch0_Duration
	INC _Ch0_Counter
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0105 ;  goto __Skip_Ch_0

 jmp .__Skip_Ch_0

.
 ; 

.__Skip_Ch0_Sound_001
 ; __Skip_Ch0_Sound_001

.
 ; 

.L0106 ;  if _Ch0_Sound  <>  2 then goto __Skip_Ch0_Sound_002

	LDA _Ch0_Sound
	CMP #2
     BEQ .skipL0106
.condpart18
 jmp .__Skip_Ch0_Sound_002

.skipL0106
.
 ; 

.
 ; 

.
 ; 

.L0107 ;  temp4  =  _SD_Gem_Drop[_Ch0_Counter]

	LDX _Ch0_Counter
	LDA _SD_Gem_Drop,x
	STA temp4
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0108 ;  if temp4  =  255 then goto __Clear_Ch_0

	LDA temp4
	CMP #255
     BNE .skipL0108
.condpart19
 jmp .__Clear_Ch_0

.skipL0108
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0109 ;  _Ch0_Counter  =  _Ch0_Counter  +  1

	INC _Ch0_Counter
.L0110 ;  temp5  =  _SD_Gem_Drop[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

	LDX _Ch0_Counter
	LDA _SD_Gem_Drop,x
	STA temp5
	INC _Ch0_Counter
.L0111 ;  temp6  =  _SD_Gem_Drop[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

	LDX _Ch0_Counter
	LDA _SD_Gem_Drop,x
	STA temp6
	INC _Ch0_Counter
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0112 ;  AUDV0  =  temp4

	LDA temp4
	STA AUDV0
.L0113 ;  AUDC0  =  temp5

	LDA temp5
	STA AUDC0
.L0114 ;  AUDF0  =  temp6

	LDA temp6
	STA AUDF0
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0115 ;  _Ch0_Duration  =  _SD_Gem_Drop[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

	LDX _Ch0_Counter
	LDA _SD_Gem_Drop,x
	STA _Ch0_Duration
	INC _Ch0_Counter
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0116 ;  goto __Skip_Ch_0

 jmp .__Skip_Ch_0

.
 ; 

.__Skip_Ch0_Sound_002
 ; __Skip_Ch0_Sound_002

.
 ; 

.L0117 ;  if _Ch0_Sound  <>  3 then goto __Skip_Ch0_Sound_003

	LDA _Ch0_Sound
	CMP #3
     BEQ .skipL0117
.condpart20
 jmp .__Skip_Ch0_Sound_003

.skipL0117
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0118 ;  temp4  =  _SD_Mino_Roar[_Ch0_Counter]

	LDX _Ch0_Counter
	LDA _SD_Mino_Roar,x
	STA temp4
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0119 ;  if temp4  =  255 then goto __Clear_Ch_0

	LDA temp4
	CMP #255
     BNE .skipL0119
.condpart21
 jmp .__Clear_Ch_0

.skipL0119
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0120 ;  _Ch0_Counter  =  _Ch0_Counter  +  1

	INC _Ch0_Counter
.L0121 ;  temp5  =  _SD_Mino_Roar[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

	LDX _Ch0_Counter
	LDA _SD_Mino_Roar,x
	STA temp5
	INC _Ch0_Counter
.L0122 ;  temp6  =  _SD_Mino_Roar[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

	LDX _Ch0_Counter
	LDA _SD_Mino_Roar,x
	STA temp6
	INC _Ch0_Counter
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0123 ;  AUDV0  =  temp4

	LDA temp4
	STA AUDV0
.L0124 ;  AUDC0  =  temp5

	LDA temp5
	STA AUDC0
.L0125 ;  AUDF0  =  temp6

	LDA temp6
	STA AUDF0
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0126 ;  _Ch0_Duration  =  _SD_Mino_Roar[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

	LDX _Ch0_Counter
	LDA _SD_Mino_Roar,x
	STA _Ch0_Duration
	INC _Ch0_Counter
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0127 ;  goto __Skip_Ch_0

 jmp .__Skip_Ch_0

.
 ; 

.__Skip_Ch0_Sound_003
 ; __Skip_Ch0_Sound_003

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0128 ;  goto __Skip_Ch_0

 jmp .__Skip_Ch_0

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

.__Clear_Ch_0
 ; __Clear_Ch_0

.
 ; 

.L0129 ;  _Ch0_Sound  =  0  :  AUDV0  =  0

	LDA #0
	STA _Ch0_Sound
	STA AUDV0
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

.__Skip_Ch_0
 ; __Skip_Ch_0

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0130 ;  if !collision(player0,player1) then __Skip_Player_Caught

	bit 	CXPPMM
 if ((* - .__Skip_Player_Caught) < 127) && ((* - .__Skip_Player_Caught) > -128)
	bpl .__Skip_Player_Caught
 else
	bmi .4skip__Skip_Player_Caught
	jmp .__Skip_Player_Caught
.4skip__Skip_Player_Caught
 endif
.L0131 ;  _Bit4_Game_Over{4}  =  1

	LDA _Bit4_Game_Over
	ORA #16
	STA _Bit4_Game_Over
.L0132 ;  goto __Game_Over

 jmp .__Game_Over

.__Skip_Player_Caught
 ; __Skip_Player_Caught

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

.L0133 ;  NUSIZ0  =  $20  :  missile0height  =  2

	LDA #$20
	STA NUSIZ0
	LDA #2
	STA missile0height
.L0134 ;  NUSIZ1  =  $30  :  missile1height  =  7

	LDA #$30
	STA NUSIZ1
	LDA #7
	STA missile1height
.
 ; 

.L0135 ;  missile1x  =  78  :  missile1y  =  48

	LDA #78
	STA missile1x
	LDA #48
	STA missile1y
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

.L0136 ;  COLUP0  =  $9C

	LDA #$9C
	STA COLUP0
.L0137 ;  if _Chase_Speed  >  $1 then COLUP1  =  $36

	LDA #$1
	CMP _Chase_Speed
     BCS .skipL0137
.condpart22
	LDA #$36
	STA COLUP1
.skipL0137
.L0138 ;  if _Chase_Speed  =  $1 then COLUP1  =  $35

	LDA _Chase_Speed
	CMP #$1
     BNE .skipL0138
.condpart23
	LDA #$35
	STA COLUP1
.skipL0138
.L0139 ;  if _Chase_Speed  =  $0 then COLUP1  =  $33

	LDA _Chase_Speed
	CMP #$0
     BNE .skipL0139
.condpart24
	LDA #$33
	STA COLUP1
.skipL0139
.L0140 ;  scorecolor  =  $9C

	LDA #$9C
	STA scorecolor
.
 ; 

.L0141 ;  if _sc1 = %00  &&  _sc2 = %00  &&  _sc3  =  $20 then _Chase_Speed  =  $0 else goto __Skip_Speed_Check1

	LDA _sc1
	CMP #%00
     BNE .skipL0141
.condpart25
	LDA _sc2
	CMP #%00
     BNE .skip25then
.condpart26
	LDA _sc3
	CMP #$20
     BNE .skip26then
.condpart27
	LDA #$0
	STA _Chase_Speed
 jmp .skipelse1
.skip26then
.skip25then
.skipL0141
 jmp .__Skip_Speed_Check1

.skipelse1
.L0142 ;  if _Ch0_Sound  <>  3  &&  !_Bit3_New_Chase_Start{3} then _Bit3_New_Chase_Start{3}  =  1  :  _Ch0_Sound  =  3  :  _Ch0_Duration  =  1  :  _Ch0_Counter  =  0

	LDA _Ch0_Sound
	CMP #3
     BEQ .skipL0142
.condpart28
	LDA _Bit3_New_Chase_Start
	AND #8
	BNE .skip28then
.condpart29
	LDA _Bit3_New_Chase_Start
	ORA #8
	STA _Bit3_New_Chase_Start
	LDA #3
	STA _Ch0_Sound
	LDA #1
	STA _Ch0_Duration
	LDA #0
	STA _Ch0_Counter
.skip28then
.skipL0142
.L0143 ;  goto __Skip_Speed_Check2

 jmp .__Skip_Speed_Check2

.__Skip_Speed_Check1
 ; __Skip_Speed_Check1

.L0144 ;  if _sc1 = %00  &&  _sc2 = %00  &&  _sc3  =  $10 then _Chase_Speed  =  $1 else goto __Skip_Speed_Check2

	LDA _sc1
	CMP #%00
     BNE .skipL0144
.condpart30
	LDA _sc2
	CMP #%00
     BNE .skip30then
.condpart31
	LDA _sc3
	CMP #$10
     BNE .skip31then
.condpart32
	LDA #$1
	STA _Chase_Speed
 jmp .skipelse2
.skip31then
.skip30then
.skipL0144
 jmp .__Skip_Speed_Check2

.skipelse2
.L0145 ;  if _Ch0_Sound  <>  3  &&  !_Bit3_New_Chase_Start{3} then _Bit3_New_Chase_Start{3}  =  1  :  _Ch0_Sound  =  3  :  _Ch0_Duration  =  1  :  _Ch0_Counter  =  0

	LDA _Ch0_Sound
	CMP #3
     BEQ .skipL0145
.condpart33
	LDA _Bit3_New_Chase_Start
	AND #8
	BNE .skip33then
.condpart34
	LDA _Bit3_New_Chase_Start
	ORA #8
	STA _Bit3_New_Chase_Start
	LDA #3
	STA _Ch0_Sound
	LDA #1
	STA _Ch0_Duration
	LDA #0
	STA _Ch0_Counter
.skip33then
.skipL0145
.__Skip_Speed_Check2
 ; __Skip_Speed_Check2

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

.L0146 ;  _Bit1_Player_Moving{1}  =  0

	LDA _Bit1_Player_Moving
	AND #253
	STA _Bit1_Player_Moving
.L0147 ;  if !joy0up  &&  !joy0down  &&  !joy0left  &&  !joy0right then goto __Skip_Joystick_Precheck

 lda #$10
 bit SWCHA
	BEQ .skipL0147
.condpart35
 lda #$20
 bit SWCHA
	BEQ .skip35then
.condpart36
 bit SWCHA
	BVC .skip36then
.condpart37
 bit SWCHA
	BPL .skip37then
.condpart38
 jmp .__Skip_Joystick_Precheck

.skip37then
.skip36then
.skip35then
.skipL0147
.
 ; 

.
 ; 

.
 ; 

.L0148 ;  _BitOp_P0_P1_Dir  =  _BitOp_P0_P1_Dir  &  %11110000

	LDA _BitOp_P0_P1_Dir
	AND #%11110000
	STA _BitOp_P0_P1_Dir
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

.
 ; 

.L0149 ;  if !joy0up then goto __Skip_Joy0_Up

 lda #$10
 bit SWCHA
	BEQ .skipL0149
.condpart39
 jmp .__Skip_Joy0_Up

.skipL0149
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0150 ;  _Bit0_P0_Dir_Up{0}  =  1

	LDA _Bit0_P0_Dir_Up
	ORA #1
	STA _Bit0_P0_Dir_Up
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0151 ;  if player0y  <=  _P_Edge_Top then goto __Skip_Joy0_Up

	LDA #_P_Edge_Top
	CMP player0y
     BCC .skipL0151
.condpart40
 jmp .__Skip_Joy0_Up

.skipL0151
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0152 ;  temp5  =   ( player0x - 11 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #11
	lsr
	lsr
	STA temp5
.
 ; 

.L0153 ;  temp6  =   ( player0y - 9 )  / 8

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

.L0154 ;  if temp5  <  34 then if pfread ( temp5 , temp6 )  then goto __Skip_Joy0_Up

	LDA temp5
	CMP #34
     BCS .skipL0154
.condpart41
	LDA temp5
	LDY temp6
 jsr pfread
	BNE .skip41then
.condpart42
 jmp .__Skip_Joy0_Up

.skip41then
.skipL0154
.
 ; 

.L0155 ;  temp4  =   ( player0x - 16 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #16
	lsr
	lsr
	STA temp4
.
 ; 

.L0156 ;  if temp4  <  34 then if pfread ( temp4 , temp6 )  then goto __Skip_Joy0_Up

	LDA temp4
	CMP #34
     BCS .skipL0156
.condpart43
	LDA temp4
	LDY temp6
 jsr pfread
	BNE .skip43then
.condpart44
 jmp .__Skip_Joy0_Up

.skip43then
.skipL0156
.
 ; 

.L0157 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L0158 ;  if temp3  <  34 then if pfread ( temp3 , temp6 )  then goto __Skip_Joy0_Up

	LDA temp3
	CMP #34
     BCS .skipL0158
.condpart45
	LDA temp3
	LDY temp6
 jsr pfread
	BNE .skip45then
.condpart46
 jmp .__Skip_Joy0_Up

.skip45then
.skipL0158
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0159 ;  _Bit1_Player_Moving{1}  =  1

	LDA _Bit1_Player_Moving
	ORA #2
	STA _Bit1_Player_Moving
.L0160 ;  player0y  =  player0y  -  1

	DEC player0y
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

.L0161 ;  if !joy0down then goto __Skip_Joy0_Down

 lda #$20
 bit SWCHA
	BEQ .skipL0161
.condpart47
 jmp .__Skip_Joy0_Down

.skipL0161
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0162 ;  _Bit1_P0_Dir_Down{1}  =  1

	LDA _Bit1_P0_Dir_Down
	ORA #2
	STA _Bit1_P0_Dir_Down
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0163 ;  if player0y  >=  _P_Edge_Bottom then goto __Skip_Joy0_Down

	LDA player0y
	CMP #_P_Edge_Bottom
     BCC .skipL0163
.condpart48
 jmp .__Skip_Joy0_Down

.skipL0163
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0164 ;  temp5  =   ( player0x - 11 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #11
	lsr
	lsr
	STA temp5
.
 ; 

.L0165 ;  temp6  =   ( player0y )  / 8

; complex statement detected
	LDA player0y
	lsr
	lsr
	lsr
	STA temp6
.
 ; 

.L0166 ;  if temp5  <  34 then if pfread ( temp5 , temp6 )  then goto __Skip_Joy0_Down

	LDA temp5
	CMP #34
     BCS .skipL0166
.condpart49
	LDA temp5
	LDY temp6
 jsr pfread
	BNE .skip49then
.condpart50
 jmp .__Skip_Joy0_Down

.skip49then
.skipL0166
.
 ; 

.L0167 ;  temp4  =   ( player0x - 16 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #16
	lsr
	lsr
	STA temp4
.
 ; 

.L0168 ;  if temp4  <  34 then if pfread ( temp4 , temp6 )  then goto __Skip_Joy0_Down

	LDA temp4
	CMP #34
     BCS .skipL0168
.condpart51
	LDA temp4
	LDY temp6
 jsr pfread
	BNE .skip51then
.condpart52
 jmp .__Skip_Joy0_Down

.skip51then
.skipL0168
.
 ; 

.L0169 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L0170 ;  if temp3  <  34 then if pfread ( temp3 , temp6 )  then goto __Skip_Joy0_Down

	LDA temp3
	CMP #34
     BCS .skipL0170
.condpart53
	LDA temp3
	LDY temp6
 jsr pfread
	BNE .skip53then
.condpart54
 jmp .__Skip_Joy0_Down

.skip53then
.skipL0170
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0171 ;  _Bit1_Player_Moving{1}  =  1

	LDA _Bit1_Player_Moving
	ORA #2
	STA _Bit1_Player_Moving
.L0172 ;  player0y  =  player0y  +  1

	INC player0y
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

.
 ; 

.
 ; 

.L0173 ;  if !joy0left then goto __Skip_Joy0_Left

 bit SWCHA
	BVC .skipL0173
.condpart55
 jmp .__Skip_Joy0_Left

.skipL0173
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0174 ;  _Bit2_P0_Dir_Left{2}  =  1

	LDA _Bit2_P0_Dir_Left
	ORA #4
	STA _Bit2_P0_Dir_Left
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0175 ;  if player0x  <=  _P_Edge_Left then goto __Skip_Joy0_Left

	LDA #_P_Edge_Left
	CMP player0x
     BCC .skipL0175
.condpart56
 jmp .__Skip_Joy0_Left

.skipL0175
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0176 ;  temp5  =   ( player0y - 1 )  / 8

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

.L0177 ;  temp6  =   ( player0x - 17 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #17
	lsr
	lsr
	STA temp6
.
 ; 

.L0178 ;  if temp6  <  34 then if pfread ( temp6 , temp5 )  then goto __Skip_Joy0_Left

	LDA temp6
	CMP #34
     BCS .skipL0178
.condpart57
	LDA temp6
	LDY temp5
 jsr pfread
	BNE .skip57then
.condpart58
 jmp .__Skip_Joy0_Left

.skip57then
.skipL0178
.
 ; 

.L0179 ;  temp3  =   ( player0y - 8 )  / 8

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

.L0180 ;  if temp6  <  34 then if pfread ( temp6 , temp3 )  then goto __Skip_Joy0_Left

	LDA temp6
	CMP #34
     BCS .skipL0180
.condpart59
	LDA temp6
	LDY temp3
 jsr pfread
	BNE .skip59then
.condpart60
 jmp .__Skip_Joy0_Left

.skip59then
.skipL0180
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0181 ;  _Bit1_Player_Moving{1}  =  1

	LDA _Bit1_Player_Moving
	ORA #2
	STA _Bit1_Player_Moving
.L0182 ;  REFP0  =  8

	LDA #8
	STA REFP0
.L0183 ;  player0x  =  player0x  -  1

	DEC player0x
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

.L0184 ;  if !joy0right then goto __Skip_Joy0_Right

 bit SWCHA
	BPL .skipL0184
.condpart61
 jmp .__Skip_Joy0_Right

.skipL0184
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0185 ;  _Bit3_P0_Dir_Right{3}  =  1

	LDA _Bit3_P0_Dir_Right
	ORA #8
	STA _Bit3_P0_Dir_Right
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0186 ;  if player0x  >=  _P_Edge_Right then goto __Skip_Joy0_Right

	LDA player0x
	CMP #_P_Edge_Right
     BCC .skipL0186
.condpart62
 jmp .__Skip_Joy0_Right

.skipL0186
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0187 ;  temp5  =   ( player0y - 1 )  / 8

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

.L0188 ;  temp6  =   ( player0x - 10 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #10
	lsr
	lsr
	STA temp6
.
 ; 

.L0189 ;  if temp6  <  34 then if pfread ( temp6 , temp5 )  then goto __Skip_Joy0_Right

	LDA temp6
	CMP #34
     BCS .skipL0189
.condpart63
	LDA temp6
	LDY temp5
 jsr pfread
	BNE .skip63then
.condpart64
 jmp .__Skip_Joy0_Right

.skip63then
.skipL0189
.
 ; 

.L0190 ;  temp3  =   ( player0y - 8 )  / 8

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

.L0191 ;  if temp6  <  34 then if pfread ( temp6 , temp3 )  then goto __Skip_Joy0_Right

	LDA temp6
	CMP #34
     BCS .skipL0191
.condpart65
	LDA temp6
	LDY temp3
 jsr pfread
	BNE .skip65then
.condpart66
 jmp .__Skip_Joy0_Right

.skip65then
.skipL0191
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0192 ;  _Bit1_Player_Moving{1}  =  1

	LDA _Bit1_Player_Moving
	ORA #2
	STA _Bit1_Player_Moving
.L0193 ;  player0x  =  player0x  +  1

	INC player0x
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

.L0194 ;  if _Master_Counter & _Chase_Speed then goto __Skip_AI_Right

	LDA _Master_Counter
	AND _Chase_Speed
     BEQ .skipL0194
.condpart67
 jmp .__Skip_AI_Right

.skipL0194
.L0195 ;  _Bit2_Mino_Moving{2}  =  0

	LDA _Bit2_Mino_Moving
	AND #251
	STA _Bit2_Mino_Moving
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

.L0196 ;  if player1y  <=  player0y then goto __Skip_AI_Up

	LDA player0y
	CMP player1y
     BCC .skipL0196
.condpart68
 jmp .__Skip_AI_Up

.skipL0196
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0197 ;  _Bit4_P1_Dir_Up{4}  =  1

	LDA _Bit4_P1_Dir_Up
	ORA #16
	STA _Bit4_P1_Dir_Up
.L0198 ;  _Bit5_P1_Dir_Down{5}  =  0

	LDA _Bit5_P1_Dir_Down
	AND #223
	STA _Bit5_P1_Dir_Down
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0199 ;  if player1y  <=  _P_Edge_Top then goto __Skip_AI_Up

	LDA #_P_Edge_Top
	CMP player1y
     BCC .skipL0199
.condpart69
 jmp .__Skip_AI_Up

.skipL0199
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0200 ;  temp5  =   ( player1x - 10 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #10
	lsr
	lsr
	STA temp5
.
 ; 

.L0201 ;  temp6  =   ( player1y - 9 )  / 8

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

.L0202 ;  if temp5  <  34 then if pfread ( temp5 , temp6 )  then goto __Skip_AI_Up

	LDA temp5
	CMP #34
     BCS .skipL0202
.condpart70
	LDA temp5
	LDY temp6
 jsr pfread
	BNE .skip70then
.condpart71
 jmp .__Skip_AI_Up

.skip70then
.skipL0202
.
 ; 

.L0203 ;  temp4  =   ( player1x - 17 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #17
	lsr
	lsr
	STA temp4
.
 ; 

.L0204 ;  if temp4  <  34 then if pfread ( temp4 , temp6 )  then goto __Skip_AI_Up

	LDA temp4
	CMP #34
     BCS .skipL0204
.condpart72
	LDA temp4
	LDY temp6
 jsr pfread
	BNE .skip72then
.condpart73
 jmp .__Skip_AI_Up

.skip72then
.skipL0204
.
 ; 

.L0205 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L0206 ;  if temp3  <  34 then if pfread ( temp3 , temp6 )  then goto __Skip_AI_Up

	LDA temp3
	CMP #34
     BCS .skipL0206
.condpart74
	LDA temp3
	LDY temp6
 jsr pfread
	BNE .skip74then
.condpart75
 jmp .__Skip_AI_Up

.skip74then
.skipL0206
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0207 ;  _Bit2_Mino_Moving{2}  =  1

	LDA _Bit2_Mino_Moving
	ORA #4
	STA _Bit2_Mino_Moving
.L0208 ;  player1y  =  player1y  -  1

	DEC player1y
.
 ; 

.__Skip_AI_Up
 ; __Skip_AI_Up

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

.L0209 ;  if player1y  >=  player0y then goto __Skip_AI_Down

	LDA player1y
	CMP player0y
     BCC .skipL0209
.condpart76
 jmp .__Skip_AI_Down

.skipL0209
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0210 ;  _Bit5_P1_Dir_Down{5}  =  1

	LDA _Bit5_P1_Dir_Down
	ORA #32
	STA _Bit5_P1_Dir_Down
.L0211 ;  _Bit4_P1_Dir_Up{4}  =  0

	LDA _Bit4_P1_Dir_Up
	AND #239
	STA _Bit4_P1_Dir_Up
.
 ; 

.
 ; 

.
 ; 

.L0212 ;  if player1y  >=  _P_Edge_Bottom then goto __Skip_AI_Down

	LDA player1y
	CMP #_P_Edge_Bottom
     BCC .skipL0212
.condpart77
 jmp .__Skip_AI_Down

.skipL0212
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0213 ;  temp5  =   ( player1x - 10 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #10
	lsr
	lsr
	STA temp5
.
 ; 

.L0214 ;  temp6  =   ( player1y )  / 8

; complex statement detected
	LDA player1y
	lsr
	lsr
	lsr
	STA temp6
.
 ; 

.L0215 ;  if temp5  <  34 then if pfread ( temp5 , temp6 )  then goto __Skip_AI_Down

	LDA temp5
	CMP #34
     BCS .skipL0215
.condpart78
	LDA temp5
	LDY temp6
 jsr pfread
	BNE .skip78then
.condpart79
 jmp .__Skip_AI_Down

.skip78then
.skipL0215
.
 ; 

.L0216 ;  temp4  =   ( player1x - 17 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #17
	lsr
	lsr
	STA temp4
.
 ; 

.L0217 ;  if temp4  <  34 then if pfread ( temp4 , temp6 )  then goto __Skip_AI_Down

	LDA temp4
	CMP #34
     BCS .skipL0217
.condpart80
	LDA temp4
	LDY temp6
 jsr pfread
	BNE .skip80then
.condpart81
 jmp .__Skip_AI_Down

.skip80then
.skipL0217
.
 ; 

.L0218 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L0219 ;  if temp3  <  34 then if pfread ( temp3 , temp6 )  then goto __Skip_AI_Down

	LDA temp3
	CMP #34
     BCS .skipL0219
.condpart82
	LDA temp3
	LDY temp6
 jsr pfread
	BNE .skip82then
.condpart83
 jmp .__Skip_AI_Down

.skip82then
.skipL0219
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0220 ;  _Bit2_Mino_Moving{2}  =  1

	LDA _Bit2_Mino_Moving
	ORA #4
	STA _Bit2_Mino_Moving
.L0221 ;  player1y  =  player1y  +  1

	INC player1y
.
 ; 

.__Skip_AI_Down
 ; __Skip_AI_Down

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

.L0222 ;  if player1x  <=  player0x then goto __Skip_AI_Left

	LDA player0x
	CMP player1x
     BCC .skipL0222
.condpart84
 jmp .__Skip_AI_Left

.skipL0222
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0223 ;  _Bit6_P1_Dir_Left{6}  =  1

	LDA _Bit6_P1_Dir_Left
	ORA #64
	STA _Bit6_P1_Dir_Left
.L0224 ;  _Bit7_P1_Dir_Right{7}  =  0

	LDA _Bit7_P1_Dir_Right
	AND #127
	STA _Bit7_P1_Dir_Right
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0225 ;  if player1x  <=  _P_Edge_Left then goto __Skip_AI_Left

	LDA #_P_Edge_Left
	CMP player1x
     BCC .skipL0225
.condpart85
 jmp .__Skip_AI_Left

.skipL0225
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0226 ;  temp5  =   ( player1y - 1 )  / 8

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

.L0227 ;  temp6  =   ( player1x - 18 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #18
	lsr
	lsr
	STA temp6
.
 ; 

.L0228 ;  if temp6  <  34 then if pfread ( temp6 , temp5 )  then goto __Skip_AI_Left

	LDA temp6
	CMP #34
     BCS .skipL0228
.condpart86
	LDA temp6
	LDY temp5
 jsr pfread
	BNE .skip86then
.condpart87
 jmp .__Skip_AI_Left

.skip86then
.skipL0228
.
 ; 

.L0229 ;  temp3  =   ( player1y - 8 )  / 8

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

.L0230 ;  if temp6  <  34 then if pfread ( temp6 , temp3 )  then goto __Skip_AI_Left

	LDA temp6
	CMP #34
     BCS .skipL0230
.condpart88
	LDA temp6
	LDY temp3
 jsr pfread
	BNE .skip88then
.condpart89
 jmp .__Skip_AI_Left

.skip88then
.skipL0230
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0231 ;  _Bit2_Mino_Moving{2}  =  1

	LDA _Bit2_Mino_Moving
	ORA #4
	STA _Bit2_Mino_Moving
.L0232 ;  player1x  =  player1x  -  1

	DEC player1x
.
 ; 

.__Skip_AI_Left
 ; __Skip_AI_Left

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

.L0233 ;  if player1x  >=  player0x then goto __Skip_AI_Right

	LDA player1x
	CMP player0x
     BCC .skipL0233
.condpart90
 jmp .__Skip_AI_Right

.skipL0233
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0234 ;  _Bit6_P1_Dir_Left{6}  =  0

	LDA _Bit6_P1_Dir_Left
	AND #191
	STA _Bit6_P1_Dir_Left
.L0235 ;  _Bit7_P1_Dir_Right{7}  =  1

	LDA _Bit7_P1_Dir_Right
	ORA #128
	STA _Bit7_P1_Dir_Right
.
 ; 

.
 ; 

.
 ; 

.L0236 ;  if player1x  >=  _P_Edge_Right then goto __Skip_AI_Right

	LDA player1x
	CMP #_P_Edge_Right
     BCC .skipL0236
.condpart91
 jmp .__Skip_AI_Right

.skipL0236
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0237 ;  temp5  =   ( player1y - 1 )  / 8

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

.L0238 ;  temp6  =   ( player1x - 9 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #9
	lsr
	lsr
	STA temp6
.
 ; 

.L0239 ;  if temp6  <  34 then if pfread ( temp6 , temp5 )  then goto __Skip_AI_Right

	LDA temp6
	CMP #34
     BCS .skipL0239
.condpart92
	LDA temp6
	LDY temp5
 jsr pfread
	BNE .skip92then
.condpart93
 jmp .__Skip_AI_Right

.skip92then
.skipL0239
.
 ; 

.L0240 ;  temp3  =   ( player1y - 8 )  / 8

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

.L0241 ;  if temp6  <  34 then if pfread ( temp6 , temp3 )  then goto __Skip_AI_Right

	LDA temp6
	CMP #34
     BCS .skipL0241
.condpart94
	LDA temp6
	LDY temp3
 jsr pfread
	BNE .skip94then
.condpart95
 jmp .__Skip_AI_Right

.skip94then
.skipL0241
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0242 ;  _Bit2_Mino_Moving{2}  =  1

	LDA _Bit2_Mino_Moving
	ORA #4
	STA _Bit2_Mino_Moving
.L0243 ;  player1x  =  player1x  +  1

	INC player1x
.
 ; 

.__Skip_AI_Right
 ; __Skip_AI_Right

.
 ; 

.L0244 ;  if _Bit6_P1_Dir_Left{6} then REFP1  =  8

	BIT _Bit6_P1_Dir_Left
	BVC .skipL0244
.condpart96
	LDA #8
	STA REFP1
.skipL0244
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

.L0245 ;  if !collision(missile0,player0) then __Skip_Gem_Collection

	bit 	CXM0P
 if ((* - .__Skip_Gem_Collection) < 127) && ((* - .__Skip_Gem_Collection) > -128)
	bvc .__Skip_Gem_Collection
 else
	bvs .5skip__Skip_Gem_Collection
	jmp .__Skip_Gem_Collection
.5skip__Skip_Gem_Collection
 endif
.L0246 ;  _Bit0_Carrying_Gem{0}  =  1

	LDA _Bit0_Carrying_Gem
	ORA #1
	STA _Bit0_Carrying_Gem
.L0247 ;  missile0x  =  200  :  missile0y  =  200

	LDA #200
	STA missile0x
	STA missile0y
.
 ; 

.
 ; 

.
 ; 

.L0248 ;  if _Ch0_Sound  <>  1 then _Ch0_Sound  =  1  :  _Ch0_Duration  =  1  :  _Ch0_Counter  =  0

	LDA _Ch0_Sound
	CMP #1
     BEQ .skipL0248
.condpart97
	LDA #1
	STA _Ch0_Sound
	STA _Ch0_Duration
	LDA #0
	STA _Ch0_Counter
.skipL0248
.__Skip_Gem_Collection
 ; __Skip_Gem_Collection

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0249 ;  if !collision(missile1,player0) then __Skip_Gem_Deposit

	bit 	CXM1P
 if ((* - .__Skip_Gem_Deposit) < 127) && ((* - .__Skip_Gem_Deposit) > -128)
	bpl .__Skip_Gem_Deposit
 else
	bmi .6skip__Skip_Gem_Deposit
	jmp .__Skip_Gem_Deposit
.6skip__Skip_Gem_Deposit
 endif
.L0250 ;  if !_Bit0_Carrying_Gem{0} then __Skip_Gem_Deposit

	LDA _Bit0_Carrying_Gem
	LSR
 if ((* - .__Skip_Gem_Deposit) < 127) && ((* - .__Skip_Gem_Deposit) > -128)
	bcc .__Skip_Gem_Deposit
 else
	bcs .7skip__Skip_Gem_Deposit
	jmp .__Skip_Gem_Deposit
.7skip__Skip_Gem_Deposit
 endif
.L0251 ;  _Bit0_Carrying_Gem{0}  =  0

	LDA _Bit0_Carrying_Gem
	AND #254
	STA _Bit0_Carrying_Gem
.L0252 ;  score  =  score  +  1

	SED
	CLC
	LDA score+2
	ADC #$01
	STA score+2
	LDA score+1
	ADC #$00
	STA score+1
	LDA score
	ADC #$00
	STA score
	CLD
.L0253 ;  _Bit3_New_Chase_Start{3}  =  0

	LDA _Bit3_New_Chase_Start
	AND #247
	STA _Bit3_New_Chase_Start
.L0254 ;  if _Ch0_Sound  <>  2 then _Ch0_Sound  =  2  :  _Ch0_Duration  =  1  :  _Ch0_Counter  =  0

	LDA _Ch0_Sound
	CMP #2
     BEQ .skipL0254
.condpart98
	LDA #2
	STA _Ch0_Sound
	LDA #1
	STA _Ch0_Duration
	LDA #0
	STA _Ch0_Counter
.skipL0254
.L0255 ;  missile0x  =   ( rand  &  118 )   +  23  :  missile0y  =   ( rand  &  70 )   +  9

; complex statement detected
 jsr randomize
	AND #118
	CLC
	ADC #23
	STA missile0x
; complex statement detected
 jsr randomize
	AND #70
	CLC
	ADC #9
	STA missile0y
.__Skip_Gem_Deposit
 ; __Skip_Gem_Deposit

.
 ; 

.__Game_Over
 ; __Game_Over

.L0256 ;  if !_Bit4_Game_Over{4} then goto __Skip_Game_Over

	LDA _Bit4_Game_Over
	AND #16
	BNE .skipL0256
.condpart99
 jmp .__Skip_Game_Over

.skipL0256
.L0257 ;  if _Ch0_Sound  =  3  ||  _Bit5_Game_Over_Music_Played{5} then goto __Skip_Game_Over_Music

	LDA _Ch0_Sound
	CMP #3
     BNE .skipL0257
.condpart100
 jmp .condpart101
.skipL0257
	LDA _Bit5_Game_Over_Music_Played
	AND #32
	BEQ .skip11OR
.condpart101
 jmp .__Skip_Game_Over_Music

.skip11OR
.L0258 ;  _Ch0_Sound  =  3  :  _Ch0_Duration  =  1  :  _Ch0_Counter  =  0

	LDA #3
	STA _Ch0_Sound
	LDA #1
	STA _Ch0_Duration
	LDA #0
	STA _Ch0_Counter
.L0259 ;  _Bit5_Game_Over_Music_Played{5}  =  1

	LDA _Bit5_Game_Over_Music_Played
	ORA #32
	STA _Bit5_Game_Over_Music_Played
.__Skip_Game_Over_Music
 ; __Skip_Game_Over_Music

.L0260 ;  COLUPF  =  14

	LDA #14
	STA COLUPF
.L0261 ;  if _Bit6_Game_Won{6} then goto __Win_Screen

	BIT _Bit6_Game_Won
	BVC .skipL0261
.condpart102
 jmp .__Win_Screen

.skipL0261
.L0262 ;  scorecolor  =  14

	LDA #14
	STA scorecolor
.L0263 ;  COLUBK  =  $36

	LDA #$36
	STA COLUBK
.L0264 ;  playfield:

  ifconst pfres
	  ldx #(11>pfres)*(pfres*pfwidth-1)+(11<=pfres)*43
  else
	  ldx #((11*pfwidth-1)*((11*pfwidth-1)<47))+(47*((11*pfwidth-1)>=47))
  endif
	jmp pflabel1
PF_data1
	.byte %00001111, %01111001
	if (pfwidth>2)
	.byte %01101100, %00001111
 endif
	.byte %00001000, %01001000
	if (pfwidth>2)
	.byte %01010100, %00000001
 endif
	.byte %00001001, %01111001
	if (pfwidth>2)
	.byte %01010100, %00000111
 endif
	.byte %00001000, %01001001
	if (pfwidth>2)
	.byte %01010100, %00000001
 endif
	.byte %00001111, %01001001
	if (pfwidth>2)
	.byte %01010100, %00001111
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00001111, %10001001
	if (pfwidth>2)
	.byte %00111100, %00000111
 endif
	.byte %00001000, %10001001
	if (pfwidth>2)
	.byte %00100000, %00001001
 endif
	.byte %00001000, %10001001
	if (pfwidth>2)
	.byte %00111000, %00000111
 endif
	.byte %00001000, %01010001
	if (pfwidth>2)
	.byte %00100000, %00001001
 endif
	.byte %00001111, %00100001
	if (pfwidth>2)
	.byte %00111100, %00001001
 endif
pflabel1
	lda PF_data1,x
	sta playfield,x
	dex
	bpl pflabel1
.__Win_Screen
 ; __Win_Screen

.L0265 ;  if !_Bit6_Game_Won{6} goto __Skip_Win_Screen

	BIT _Bit6_Game_Won
 if ((* - .__Skip_Win_Screen) < 127) && ((* - .__Skip_Win_Screen) > -128)
	bvc .__Skip_Win_Screen
 else
	bvs .8skip__Skip_Win_Screen
	jmp .__Skip_Win_Screen
.8skip__Skip_Win_Screen
 endif
.L0266 ;  COLUBK  =  $C4

	LDA #$C4
	STA COLUBK
.L0267 ;  scorecolor  =  $C4

	LDA #$C4
	STA scorecolor
.L0268 ;  playfield:

  ifconst pfres
	  ldx #(11>pfres)*(pfres*pfwidth-1)+(11<=pfres)*43
  else
	  ldx #((11*pfwidth-1)*((11*pfwidth-1)<47))+(47*((11*pfwidth-1)>=47))
  endif
	jmp pflabel2
PF_data2
	.byte %00001000, %11000100
	if (pfwidth>2)
	.byte %11000100, %00001000
 endif
	.byte %00000100, %00100010
	if (pfwidth>2)
	.byte %00100100, %00001000
 endif
	.byte %00000011, %00100001
	if (pfwidth>2)
	.byte %00100100, %00001000
 endif
	.byte %00000001, %00100000
	if (pfwidth>2)
	.byte %00100100, %00001000
 endif
	.byte %00000001, %11000000
	if (pfwidth>2)
	.byte %11000011, %00000111
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000100, %11001000
	if (pfwidth>2)
	.byte %11100100, %00000100
 endif
	.byte %00000100, %00001000
	if (pfwidth>2)
	.byte %10000110, %00000100
 endif
	.byte %00000100, %00001001
	if (pfwidth>2)
	.byte %10000101, %00000100
 endif
	.byte %00000100, %00001001
	if (pfwidth>2)
	.byte %10000100, %00000101
 endif
	.byte %00000011, %11000110
	if (pfwidth>2)
	.byte %11100100, %00000110
 endif
pflabel2
	lda PF_data2,x
	sta playfield,x
	dex
	bpl pflabel2
.__Skip_Win_Screen
 ; __Skip_Win_Screen

.L0269 ;  player0x  =  200  :  player0y  =  200

	LDA #200
	STA player0x
	STA player0y
.L0270 ;  player1x  =  200  :  player1y  =  200

	LDA #200
	STA player1x
	STA player1y
.L0271 ;  missile0x  =  200  :  missile0y  =  200

	LDA #200
	STA missile0x
	STA missile0y
.L0272 ;  missile1x  =  200  :  missile1y  =  200

	LDA #200
	STA missile1x
	STA missile1y
.__Skip_Game_Over
 ; __Skip_Game_Over

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

.L0273 ;  drawscreen

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

.L0274 ;  if !switchreset then _Bit0_Reset_Restrainer{0}  =  0  :  goto __Main_Loop

 lda #1
 bit SWCHB
	BEQ .skipL0274
.condpart103
	LDA _Bit0_Reset_Restrainer
	AND #254
	STA _Bit0_Reset_Restrainer
 jmp .__Main_Loop

.skipL0274
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

.L0275 ;  if _Bit0_Reset_Restrainer{0} then goto __Main_Loop

	LDA _Bit0_Reset_Restrainer
	LSR
	BCC .skipL0275
.condpart104
 jmp .__Main_Loop

.skipL0275
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0276 ;  goto __Start_Restart

 jmp .__Start_Restart

.
 ; 

.__L0
 ; __L0

.L0277 ;  playfield:

  ifconst pfres
	  ldx #(11>pfres)*(pfres*pfwidth-1)+(11<=pfres)*43
  else
	  ldx #((11*pfwidth-1)*((11*pfwidth-1)<47))+(47*((11*pfwidth-1)>=47))
  endif
	jmp pflabel3
PF_data3
	.byte %11111111, %11111111
	if (pfwidth>2)
	.byte %11111111, %11111111
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %10010011, %01100001
	if (pfwidth>2)
	.byte %01100001, %10010011
 endif
	.byte %10010000, %01100000
	if (pfwidth>2)
	.byte %01100000, %10010000
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %10011001, %00011001
	if (pfwidth>2)
	.byte %00011001, %10011001
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %10010000, %01100000
	if (pfwidth>2)
	.byte %01100000, %10010000
 endif
	.byte %10010011, %01100001
	if (pfwidth>2)
	.byte %01100001, %10010011
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %11111111, %11111111
	if (pfwidth>2)
	.byte %11111111, %11111111
 endif
pflabel3
	lda PF_data3,x
	sta playfield,x
	dex
	bpl pflabel3
.L0278 ;  goto __L_Done

 jmp .__L_Done

.__L1
 ; __L1

.L0279 ;  COLUPF  =  $12

	LDA #$12
	STA COLUPF
.L0280 ;  playfield:

  ifconst pfres
	  ldx #(11>pfres)*(pfres*pfwidth-1)+(11<=pfres)*43
  else
	  ldx #((11*pfwidth-1)*((11*pfwidth-1)<47))+(47*((11*pfwidth-1)>=47))
  endif
	jmp pflabel4
PF_data4
	.byte %11111111, %11111111
	if (pfwidth>2)
	.byte %11111111, %11111111
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %10010000, %10000010
	if (pfwidth>2)
	.byte %10000010, %10010000
 endif
	.byte %10000010, %10000000
	if (pfwidth>2)
	.byte %10000000, %10000010
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %10010000, %00000010
	if (pfwidth>2)
	.byte %00000010, %10010000
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %10000010, %10000000
	if (pfwidth>2)
	.byte %10000000, %10000010
 endif
	.byte %10010000, %10000010
	if (pfwidth>2)
	.byte %10000010, %10010000
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %11111111, %11111111
	if (pfwidth>2)
	.byte %11111111, %11111111
 endif
pflabel4
	lda PF_data4,x
	sta playfield,x
	dex
	bpl pflabel4
.L0281 ;  goto __L_Done

 jmp .__L_Done

.__L2
 ; __L2

.L0282 ;  COLUPF  =  $10

	LDA #$10
	STA COLUPF
.L0283 ;  playfield:

  ifconst pfres
	  ldx #(11>pfres)*(pfres*pfwidth-1)+(11<=pfres)*43
  else
	  ldx #((11*pfwidth-1)*((11*pfwidth-1)<47))+(47*((11*pfwidth-1)>=47))
  endif
	jmp pflabel5
PF_data5
	.byte %11111111, %11111111
	if (pfwidth>2)
	.byte %11111111, %11111111
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %10000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %10000000
 endif
	.byte %11111111, %11111111
	if (pfwidth>2)
	.byte %11111111, %11111111
 endif
pflabel5
	lda PF_data5,x
	sta playfield,x
	dex
	bpl pflabel5
.L0284 ;  goto __L_Done

 jmp .__L_Done

.
 ; 

.
 ; 

.__Pl00
 ; __Pl00

.L0285 ;  player0:

	LDX #<playerL0285_0
	STX player0pointerlo
	LDA #>playerL0285_0
	STA player0pointerhi
	LDA #7
	STA player0height
.L0286 ;  goto __Pl_Frame_Done

 jmp .__Pl_Frame_Done

.__Pl01
 ; __Pl01

.L0287 ;  player0:

	LDX #<playerL0287_0
	STX player0pointerlo
	LDA #>playerL0287_0
	STA player0pointerhi
	LDA #7
	STA player0height
.L0288 ;  goto __Pl_Frame_Done

 jmp .__Pl_Frame_Done

.__Pl02
 ; __Pl02

.L0289 ;  player0:

	LDX #<playerL0289_0
	STX player0pointerlo
	LDA #>playerL0289_0
	STA player0pointerhi
	LDA #7
	STA player0height
.L0290 ;  goto __Pl_Frame_Done

 jmp .__Pl_Frame_Done

.
 ; 

.__Mn00
 ; __Mn00

.L0291 ;  player1:

	LDX #<playerL0291_1
	STX player1pointerlo
	LDA #>playerL0291_1
	STA player1pointerhi
	LDA #7
	STA player1height
.L0292 ;  goto __Mn_Frame_Done

 jmp .__Mn_Frame_Done

.__Mn01
 ; __Mn01

.L0293 ;  player1:

	LDX #<playerL0293_1
	STX player1pointerlo
	LDA #>playerL0293_1
	STA player1pointerhi
	LDA #7
	STA player1height
.L0294 ;  goto __Mn_Frame_Done

 jmp .__Mn_Frame_Done

.__Mn02
 ; __Mn02

.L0295 ;  player1:

	LDX #<playerL0295_1
	STX player1pointerlo
	LDA #>playerL0295_1
	STA player1pointerhi
	LDA #7
	STA player1height
.L0296 ;  goto __Mn_Frame_Done

 jmp .__Mn_Frame_Done

.
 ; 

.L0297 ;  data _SD_Gem_Coll

	JMP .skipL0297
_SD_Gem_Coll
	.byte    8,4,7

	.byte    4

	.byte    8,4,6

	.byte    8

	.byte    255

.skipL0297
.L0298 ;  data _SD_Gem_Drop

	JMP .skipL0298
_SD_Gem_Drop
	.byte    8,4,6

	.byte    4

	.byte    8,4,7

	.byte    8

	.byte    8,4,4

	.byte    4

	.byte    255

.skipL0298
.L0299 ;  data _SD_Mino_Roar

	JMP .skipL0299
_SD_Mino_Roar
	.byte    8,14,5

	.byte    12

	.byte    8,14,4

	.byte    8

	.byte    8,14,6

	.byte    4

	.byte    8,14,3

	.byte    12

	.byte    8,14,4

	.byte    8

	.byte    255

.skipL0299
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL038_0
	.byte    %01100110
	.byte    %00100100
	.byte    %10011001
	.byte    %10111101
	.byte    %01111110
	.byte    %00011000
	.byte    %00111100
	.byte    %00100100
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL039_1
	.byte    %11111111
	.byte    %00000000
	.byte    %11111111
	.byte    %10000001
	.byte    %01111110
	.byte    %11000011
	.byte    %00111100
	.byte    %11100111
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL086_0
	.byte    %01100110
	.byte    %00100100
	.byte    %00100100
	.byte    %01011010
	.byte    %01011010
	.byte    %00111100
	.byte    %00011000
	.byte    %00011000
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL089_1
	.byte    %01100110
	.byte    %00100100
	.byte    %10011001
	.byte    %10111101
	.byte    %01111110
	.byte    %00011000
	.byte    %00111100
	.byte    %00100100
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0285_0
	.byte    %00110110
	.byte    %00100100
	.byte    %00100100
	.byte    %01011010
	.byte    %01011010
	.byte    %00111100
	.byte    %00011000
	.byte    %00011000
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0287_0
	.byte    %00110000
	.byte    %00100110
	.byte    %00100100
	.byte    %00011010
	.byte    %01011010
	.byte    %00111100
	.byte    %00011000
	.byte    %00011000
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0289_0
	.byte    %00000110
	.byte    %00110100
	.byte    %00100100
	.byte    %01011000
	.byte    %01011010
	.byte    %00111100
	.byte    %00011000
	.byte    %00011000
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0291_1
	.byte    %00110110
	.byte    %00100100
	.byte    %10011001
	.byte    %10111101
	.byte    %01111110
	.byte    %00011000
	.byte    %00111100
	.byte    %00100100
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0293_1
	.byte    %00000110
	.byte    %00110100
	.byte    %00011001
	.byte    %10111101
	.byte    %01111110
	.byte    %00011000
	.byte    %00111100
	.byte    %00100100
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL0295_1
	.byte    %00110000
	.byte    %00100110
	.byte    %10011000
	.byte    %10111101
	.byte    %01111110
	.byte    %00011000
	.byte    %00111100
	.byte    %00100100
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
