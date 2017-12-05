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

.L01 ;  dim _BitOp_P0_M0_Dir  =  g

.L02 ;  dim _Bit0_P0_Dir_Up  =  g

.L03 ;  dim _Bit1_P0_Dir_Down  =  g

.L04 ;  dim _Bit2_P0_Dir_Left  =  g

.L05 ;  dim _Bit3_P0_Dir_Right  =  g

.L06 ;  dim _Bit4_M0_Dir_Up  =  g

.L07 ;  dim _Bit5_M0_Dir_Down  =  g

.L08 ;  dim _Bit6_M0_Dir_Left  =  g

.L09 ;  dim _Bit7_M0_Dir_Right  =  g

.
 ; 

.
 ; 

.
 ; 

.L010 ;  dim _BitOp_P1_M1_Dir  =  k

.L011 ;  dim _Bit0_P1_Dir_Up  =  k

.L012 ;  dim _Bit1_P1_Dir_Down  =  k

.L013 ;  dim _Bit2_P1_Dir_Left  =  k

.L014 ;  dim _Bit3_P1_Dir_Right  =  k

.L015 ;  dim _Bit4_P1_Col_Up  =  k

.L016 ;  dim _Bit5_P1_Col_Down  =  k

.L017 ;  dim _Bit6_P1_Col_Left  =  k

.L018 ;  dim _Bit7_P1_Col_Right  =  k

.
 ; 

.L019 ;  dim _Master_Counter  =  a

.L020 ;  dim _Frame_Counter  =  b

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L021 ;  const _Minotaur_Awareness_Size  =  100

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L022 ;  l  =  $15

	LDA #$15
	STA l
.L023 ;  m  =  $30

	LDA #$30
	STA m
.L024 ;  dim _Score_Phase_1  =  m

.L025 ;  dim _Score_Phase_2  =  l

.L026 ;  dim _Bit0_Carrying_Gem  =  x

.L027 ;  dim _Bit1_Game_Over  =  x

.L028 ;  dim _Bit2_Player_Moving  =  x

.L029 ;  dim _Bit3_Mino_Moving  =  x

.L030 ;  dim _Bit4_New_Chase_Start  =  x

.L031 ;  dim _Bit5_Game_Over_Music_Played  =  x

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L032 ;  dim _BitOp_01  =  y

.L033 ;  dim _Bit0_Reset_Restrainer  =  y

.L034 ;  dim _Bit1_Toggle_Screen  =  y

.
 ; 

.
 ; 

.
 ; 

.L035 ;  dim _Ch0_Sound  =  q

.L036 ;  dim _Ch0_Duration  =  r

.L037 ;  dim _Ch0_Counter  =  s

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L038 ;  dim rand16  =  z

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L039 ;  const _P_Edge_Top  =  9

.L040 ;  const _P_Edge_Bottom  =  88

.L041 ;  const _P_Edge_Left  =  1

.L042 ;  const _P_Edge_Right  =  153

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L043 ;  const _B_Edge_Top  =  2

.L044 ;  const _B_Edge_Bottom  =  88

.L045 ;  const _B_Edge_Left  =  2

.L046 ;  const _B_Edge_Right  =  160

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L047 ;  const _M_Edge_Top  =  2

.L048 ;  const _M_Edge_Bottom  =  88

.L049 ;  const _M_Edge_Left  =  2

.L050 ;  const _M_Edge_Right  =  159

.
 ; 

.
 ; 

.
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

.L051 ;  AUDV0  =  0  :  AUDV1  =  0

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

.L052 ;  a  =  0  :  b  =  0  :  c  =  0  :  d  =  0  :  e  =  0  :  f  =  0  :  g  =  0  :  h  =  0  :  i  =  0

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
.L053 ;  j  =  0  :  k  =  0  :  l  =  0  :  m  =  0  :  n  =  0  :  o  =  0  :  p  =  0  :  q  =  0  :  r  =  0

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
.L054 ;  s  =  0  :  t  =  0  :  u  =  0  :  v  =  0  :  w  =  0  :  x  =  0

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

.L055 ;  _BitOp_01  =  _BitOp_01  &  %00010000

	LDA _BitOp_01
	AND #%00010000
	STA _BitOp_01
.
 ; 

.
 ; 

.__Title_Screen
 ; __Title_Screen

.L056 ;  playfield:

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
.L057 ;  player0:

	LDX #<playerL057_0
	STX player0pointerlo
	LDA #>playerL057_0
	STA player0pointerhi
	LDA #7
	STA player0height
.L058 ;  player1:

	LDX #<playerL058_1
	STX player1pointerlo
	LDA #>playerL058_1
	STA player1pointerhi
	LDA #7
	STA player1height
.
 ; 

.__Start_Screen_Loop
 ; __Start_Screen_Loop

.L059 ;  if _Master_Counter  <=  10 then goto __Skip_Joy_Checks

	LDA #10
	CMP _Master_Counter
     BCC .skipL059
.condpart0
 jmp .__Skip_Joy_Checks

.skipL059
.L060 ;  if joy0up then goto __Skip_Start_Screen

 lda #$10
 bit SWCHA
	BNE .skipL060
.condpart1
 jmp .__Skip_Start_Screen

.skipL060
.L061 ;  if joy0down then goto __Skip_Start_Screen

 lda #$20
 bit SWCHA
	BNE .skipL061
.condpart2
 jmp .__Skip_Start_Screen

.skipL061
.L062 ;  if joy0right then goto __Skip_Start_Screen

 bit SWCHA
	BMI .skipL062
.condpart3
 jmp .__Skip_Start_Screen

.skipL062
.L063 ;  if joy0left then goto __Skip_Start_Screen

 bit SWCHA
	BVS .skipL063
.condpart4
 jmp .__Skip_Start_Screen

.skipL063
.L064 ;  if joy0fire then goto __Skip_Start_Screen

 bit INPT4
	BMI .skipL064
.condpart5
 jmp .__Skip_Start_Screen

.skipL064
.__Skip_Joy_Checks
 ; __Skip_Joy_Checks

.L065 ;  player1x  =  77

	LDA #77
	STA player1x
.L066 ;  player1y  =  80

	LDA #80
	STA player1y
.L067 ;  player0x  =  77

	LDA #77
	STA player0x
.L068 ;  player0y  =  72

	LDA #72
	STA player0y
.L069 ;  COLUP0  =  $36

	LDA #$36
	STA COLUP0
.L070 ;  COLUP1  =  $14

	LDA #$14
	STA COLUP1
.L071 ;  COLUPF  =  $14

	LDA #$14
	STA COLUPF
.L072 ;  COLUBK  =  0

	LDA #0
	STA COLUBK
.L073 ;  scorecolor  =  0

	LDA #0
	STA scorecolor
.L074 ;  _Master_Counter  =  _Master_Counter  +  1

	INC _Master_Counter
.L075 ;  drawscreen

 jsr drawscreen
.L076 ;  goto __Start_Screen_Loop

 jmp .__Start_Screen_Loop

.
 ; 

.L077 ;  _Master_Counter  =  0  :  _Frame_Counter  =  0

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

.L078 ;  player0x  =  77  :  player0y  =  60

	LDA #77
	STA player0x
	LDA #60
	STA player0y
.L079 ;  player1x  =  21  :  player1y  =  16

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

.L080 ;  missile0x  =  200  :  missile0y  =  200

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

.L081 ;  missile0x  =  80  :  missile0y  =  79

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

.L082 ;  NUSIZ0  =  $10  :  missile0height  =  1

	LDA #$10
	STA NUSIZ0
	LDA #1
	STA missile0height
.L083 ;  NUSIZ1  =  $30  :  missile0height  =  4

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

.L084 ;  COLUPF  =  $14

	LDA #$14
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

.L085 ;  COLUBK  =  0

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

.L086 ;  _Bit3_P0_Dir_Right{3}  =  1

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

.L087 ;  _Bit0_Reset_Restrainer{0}  =  1

	LDA _Bit0_Reset_Restrainer
	ORA #1
	STA _Bit0_Reset_Restrainer
.
 ; 

.L088 ;  playfield:

  ifconst pfres
	  ldx #(11>pfres)*(pfres*pfwidth-1)+(11<=pfres)*43
  else
	  ldx #((11*pfwidth-1)*((11*pfwidth-1)<47))+(47*((11*pfwidth-1)>=47))
  endif
	jmp pflabel1
PF_data1
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
pflabel1
	lda PF_data1,x
	sta playfield,x
	dex
	bpl pflabel1
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L089 ;  score  =  0

	LDA #$00
	STA score+2
	LDA #$00
	STA score+1
	LDA #$00
	STA score
.
 ; 

.L090 ;  dim _sc1  =  score

.L091 ;  dim _sc2  =  score + 1

.L092 ;  dim _sc3  =  score + 2

.
 ; 

.L093 ;  t  =  $3

	LDA #$3
	STA t
.L094 ;  dim _Chase_Speed  =  t

.
 ; 

.
 ; 

.
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

.L095 ;  if _Bit1_Game_Over{1} then if joy0fire goto __Start_Restart

	LDA _Bit1_Game_Over
	AND #2
	BEQ .skipL095
.condpart6
 bit INPT4
 if ((* - .__Start_Restart) < 127) && ((* - .__Start_Restart) > -128)
	bpl .__Start_Restart
 else
	bmi .0skip__Start_Restart
	jmp .__Start_Restart
.0skip__Start_Restart
 endif
.skipL095
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L096 ;  _Master_Counter  =  _Master_Counter  +  1

	INC _Master_Counter
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L097 ;  if _Master_Counter  <  7 then goto __Skip_Counters

	LDA _Master_Counter
	CMP #7
     BCS .skipL097
.condpart7
 jmp .__Skip_Counters

.skipL097
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L098 ;  _Frame_Counter  =  _Frame_Counter  +  1  :  _Master_Counter  =  0

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

.L099 ;  if _Frame_Counter  >  3 then _Frame_Counter  =  0

	LDA #3
	CMP _Frame_Counter
     BCS .skipL099
.condpart8
	LDA #0
	STA _Frame_Counter
.skipL099
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

.L0100 ;  player0:

	LDX #<playerL0100_0
	STX player0pointerlo
	LDA #>playerL0100_0
	STA player0pointerhi
	LDA #7
	STA player0height
.L0101 ;  if !_Bit2_Player_Moving{2} goto __Pl_Frame_Done

	LDA _Bit2_Player_Moving
	AND #4
 if ((* - .__Pl_Frame_Done) < 127) && ((* - .__Pl_Frame_Done) > -128)
	BEQ .__Pl_Frame_Done
 else
	bne .1skip__Pl_Frame_Done
	jmp .__Pl_Frame_Done
.1skip__Pl_Frame_Done
 endif
.L0102 ;  on _Frame_Counter goto __Pl00 __Pl01 __Pl00 __Pl02

	LDX _Frame_Counter
	LDA .L0102jumptablehi,x
	PHA
	LDA .L0102jumptablelo,x
	PHA
	RTS
.L0102jumptablehi
	.byte >(.__Pl00-1)
	.byte >(.__Pl01-1)
	.byte >(.__Pl00-1)
	.byte >(.__Pl02-1)
.L0102jumptablelo
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

.L0103 ;  player1:

	LDX #<playerL0103_1
	STX player1pointerlo
	LDA #>playerL0103_1
	STA player1pointerhi
	LDA #7
	STA player1height
.L0104 ;  if !_Bit3_Mino_Moving{3} goto __Mn_Frame_Done

	LDA _Bit3_Mino_Moving
	AND #8
 if ((* - .__Mn_Frame_Done) < 127) && ((* - .__Mn_Frame_Done) > -128)
	BEQ .__Mn_Frame_Done
 else
	bne .2skip__Mn_Frame_Done
	jmp .__Mn_Frame_Done
.2skip__Mn_Frame_Done
 endif
.L0105 ;  on _Frame_Counter goto __Mn00 __Mn01 __Mn00 __Mn02

	LDX _Frame_Counter
	LDA .L0105jumptablehi,x
	PHA
	LDA .L0105jumptablelo,x
	PHA
	RTS
.L0105jumptablehi
	.byte >(.__Mn00-1)
	.byte >(.__Mn01-1)
	.byte >(.__Mn00-1)
	.byte >(.__Mn02-1)
.L0105jumptablelo
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

.L0106 ;  if !_Ch0_Sound then goto __Skip_Ch_0

	LDA _Ch0_Sound
	BNE .skipL0106
.condpart9
 jmp .__Skip_Ch_0

.skipL0106
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0107 ;  _Ch0_Duration  =  _Ch0_Duration  -  1

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

.L0108 ;  if _Ch0_Duration then goto __Skip_Ch_0

	LDA _Ch0_Duration
	BEQ .skipL0108
.condpart10
 jmp .__Skip_Ch_0

.skipL0108
.
 ; 

.L0109 ;  if _Ch0_Sound  <>  1 then goto __Skip_Ch0_Sound_001

	LDA _Ch0_Sound
	CMP #1
     BEQ .skipL0109
.condpart11
 jmp .__Skip_Ch0_Sound_001

.skipL0109
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0110 ;  temp4  =  _SD_Gem_Coll[_Ch0_Counter]

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

.L0111 ;  if temp4  =  255 then goto __Clear_Ch_0

	LDA temp4
	CMP #255
     BNE .skipL0111
.condpart12
 jmp .__Clear_Ch_0

.skipL0111
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0112 ;  _Ch0_Counter  =  _Ch0_Counter  +  1

	INC _Ch0_Counter
.L0113 ;  temp5  =  _SD_Gem_Coll[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

	LDX _Ch0_Counter
	LDA _SD_Gem_Coll,x
	STA temp5
	INC _Ch0_Counter
.L0114 ;  temp6  =  _SD_Gem_Coll[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

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

.L0115 ;  AUDV0  =  temp4

	LDA temp4
	STA AUDV0
.L0116 ;  AUDC0  =  temp5

	LDA temp5
	STA AUDC0
.L0117 ;  AUDF0  =  temp6

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

.L0118 ;  _Ch0_Duration  =  _SD_Gem_Coll[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

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

.L0119 ;  goto __Skip_Ch_0

 jmp .__Skip_Ch_0

.
 ; 

.__Skip_Ch0_Sound_001
 ; __Skip_Ch0_Sound_001

.
 ; 

.L0120 ;  if _Ch0_Sound  <>  2 then goto __Skip_Ch0_Sound_002

	LDA _Ch0_Sound
	CMP #2
     BEQ .skipL0120
.condpart13
 jmp .__Skip_Ch0_Sound_002

.skipL0120
.
 ; 

.
 ; 

.
 ; 

.L0121 ;  temp4  =  _SD_Gem_Drop[_Ch0_Counter]

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

.L0122 ;  if temp4  =  255 then goto __Clear_Ch_0

	LDA temp4
	CMP #255
     BNE .skipL0122
.condpart14
 jmp .__Clear_Ch_0

.skipL0122
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0123 ;  _Ch0_Counter  =  _Ch0_Counter  +  1

	INC _Ch0_Counter
.L0124 ;  temp5  =  _SD_Gem_Drop[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

	LDX _Ch0_Counter
	LDA _SD_Gem_Drop,x
	STA temp5
	INC _Ch0_Counter
.L0125 ;  temp6  =  _SD_Gem_Drop[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

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

.L0126 ;  AUDV0  =  temp4

	LDA temp4
	STA AUDV0
.L0127 ;  AUDC0  =  temp5

	LDA temp5
	STA AUDC0
.L0128 ;  AUDF0  =  temp6

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

.L0129 ;  _Ch0_Duration  =  _SD_Gem_Drop[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

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

.L0130 ;  goto __Skip_Ch_0

 jmp .__Skip_Ch_0

.
 ; 

.__Skip_Ch0_Sound_002
 ; __Skip_Ch0_Sound_002

.
 ; 

.L0131 ;  if _Ch0_Sound  <>  3 then goto __Skip_Ch0_Sound_003

	LDA _Ch0_Sound
	CMP #3
     BEQ .skipL0131
.condpart15
 jmp .__Skip_Ch0_Sound_003

.skipL0131
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0132 ;  temp4  =  _SD_Mino_Roar[_Ch0_Counter]

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

.L0133 ;  if temp4  =  255 then goto __Clear_Ch_0

	LDA temp4
	CMP #255
     BNE .skipL0133
.condpart16
 jmp .__Clear_Ch_0

.skipL0133
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0134 ;  _Ch0_Counter  =  _Ch0_Counter  +  1

	INC _Ch0_Counter
.L0135 ;  temp5  =  _SD_Mino_Roar[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

	LDX _Ch0_Counter
	LDA _SD_Mino_Roar,x
	STA temp5
	INC _Ch0_Counter
.L0136 ;  temp6  =  _SD_Mino_Roar[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

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

.L0137 ;  AUDV0  =  temp4

	LDA temp4
	STA AUDV0
.L0138 ;  AUDC0  =  temp5

	LDA temp5
	STA AUDC0
.L0139 ;  AUDF0  =  temp6

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

.L0140 ;  _Ch0_Duration  =  _SD_Mino_Roar[_Ch0_Counter]  :  _Ch0_Counter  =  _Ch0_Counter  +  1

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

.L0141 ;  goto __Skip_Ch_0

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

.L0142 ;  goto __Skip_Ch_0

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

.L0143 ;  _Ch0_Sound  =  0  :  AUDV0  =  0

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

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0144 ;  if !collision(player0,player1) then __Skip_Player_Caught

	bit 	CXPPMM
 if ((* - .__Skip_Player_Caught) < 127) && ((* - .__Skip_Player_Caught) > -128)
	bpl .__Skip_Player_Caught
 else
	bmi .3skip__Skip_Player_Caught
	jmp .__Skip_Player_Caught
.3skip__Skip_Player_Caught
 endif
.L0145 ;  _Bit1_Game_Over{1}  =  1

	LDA _Bit1_Game_Over
	ORA #2
	STA _Bit1_Game_Over
.L0146 ;  goto __Game_Over

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

.L0147 ;  NUSIZ0  =  $20  :  missile0height  =  2

	LDA #$20
	STA NUSIZ0
	LDA #2
	STA missile0height
.L0148 ;  NUSIZ1  =  $30  :  missile1height  =  7

	LDA #$30
	STA NUSIZ1
	LDA #7
	STA missile1height
.
 ; 

.L0149 ;  missile1x  =  78  :  missile1y  =  48

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

.L0150 ;  COLUP0  =  $9C

	LDA #$9C
	STA COLUP0
.L0151 ;  if _Chase_Speed  >  $1 then COLUP1  =  $36

	LDA #$1
	CMP _Chase_Speed
     BCS .skipL0151
.condpart17
	LDA #$36
	STA COLUP1
.skipL0151
.L0152 ;  if _Chase_Speed  =  $1 then COLUP1  =  $35

	LDA _Chase_Speed
	CMP #$1
     BNE .skipL0152
.condpart18
	LDA #$35
	STA COLUP1
.skipL0152
.L0153 ;  if _Chase_Speed  =  $0 then COLUP1  =  $33

	LDA _Chase_Speed
	CMP #$0
     BNE .skipL0153
.condpart19
	LDA #$33
	STA COLUP1
.skipL0153
.L0154 ;  scorecolor  =  $9C

	LDA #$9C
	STA scorecolor
.
 ; 

.L0155 ;  if _sc1 = %00  &&  _sc2 = %00  &&  _sc3  =  $30 then _Chase_Speed  =  $0 else goto __Skip_Speed_Check1

	LDA _sc1
	CMP #%00
     BNE .skipL0155
.condpart20
	LDA _sc2
	CMP #%00
     BNE .skip20then
.condpart21
	LDA _sc3
	CMP #$30
     BNE .skip21then
.condpart22
	LDA #$0
	STA _Chase_Speed
 jmp .skipelse0
.skip21then
.skip20then
.skipL0155
 jmp .__Skip_Speed_Check1

.skipelse0
.L0156 ;  if _Ch0_Sound  <>  3  &&  !_Bit4_New_Chase_Start{4} then _Bit4_New_Chase_Start{4}  =  1  :  _Ch0_Sound  =  3  :  _Ch0_Duration  =  1  :  _Ch0_Counter  =  0

	LDA _Ch0_Sound
	CMP #3
     BEQ .skipL0156
.condpart23
	LDA _Bit4_New_Chase_Start
	AND #16
	BNE .skip23then
.condpart24
	LDA _Bit4_New_Chase_Start
	ORA #16
	STA _Bit4_New_Chase_Start
	LDA #3
	STA _Ch0_Sound
	LDA #1
	STA _Ch0_Duration
	LDA #0
	STA _Ch0_Counter
.skip23then
.skipL0156
.L0157 ;  goto __Skip_Speed_Check2

 jmp .__Skip_Speed_Check2

.__Skip_Speed_Check1
 ; __Skip_Speed_Check1

.L0158 ;  if _sc1 = %00  &&  _sc2 = %00  &&  _sc3  =  $15 then _Chase_Speed  =  $1 else goto __Skip_Speed_Check2

	LDA _sc1
	CMP #%00
     BNE .skipL0158
.condpart25
	LDA _sc2
	CMP #%00
     BNE .skip25then
.condpart26
	LDA _sc3
	CMP #$15
     BNE .skip26then
.condpart27
	LDA #$1
	STA _Chase_Speed
 jmp .skipelse1
.skip26then
.skip25then
.skipL0158
 jmp .__Skip_Speed_Check2

.skipelse1
.L0159 ;  if _Ch0_Sound  <>  3  &&  !_Bit4_New_Chase_Start{4} then _Bit4_New_Chase_Start{4}  =  1  :  _Ch0_Sound  =  3  :  _Ch0_Duration  =  1  :  _Ch0_Counter  =  0

	LDA _Ch0_Sound
	CMP #3
     BEQ .skipL0159
.condpart28
	LDA _Bit4_New_Chase_Start
	AND #16
	BNE .skip28then
.condpart29
	LDA _Bit4_New_Chase_Start
	ORA #16
	STA _Bit4_New_Chase_Start
	LDA #3
	STA _Ch0_Sound
	LDA #1
	STA _Ch0_Duration
	LDA #0
	STA _Ch0_Counter
.skip28then
.skipL0159
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

.L0160 ;  _Bit2_Player_Moving{2}  =  0

	LDA _Bit2_Player_Moving
	AND #251
	STA _Bit2_Player_Moving
.L0161 ;  if !joy0up  &&  !joy0down  &&  !joy0left  &&  !joy0right then goto __Skip_Joystick_Precheck

 lda #$10
 bit SWCHA
	BEQ .skipL0161
.condpart30
 lda #$20
 bit SWCHA
	BEQ .skip30then
.condpart31
 bit SWCHA
	BVC .skip31then
.condpart32
 bit SWCHA
	BPL .skip32then
.condpart33
 jmp .__Skip_Joystick_Precheck

.skip32then
.skip31then
.skip30then
.skipL0161
.
 ; 

.
 ; 

.
 ; 

.L0162 ;  _BitOp_P0_M0_Dir  =  _BitOp_P0_M0_Dir  &  %11110000

	LDA _BitOp_P0_M0_Dir
	AND #%11110000
	STA _BitOp_P0_M0_Dir
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

.L0163 ;  if !joy0up then goto __Skip_Joy0_Up

 lda #$10
 bit SWCHA
	BEQ .skipL0163
.condpart34
 jmp .__Skip_Joy0_Up

.skipL0163
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0164 ;  _Bit0_P0_Dir_Up{0}  =  1

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

.L0165 ;  if player0y  <=  _P_Edge_Top then goto __Skip_Joy0_Up

	LDA #_P_Edge_Top
	CMP player0y
     BCC .skipL0165
.condpart35
 jmp .__Skip_Joy0_Up

.skipL0165
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0166 ;  temp5  =   ( player0x - 11 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #11
	lsr
	lsr
	STA temp5
.
 ; 

.L0167 ;  temp6  =   ( player0y - 8 )  / 8

; complex statement detected
	LDA player0y
	SEC
	SBC #8
	lsr
	lsr
	lsr
	STA temp6
.
 ; 

.L0168 ;  if temp5  <  34 then if pfread ( temp5 , temp6 )  then goto __Skip_Joy0_Up

	LDA temp5
	CMP #34
     BCS .skipL0168
.condpart36
	LDA temp5
	LDY temp6
 jsr pfread
	BNE .skip36then
.condpart37
 jmp .__Skip_Joy0_Up

.skip36then
.skipL0168
.
 ; 

.L0169 ;  temp4  =   ( player0x - 16 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #16
	lsr
	lsr
	STA temp4
.
 ; 

.L0170 ;  if temp4  <  34 then if pfread ( temp4 , temp6 )  then goto __Skip_Joy0_Up

	LDA temp4
	CMP #34
     BCS .skipL0170
.condpart38
	LDA temp4
	LDY temp6
 jsr pfread
	BNE .skip38then
.condpart39
 jmp .__Skip_Joy0_Up

.skip38then
.skipL0170
.
 ; 

.L0171 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L0172 ;  if temp3  <  34 then if pfread ( temp3 , temp6 )  then goto __Skip_Joy0_Up

	LDA temp3
	CMP #34
     BCS .skipL0172
.condpart40
	LDA temp3
	LDY temp6
 jsr pfread
	BNE .skip40then
.condpart41
 jmp .__Skip_Joy0_Up

.skip40then
.skipL0172
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0173 ;  _Bit2_Player_Moving{2}  =  1

	LDA _Bit2_Player_Moving
	ORA #4
	STA _Bit2_Player_Moving
.L0174 ;  player0y  =  player0y  -  1

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

.
 ; 

.
 ; 

.
 ; 

.L0175 ;  if !joy0down then goto __Skip_Joy0_Down

 lda #$20
 bit SWCHA
	BEQ .skipL0175
.condpart42
 jmp .__Skip_Joy0_Down

.skipL0175
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0176 ;  _Bit1_P0_Dir_Down{1}  =  1

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

.L0177 ;  if player0y  >=  _P_Edge_Bottom then goto __Skip_Joy0_Down

	LDA player0y
	CMP #_P_Edge_Bottom
     BCC .skipL0177
.condpart43
 jmp .__Skip_Joy0_Down

.skipL0177
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0178 ;  temp5  =   ( player0x - 11 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #11
	lsr
	lsr
	STA temp5
.
 ; 

.L0179 ;  temp6  =   ( player0y )  / 8

; complex statement detected
	LDA player0y
	lsr
	lsr
	lsr
	STA temp6
.
 ; 

.L0180 ;  if temp5  <  34 then if pfread ( temp5 , temp6 )  then goto __Skip_Joy0_Down

	LDA temp5
	CMP #34
     BCS .skipL0180
.condpart44
	LDA temp5
	LDY temp6
 jsr pfread
	BNE .skip44then
.condpart45
 jmp .__Skip_Joy0_Down

.skip44then
.skipL0180
.
 ; 

.L0181 ;  temp4  =   ( player0x - 16 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #16
	lsr
	lsr
	STA temp4
.
 ; 

.L0182 ;  if temp4  <  34 then if pfread ( temp4 , temp6 )  then goto __Skip_Joy0_Down

	LDA temp4
	CMP #34
     BCS .skipL0182
.condpart46
	LDA temp4
	LDY temp6
 jsr pfread
	BNE .skip46then
.condpart47
 jmp .__Skip_Joy0_Down

.skip46then
.skipL0182
.
 ; 

.L0183 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L0184 ;  if temp3  <  34 then if pfread ( temp3 , temp6 )  then goto __Skip_Joy0_Down

	LDA temp3
	CMP #34
     BCS .skipL0184
.condpart48
	LDA temp3
	LDY temp6
 jsr pfread
	BNE .skip48then
.condpart49
 jmp .__Skip_Joy0_Down

.skip48then
.skipL0184
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0185 ;  _Bit2_Player_Moving{2}  =  1

	LDA _Bit2_Player_Moving
	ORA #4
	STA _Bit2_Player_Moving
.L0186 ;  player0y  =  player0y  +  1

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

.L0187 ;  if !joy0left then goto __Skip_Joy0_Left

 bit SWCHA
	BVC .skipL0187
.condpart50
 jmp .__Skip_Joy0_Left

.skipL0187
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0188 ;  _Bit2_P0_Dir_Left{2}  =  1

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

.L0189 ;  if player0x  <=  _P_Edge_Left then goto __Skip_Joy0_Left

	LDA #_P_Edge_Left
	CMP player0x
     BCC .skipL0189
.condpart51
 jmp .__Skip_Joy0_Left

.skipL0189
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0190 ;  temp5  =   ( player0y - 1 )  / 8

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

.L0191 ;  temp6  =   ( player0x - 17 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #17
	lsr
	lsr
	STA temp6
.
 ; 

.L0192 ;  if temp6  <  34 then if pfread ( temp6 , temp5 )  then goto __Skip_Joy0_Left

	LDA temp6
	CMP #34
     BCS .skipL0192
.condpart52
	LDA temp6
	LDY temp5
 jsr pfread
	BNE .skip52then
.condpart53
 jmp .__Skip_Joy0_Left

.skip52then
.skipL0192
.
 ; 

.L0193 ;  temp3  =   ( player0y - 7 )  / 8

; complex statement detected
	LDA player0y
	SEC
	SBC #7
	lsr
	lsr
	lsr
	STA temp3
.
 ; 

.L0194 ;  if temp6  <  34 then if pfread ( temp6 , temp3 )  then goto __Skip_Joy0_Left

	LDA temp6
	CMP #34
     BCS .skipL0194
.condpart54
	LDA temp6
	LDY temp3
 jsr pfread
	BNE .skip54then
.condpart55
 jmp .__Skip_Joy0_Left

.skip54then
.skipL0194
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0195 ;  _Bit2_Player_Moving{2}  =  1

	LDA _Bit2_Player_Moving
	ORA #4
	STA _Bit2_Player_Moving
.L0196 ;  REFP0  =  8

	LDA #8
	STA REFP0
.L0197 ;  player0x  =  player0x  -  1

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

.
 ; 

.
 ; 

.L0198 ;  if !joy0right then goto __Skip_Joy0_Right

 bit SWCHA
	BPL .skipL0198
.condpart56
 jmp .__Skip_Joy0_Right

.skipL0198
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0199 ;  _Bit3_P0_Dir_Right{3}  =  1

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

.L0200 ;  if player0x  >=  _P_Edge_Right then goto __Skip_Joy0_Right

	LDA player0x
	CMP #_P_Edge_Right
     BCC .skipL0200
.condpart57
 jmp .__Skip_Joy0_Right

.skipL0200
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0201 ;  temp5  =   ( player0y - 1 )  / 8

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

.L0202 ;  temp6  =   ( player0x - 10 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #10
	lsr
	lsr
	STA temp6
.
 ; 

.L0203 ;  if temp6  <  34 then if pfread ( temp6 , temp5 )  then goto __Skip_Joy0_Right

	LDA temp6
	CMP #34
     BCS .skipL0203
.condpart58
	LDA temp6
	LDY temp5
 jsr pfread
	BNE .skip58then
.condpart59
 jmp .__Skip_Joy0_Right

.skip58then
.skipL0203
.
 ; 

.L0204 ;  temp3  =   ( player0y - 7 )  / 8

; complex statement detected
	LDA player0y
	SEC
	SBC #7
	lsr
	lsr
	lsr
	STA temp3
.
 ; 

.L0205 ;  if temp6  <  34 then if pfread ( temp6 , temp3 )  then goto __Skip_Joy0_Right

	LDA temp6
	CMP #34
     BCS .skipL0205
.condpart60
	LDA temp6
	LDY temp3
 jsr pfread
	BNE .skip60then
.condpart61
 jmp .__Skip_Joy0_Right

.skip60then
.skipL0205
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0206 ;  _Bit2_Player_Moving{2}  =  1

	LDA _Bit2_Player_Moving
	ORA #4
	STA _Bit2_Player_Moving
.L0207 ;  player0x  =  player0x  +  1

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

.
 ; 

.
 ; 

.
 ; 

.L0208 ;  temp1  =  player0x  +  8

	LDA player0x
	CLC
	ADC #8
	STA temp1
.L0209 ;  temp2  =  player0y  +  8

	LDA player0y
	CLC
	ADC #8
	STA temp2
.
 ; 

.L0210 ;  temp3  =  player1x  +  4

	LDA player1x
	CLC
	ADC #4
	STA temp3
.L0211 ;  temp4  =  player1y  +  4

	LDA player1y
	CLC
	ADC #4
	STA temp4
.
 ; 

.L0212 ;  temp5  =  _Minotaur_Awareness_Size

	LDA #_Minotaur_Awareness_Size
	STA temp5
.
 ; 

.L0213 ;  if _Master_Counter & _Chase_Speed then goto __Skip_AI_Right

	LDA _Master_Counter
	AND _Chase_Speed
     BEQ .skipL0213
.condpart62
 jmp .__Skip_AI_Right

.skipL0213
.L0214 ;  _Bit3_Mino_Moving{3}  =  0

	LDA _Bit3_Mino_Moving
	AND #247
	STA _Bit3_Mino_Moving
.
 ; 

.
 ; 

.L0215 ;  if player0y  >=  temp4  +  temp5 then goto __Skip_AI_Right

; complex condition detected
	LDA temp4
	CLC
	ADC temp5
  PHA
  TSX
  PLA
	LDA player0y
	CMP  1,x
     BCC .skipL0215
.condpart63
 jmp .__Skip_AI_Right

.skipL0215
.
 ; 

.
 ; 

.L0216 ;  if temp4  <  temp5 then temp4  =  temp5

	LDA temp4
	CMP temp5
     BCS .skipL0216
.condpart64
	LDA temp5
	STA temp4
.skipL0216
.L0217 ;  if temp2  <=  temp4  -  temp5 then goto __Skip_AI_Right

; complex condition detected
	LDA temp4
	SEC
	SBC temp5
	CMP temp2
     BCC .skipL0217
.condpart65
 jmp .__Skip_AI_Right

.skipL0217
.
 ; 

.
 ; 

.L0218 ;  if temp3  <  temp5 then temp3  =  temp5

	LDA temp3
	CMP temp5
     BCS .skipL0218
.condpart66
	LDA temp5
	STA temp3
.skipL0218
.L0219 ;  if temp1  <=  temp3  -  temp5 then goto __Skip_AI_Right

; complex condition detected
	LDA temp3
	SEC
	SBC temp5
	CMP temp1
     BCC .skipL0219
.condpart67
 jmp .__Skip_AI_Right

.skipL0219
.
 ; 

.
 ; 

.L0220 ;  if player0x  >=  temp3  +  temp5 then goto __Skip_AI_Right

; complex condition detected
	LDA temp3
	CLC
	ADC temp5
  PHA
  TSX
  PLA
	LDA player0x
	CMP  1,x
     BCC .skipL0220
.condpart68
 jmp .__Skip_AI_Right

.skipL0220
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0221 ;  if player1y  <=  player0y then goto __Skip_AI_Up

	LDA player0y
	CMP player1y
     BCC .skipL0221
.condpart69
 jmp .__Skip_AI_Up

.skipL0221
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0222 ;  _Bit0_P1_Dir_Up{0}  =  1

	LDA _Bit0_P1_Dir_Up
	ORA #1
	STA _Bit0_P1_Dir_Up
.L0223 ;  _Bit1_P1_Dir_Down{1}  =  0

	LDA _Bit1_P1_Dir_Down
	AND #253
	STA _Bit1_P1_Dir_Down
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0224 ;  if player1y  <=  _P_Edge_Top then goto __Skip_AI_Up

	LDA #_P_Edge_Top
	CMP player1y
     BCC .skipL0224
.condpart70
 jmp .__Skip_AI_Up

.skipL0224
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0225 ;  temp5  =   ( player1x - 10 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #10
	lsr
	lsr
	STA temp5
.
 ; 

.L0226 ;  temp6  =   ( player1y - 9 )  / 8

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

.L0227 ;  if temp5  <  34 then if pfread ( temp5 , temp6 )  then goto __Skip_AI_Up

	LDA temp5
	CMP #34
     BCS .skipL0227
.condpart71
	LDA temp5
	LDY temp6
 jsr pfread
	BNE .skip71then
.condpart72
 jmp .__Skip_AI_Up

.skip71then
.skipL0227
.
 ; 

.L0228 ;  temp4  =   ( player1x - 17 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #17
	lsr
	lsr
	STA temp4
.
 ; 

.L0229 ;  if temp4  <  34 then if pfread ( temp4 , temp6 )  then goto __Skip_AI_Up

	LDA temp4
	CMP #34
     BCS .skipL0229
.condpart73
	LDA temp4
	LDY temp6
 jsr pfread
	BNE .skip73then
.condpart74
 jmp .__Skip_AI_Up

.skip73then
.skipL0229
.
 ; 

.L0230 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L0231 ;  if temp3  <  34 then if pfread ( temp3 , temp6 )  then goto __Skip_AI_Up

	LDA temp3
	CMP #34
     BCS .skipL0231
.condpart75
	LDA temp3
	LDY temp6
 jsr pfread
	BNE .skip75then
.condpart76
 jmp .__Skip_AI_Up

.skip75then
.skipL0231
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0232 ;  _Bit3_Mino_Moving{3}  =  1

	LDA _Bit3_Mino_Moving
	ORA #8
	STA _Bit3_Mino_Moving
.L0233 ;  player1y  =  player1y  -  1

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

.L0234 ;  if player1y  >=  player0y then goto __Skip_AI_Down

	LDA player1y
	CMP player0y
     BCC .skipL0234
.condpart77
 jmp .__Skip_AI_Down

.skipL0234
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0235 ;  _Bit1_P1_Dir_Down{1}  =  1

	LDA _Bit1_P1_Dir_Down
	ORA #2
	STA _Bit1_P1_Dir_Down
.L0236 ;  _Bit0_P1_Dir_Up{0}  =  0

	LDA _Bit0_P1_Dir_Up
	AND #254
	STA _Bit0_P1_Dir_Up
.
 ; 

.
 ; 

.
 ; 

.L0237 ;  if player1y  >=  _P_Edge_Bottom then goto __Skip_AI_Down

	LDA player1y
	CMP #_P_Edge_Bottom
     BCC .skipL0237
.condpart78
 jmp .__Skip_AI_Down

.skipL0237
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0238 ;  temp5  =   ( player1x - 10 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #10
	lsr
	lsr
	STA temp5
.
 ; 

.L0239 ;  temp6  =   ( player1y )  / 8

; complex statement detected
	LDA player1y
	lsr
	lsr
	lsr
	STA temp6
.
 ; 

.L0240 ;  if temp5  <  34 then if pfread ( temp5 , temp6 )  then goto __Skip_AI_Down

	LDA temp5
	CMP #34
     BCS .skipL0240
.condpart79
	LDA temp5
	LDY temp6
 jsr pfread
	BNE .skip79then
.condpart80
 jmp .__Skip_AI_Down

.skip79then
.skipL0240
.
 ; 

.L0241 ;  temp4  =   ( player1x - 17 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #17
	lsr
	lsr
	STA temp4
.
 ; 

.L0242 ;  if temp4  <  34 then if pfread ( temp4 , temp6 )  then goto __Skip_AI_Down

	LDA temp4
	CMP #34
     BCS .skipL0242
.condpart81
	LDA temp4
	LDY temp6
 jsr pfread
	BNE .skip81then
.condpart82
 jmp .__Skip_AI_Down

.skip81then
.skipL0242
.
 ; 

.L0243 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L0244 ;  if temp3  <  34 then if pfread ( temp3 , temp6 )  then goto __Skip_AI_Down

	LDA temp3
	CMP #34
     BCS .skipL0244
.condpart83
	LDA temp3
	LDY temp6
 jsr pfread
	BNE .skip83then
.condpart84
 jmp .__Skip_AI_Down

.skip83then
.skipL0244
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0245 ;  _Bit3_Mino_Moving{3}  =  1

	LDA _Bit3_Mino_Moving
	ORA #8
	STA _Bit3_Mino_Moving
.L0246 ;  player1y  =  player1y  +  1

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

.L0247 ;  if player1x  <=  player0x then goto __Skip_AI_Left

	LDA player0x
	CMP player1x
     BCC .skipL0247
.condpart85
 jmp .__Skip_AI_Left

.skipL0247
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0248 ;  _Bit2_P1_Dir_Left{2}  =  1

	LDA _Bit2_P1_Dir_Left
	ORA #4
	STA _Bit2_P1_Dir_Left
.L0249 ;  _Bit3_P1_Dir_Right{3}  =  0

	LDA _Bit3_P1_Dir_Right
	AND #247
	STA _Bit3_P1_Dir_Right
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0250 ;  if player1x  <=  _P_Edge_Left then goto __Skip_AI_Left

	LDA #_P_Edge_Left
	CMP player1x
     BCC .skipL0250
.condpart86
 jmp .__Skip_AI_Left

.skipL0250
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0251 ;  temp5  =   ( player1y - 1 )  / 8

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

.L0252 ;  temp6  =   ( player1x - 18 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #18
	lsr
	lsr
	STA temp6
.
 ; 

.L0253 ;  if temp6  <  34 then if pfread ( temp6 , temp5 )  then goto __Skip_AI_Left

	LDA temp6
	CMP #34
     BCS .skipL0253
.condpart87
	LDA temp6
	LDY temp5
 jsr pfread
	BNE .skip87then
.condpart88
 jmp .__Skip_AI_Left

.skip87then
.skipL0253
.
 ; 

.L0254 ;  temp3  =   ( player1y - 8 )  / 8

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

.L0255 ;  if temp6  <  34 then if pfread ( temp6 , temp3 )  then goto __Skip_AI_Left

	LDA temp6
	CMP #34
     BCS .skipL0255
.condpart89
	LDA temp6
	LDY temp3
 jsr pfread
	BNE .skip89then
.condpart90
 jmp .__Skip_AI_Left

.skip89then
.skipL0255
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0256 ;  _Bit3_Mino_Moving{3}  =  1

	LDA _Bit3_Mino_Moving
	ORA #8
	STA _Bit3_Mino_Moving
.L0257 ;  player1x  =  player1x  -  1

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

.L0258 ;  if player1x  >=  player0x then goto __Skip_AI_Right

	LDA player1x
	CMP player0x
     BCC .skipL0258
.condpart91
 jmp .__Skip_AI_Right

.skipL0258
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0259 ;  _Bit2_P1_Dir_Left{2}  =  0

	LDA _Bit2_P1_Dir_Left
	AND #251
	STA _Bit2_P1_Dir_Left
.L0260 ;  _Bit3_P1_Dir_Right{3}  =  1

	LDA _Bit3_P1_Dir_Right
	ORA #8
	STA _Bit3_P1_Dir_Right
.
 ; 

.
 ; 

.
 ; 

.L0261 ;  if player1x  >=  _P_Edge_Right then goto __Skip_AI_Right

	LDA player1x
	CMP #_P_Edge_Right
     BCC .skipL0261
.condpart92
 jmp .__Skip_AI_Right

.skipL0261
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0262 ;  temp5  =   ( player1y - 1 )  / 8

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

.L0263 ;  temp6  =   ( player1x - 9 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #9
	lsr
	lsr
	STA temp6
.
 ; 

.L0264 ;  if temp6  <  34 then if pfread ( temp6 , temp5 )  then goto __Skip_AI_Right

	LDA temp6
	CMP #34
     BCS .skipL0264
.condpart93
	LDA temp6
	LDY temp5
 jsr pfread
	BNE .skip93then
.condpart94
 jmp .__Skip_AI_Right

.skip93then
.skipL0264
.
 ; 

.L0265 ;  temp3  =   ( player1y - 8 )  / 8

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

.L0266 ;  if temp6  <  34 then if pfread ( temp6 , temp3 )  then goto __Skip_AI_Right

	LDA temp6
	CMP #34
     BCS .skipL0266
.condpart95
	LDA temp6
	LDY temp3
 jsr pfread
	BNE .skip95then
.condpart96
 jmp .__Skip_AI_Right

.skip95then
.skipL0266
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0267 ;  _Bit3_Mino_Moving{3}  =  1

	LDA _Bit3_Mino_Moving
	ORA #8
	STA _Bit3_Mino_Moving
.L0268 ;  player1x  =  player1x  +  1

	INC player1x
.
 ; 

.__Skip_AI_Right
 ; __Skip_AI_Right

.
 ; 

.L0269 ;  if _Bit2_P1_Dir_Left{2} then REFP1  =  8

	LDA _Bit2_P1_Dir_Left
	AND #4
	BEQ .skipL0269
.condpart97
	LDA #8
	STA REFP1
.skipL0269
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0270 ;  if !collision(missile0,player0) then __Skip_Gem_Collection

	bit 	CXM0P
 if ((* - .__Skip_Gem_Collection) < 127) && ((* - .__Skip_Gem_Collection) > -128)
	bvc .__Skip_Gem_Collection
 else
	bvs .4skip__Skip_Gem_Collection
	jmp .__Skip_Gem_Collection
.4skip__Skip_Gem_Collection
 endif
.L0271 ;  _Bit0_Carrying_Gem{0}  =  1

	LDA _Bit0_Carrying_Gem
	ORA #1
	STA _Bit0_Carrying_Gem
.L0272 ;  missile0x  =  200  :  missile0y  =  200

	LDA #200
	STA missile0x
	STA missile0y
.
 ; 

.
 ; 

.
 ; 

.L0273 ;  if _Ch0_Sound  <>  1 then _Ch0_Sound  =  1  :  _Ch0_Duration  =  1  :  _Ch0_Counter  =  0

	LDA _Ch0_Sound
	CMP #1
     BEQ .skipL0273
.condpart98
	LDA #1
	STA _Ch0_Sound
	STA _Ch0_Duration
	LDA #0
	STA _Ch0_Counter
.skipL0273
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

.L0274 ;  if !collision(missile1,player0) then __Skip_Gem_Deposit

	bit 	CXM1P
 if ((* - .__Skip_Gem_Deposit) < 127) && ((* - .__Skip_Gem_Deposit) > -128)
	bpl .__Skip_Gem_Deposit
 else
	bmi .5skip__Skip_Gem_Deposit
	jmp .__Skip_Gem_Deposit
.5skip__Skip_Gem_Deposit
 endif
.L0275 ;  if !_Bit0_Carrying_Gem{0} then __Skip_Gem_Deposit

	LDA _Bit0_Carrying_Gem
	LSR
 if ((* - .__Skip_Gem_Deposit) < 127) && ((* - .__Skip_Gem_Deposit) > -128)
	bcc .__Skip_Gem_Deposit
 else
	bcs .6skip__Skip_Gem_Deposit
	jmp .__Skip_Gem_Deposit
.6skip__Skip_Gem_Deposit
 endif
.L0276 ;  _Bit0_Carrying_Gem{0}  =  0

	LDA _Bit0_Carrying_Gem
	AND #254
	STA _Bit0_Carrying_Gem
.L0277 ;  score  =  score  +  1

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
.L0278 ;  _Bit4_New_Chase_Start{4}  =  0

	LDA _Bit4_New_Chase_Start
	AND #239
	STA _Bit4_New_Chase_Start
.L0279 ;  if _Ch0_Sound  <>  2 then _Ch0_Sound  =  2  :  _Ch0_Duration  =  1  :  _Ch0_Counter  =  0

	LDA _Ch0_Sound
	CMP #2
     BEQ .skipL0279
.condpart99
	LDA #2
	STA _Ch0_Sound
	LDA #1
	STA _Ch0_Duration
	LDA #0
	STA _Ch0_Counter
.skipL0279
.L0280 ;  missile0x  =   ( rand  &  118 )   +  23  :  missile0y  =   ( rand  &  70 )   +  9

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

.L0281 ;  if !_Bit1_Game_Over{1} then goto __Skip_Game_Over

	LDA _Bit1_Game_Over
	AND #2
	BNE .skipL0281
.condpart100
 jmp .__Skip_Game_Over

.skipL0281
.L0282 ;  if _Ch0_Sound  =  3  ||  _Bit5_Game_Over_Music_Played{5} then goto __Skip_Game_Over_Music

	LDA _Ch0_Sound
	CMP #3
     BNE .skipL0282
.condpart101
 jmp .condpart102
.skipL0282
	LDA _Bit5_Game_Over_Music_Played
	AND #32
	BEQ .skip9OR
.condpart102
 jmp .__Skip_Game_Over_Music

.skip9OR
.L0283 ;  _Ch0_Sound  =  3  :  _Ch0_Duration  =  1  :  _Ch0_Counter  =  0

	LDA #3
	STA _Ch0_Sound
	LDA #1
	STA _Ch0_Duration
	LDA #0
	STA _Ch0_Counter
.L0284 ;  _Bit5_Game_Over_Music_Played{5}  =  1

	LDA _Bit5_Game_Over_Music_Played
	ORA #32
	STA _Bit5_Game_Over_Music_Played
.__Skip_Game_Over_Music
 ; __Skip_Game_Over_Music

.L0285 ;  COLUPF  =  14

	LDA #14
	STA COLUPF
.L0286 ;  scorecolor  =  14

	LDA #14
	STA scorecolor
.L0287 ;  COLUBK  =  $36

	LDA #$36
	STA COLUBK
.L0288 ;  playfield:

  ifconst pfres
	  ldx #(11>pfres)*(pfres*pfwidth-1)+(11<=pfres)*43
  else
	  ldx #((11*pfwidth-1)*((11*pfwidth-1)<47))+(47*((11*pfwidth-1)>=47))
  endif
	jmp pflabel2
PF_data2
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
	.byte %00001011, %01001001
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
pflabel2
	lda PF_data2,x
	sta playfield,x
	dex
	bpl pflabel2
.
 ; 

.L0289 ;  player0x  =  200  :  player0y  =  200

	LDA #200
	STA player0x
	STA player0y
.L0290 ;  player1x  =  200  :  player1y  =  200

	LDA #200
	STA player1x
	STA player1y
.L0291 ;  missile0x  =  200  :  missile0y  =  200

	LDA #200
	STA missile0x
	STA missile0y
.L0292 ;  missile1x  =  200  :  missile1y  =  200

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

.L0293 ;  drawscreen

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

.L0294 ;  if !switchreset then _Bit0_Reset_Restrainer{0}  =  0  :  goto __Main_Loop

 lda #1
 bit SWCHB
	BEQ .skipL0294
.condpart103
	LDA _Bit0_Reset_Restrainer
	AND #254
	STA _Bit0_Reset_Restrainer
 jmp .__Main_Loop

.skipL0294
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0295 ;  if _Bit0_Reset_Restrainer{0} then goto __Main_Loop

	LDA _Bit0_Reset_Restrainer
	LSR
	BCC .skipL0295
.condpart104
 jmp .__Main_Loop

.skipL0295
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0296 ;  goto __Start_Restart

 jmp .__Start_Restart

.
 ; 

.__Pl00
 ; __Pl00

.L0297 ;  player0:

	LDX #<playerL0297_0
	STX player0pointerlo
	LDA #>playerL0297_0
	STA player0pointerhi
	LDA #7
	STA player0height
.L0298 ;  goto __Pl_Frame_Done

 jmp .__Pl_Frame_Done

.__Pl01
 ; __Pl01

.L0299 ;  player0:

	LDX #<playerL0299_0
	STX player0pointerlo
	LDA #>playerL0299_0
	STA player0pointerhi
	LDA #7
	STA player0height
.L0300 ;  goto __Pl_Frame_Done

 jmp .__Pl_Frame_Done

.__Pl02
 ; __Pl02

.L0301 ;  player0:

	LDX #<playerL0301_0
	STX player0pointerlo
	LDA #>playerL0301_0
	STA player0pointerhi
	LDA #7
	STA player0height
.L0302 ;  goto __Pl_Frame_Done

 jmp .__Pl_Frame_Done

.
 ; 

.__Mn00
 ; __Mn00

.L0303 ;  player1:

	LDX #<playerL0303_1
	STX player1pointerlo
	LDA #>playerL0303_1
	STA player1pointerhi
	LDA #7
	STA player1height
.L0304 ;  goto __Mn_Frame_Done

 jmp .__Mn_Frame_Done

.__Mn01
 ; __Mn01

.L0305 ;  player1:

	LDX #<playerL0305_1
	STX player1pointerlo
	LDA #>playerL0305_1
	STA player1pointerhi
	LDA #7
	STA player1height
.L0306 ;  goto __Mn_Frame_Done

 jmp .__Mn_Frame_Done

.__Mn02
 ; __Mn02

.L0307 ;  player1:

	LDX #<playerL0307_1
	STX player1pointerlo
	LDA #>playerL0307_1
	STA player1pointerhi
	LDA #7
	STA player1height
.L0308 ;  goto __Mn_Frame_Done

 jmp .__Mn_Frame_Done

.
 ; 

.L0309 ;  data _SD_Gem_Coll

	JMP .skipL0309
_SD_Gem_Coll
	.byte    8,4,7

	.byte    4

	.byte    8,4,6

	.byte    8

	.byte    255

.skipL0309
.L0310 ;  data _SD_Gem_Drop

	JMP .skipL0310
_SD_Gem_Drop
	.byte    8,4,6

	.byte    4

	.byte    8,4,7

	.byte    8

	.byte    8,4,4

	.byte    4

	.byte    255

.skipL0310
.L0311 ;  data _SD_Mino_Roar

	JMP .skipL0311
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

.skipL0311
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL057_0
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
playerL058_1
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
playerL0100_0
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
playerL0103_1
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
playerL0297_0
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
playerL0299_0
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
playerL0301_0
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
playerL0303_1
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
playerL0305_1
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
playerL0307_1
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
