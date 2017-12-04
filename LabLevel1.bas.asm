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

.L019 ;  dim _Frame_Count  =  h

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L020 ;  const _Minotaur_Awareness_Size  =  100

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L021 ;  dim _Bit0_Carrying_Gem  =  x

.L022 ;  dim _Bit1_Game_Over  =  x

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L023 ;  dim _BitOp_01  =  y

.L024 ;  dim _Bit0_Reset_Restrainer  =  y

.L025 ;  dim _Bit1_Toggle_Screen  =  y

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

.
 ; 

.L031 ;  const _B_Edge_Top  =  2

.L032 ;  const _B_Edge_Bottom  =  88

.L033 ;  const _B_Edge_Left  =  2

.L034 ;  const _B_Edge_Right  =  160

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L035 ;  const _M_Edge_Top  =  2

.L036 ;  const _M_Edge_Bottom  =  88

.L037 ;  const _M_Edge_Left  =  2

.L038 ;  const _M_Edge_Right  =  159

.
 ; 

.
 ; 

.
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

.L039 ;  AUDV0  =  0  :  AUDV1  =  0

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

.L040 ;  a  =  0  :  b  =  0  :  c  =  0  :  d  =  0  :  e  =  0  :  f  =  0  :  g  =  0  :  h  =  0  :  i  =  0

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
.L041 ;  j  =  0  :  k  =  0  :  l  =  0  :  m  =  0  :  n  =  0  :  o  =  0  :  p  =  0  :  q  =  0  :  r  =  0

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
.L042 ;  s  =  0  :  t  =  0  :  u  =  0  :  v  =  0  :  w  =  0  :  x  =  0

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

.L043 ;  _BitOp_01  =  _BitOp_01  &  %00010000

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

.
 ; 

.L044 ;  player0x  =  77  :  player0y  =  60

	LDA #77
	STA player0x
	LDA #60
	STA player0y
.L045 ;  player1x  =  21  :  player1y  =  16

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

.L046 ;  missile0x  =  200  :  missile0y  =  200

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

.L047 ;  missile0x  =  80  :  missile0y  =  79

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

.L048 ;  NUSIZ0  =  $10  :  missile0height  =  1

	LDA #$10
	STA NUSIZ0
	LDA #1
	STA missile0height
.L049 ;  NUSIZ1  =  $30  :  missile0height  =  4

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

.L050 ;  COLUPF  =  $2C

	LDA #$2C
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

.L051 ;  COLUBK  =  0

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

.L052 ;  _Bit3_P0_Dir_Right{3}  =  1

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

.L053 ;  _Bit0_Reset_Restrainer{0}  =  1

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

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L054 ;  playfield:

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
	.byte %10011001, %00011001
	if (pfwidth>2)
	.byte %00011001, %10011001
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

.L055 ;  player0:

	LDX #<playerL055_0
	STX player0pointerlo
	LDA #>playerL055_0
	STA player0pointerhi
	LDA #7
	STA player0height
.
 ; 

.L056 ;  player1:

	LDX #<playerL056_1
	STX player1pointerlo
	LDA #>playerL056_1
	STA player1pointerhi
	LDA #7
	STA player1height
.
 ; 

.
 ; 

.
 ; 

.L057 ;  score  =  0

	LDA #$00
	STA score+2
	LDA #$00
	STA score+1
	LDA #$00
	STA score
.
 ; 

.L058 ;  dim _sc1  =  score

.L059 ;  dim _sc2  =  score + 1

.L060 ;  dim _sc3  =  score + 2

.
 ; 

.
 ; 

.
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

.L061 ;  if _Bit1_Game_Over{1} then if joy0fire goto __Start_Restart

	LDA _Bit1_Game_Over
	AND #2
	BEQ .skipL061
.condpart0
 bit INPT4
 if ((* - .__Start_Restart) < 127) && ((* - .__Start_Restart) > -128)
	bpl .__Start_Restart
 else
	bmi .0skip__Start_Restart
	jmp .__Start_Restart
.0skip__Start_Restart
 endif
.skipL061
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L062 ;  if !collision(player0,player1) then __Skip_Player_Caught

	bit 	CXPPMM
 if ((* - .__Skip_Player_Caught) < 127) && ((* - .__Skip_Player_Caught) > -128)
	bpl .__Skip_Player_Caught
 else
	bmi .1skip__Skip_Player_Caught
	jmp .__Skip_Player_Caught
.1skip__Skip_Player_Caught
 endif
.L063 ;  _Bit1_Game_Over{1}  =  1

	LDA _Bit1_Game_Over
	ORA #2
	STA _Bit1_Game_Over
.L064 ;  goto __Game_Over

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

.L065 ;  NUSIZ0  =  $10  :  missile0height  =  1

	LDA #$10
	STA NUSIZ0
	LDA #1
	STA missile0height
.L066 ;  NUSIZ1  =  $30  :  missile1height  =  7

	LDA #$30
	STA NUSIZ1
	LDA #7
	STA missile1height
.
 ; 

.L067 ;  missile1x  =  78  :  missile1y  =  48

	LDA #78
	STA missile1x
	LDA #48
	STA missile1y
.
 ; 

.L068 ;  _Frame_Count  =  _Frame_Count  +  1

	INC _Frame_Count
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L069 ;  COLUP0  =  $9C

	LDA #$9C
	STA COLUP0
.L070 ;  COLUP1  =  $34

	LDA #$34
	STA COLUP1
.L071 ;  scorecolor  =  $9C

	LDA #$9C
	STA scorecolor
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L072 ;  if !joy0up  &&  !joy0down  &&  !joy0left  &&  !joy0right then goto __Skip_Joystick_Precheck

 lda #$10
 bit SWCHA
	BEQ .skipL072
.condpart1
 lda #$20
 bit SWCHA
	BEQ .skip1then
.condpart2
 bit SWCHA
	BVC .skip2then
.condpart3
 bit SWCHA
	BPL .skip3then
.condpart4
 jmp .__Skip_Joystick_Precheck

.skip3then
.skip2then
.skip1then
.skipL072
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L073 ;  _BitOp_P0_M0_Dir  =  _BitOp_P0_M0_Dir  &  %11110000

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

.L074 ;  if !joy0up then goto __Skip_Joy0_Up

 lda #$10
 bit SWCHA
	BEQ .skipL074
.condpart5
 jmp .__Skip_Joy0_Up

.skipL074
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L075 ;  _Bit0_P0_Dir_Up{0}  =  1

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

.L076 ;  if player0y  <=  _P_Edge_Top then goto __Skip_Joy0_Up

	LDA #_P_Edge_Top
	CMP player0y
     BCC .skipL076
.condpart6
 jmp .__Skip_Joy0_Up

.skipL076
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L077 ;  temp5  =   ( player0x - 10 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #10
	lsr
	lsr
	STA temp5
.
 ; 

.L078 ;  temp6  =   ( player0y - 9 )  / 8

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

.L079 ;  if temp5  <  34 then if pfread ( temp5 , temp6 )  then goto __Skip_Joy0_Up

	LDA temp5
	CMP #34
     BCS .skipL079
.condpart7
	LDA temp5
	LDY temp6
 jsr pfread
	BNE .skip7then
.condpart8
 jmp .__Skip_Joy0_Up

.skip7then
.skipL079
.
 ; 

.L080 ;  temp4  =   ( player0x - 17 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #17
	lsr
	lsr
	STA temp4
.
 ; 

.L081 ;  if temp4  <  34 then if pfread ( temp4 , temp6 )  then goto __Skip_Joy0_Up

	LDA temp4
	CMP #34
     BCS .skipL081
.condpart9
	LDA temp4
	LDY temp6
 jsr pfread
	BNE .skip9then
.condpart10
 jmp .__Skip_Joy0_Up

.skip9then
.skipL081
.
 ; 

.L082 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L083 ;  if temp3  <  34 then if pfread ( temp3 , temp6 )  then goto __Skip_Joy0_Up

	LDA temp3
	CMP #34
     BCS .skipL083
.condpart11
	LDA temp3
	LDY temp6
 jsr pfread
	BNE .skip11then
.condpart12
 jmp .__Skip_Joy0_Up

.skip11then
.skipL083
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L084 ;  player0y  =  player0y  -  1

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

.L085 ;  if !joy0down then goto __Skip_Joy0_Down

 lda #$20
 bit SWCHA
	BEQ .skipL085
.condpart13
 jmp .__Skip_Joy0_Down

.skipL085
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L086 ;  _Bit1_P0_Dir_Down{1}  =  1

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

.L087 ;  if player0y  >=  _P_Edge_Bottom then goto __Skip_Joy0_Down

	LDA player0y
	CMP #_P_Edge_Bottom
     BCC .skipL087
.condpart14
 jmp .__Skip_Joy0_Down

.skipL087
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L088 ;  temp5  =   ( player0x - 10 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #10
	lsr
	lsr
	STA temp5
.
 ; 

.L089 ;  temp6  =   ( player0y )  / 8

; complex statement detected
	LDA player0y
	lsr
	lsr
	lsr
	STA temp6
.
 ; 

.L090 ;  if temp5  <  34 then if pfread ( temp5 , temp6 )  then goto __Skip_Joy0_Down

	LDA temp5
	CMP #34
     BCS .skipL090
.condpart15
	LDA temp5
	LDY temp6
 jsr pfread
	BNE .skip15then
.condpart16
 jmp .__Skip_Joy0_Down

.skip15then
.skipL090
.
 ; 

.L091 ;  temp4  =   ( player0x - 17 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #17
	lsr
	lsr
	STA temp4
.
 ; 

.L092 ;  if temp4  <  34 then if pfread ( temp4 , temp6 )  then goto __Skip_Joy0_Down

	LDA temp4
	CMP #34
     BCS .skipL092
.condpart17
	LDA temp4
	LDY temp6
 jsr pfread
	BNE .skip17then
.condpart18
 jmp .__Skip_Joy0_Down

.skip17then
.skipL092
.
 ; 

.L093 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L094 ;  if temp3  <  34 then if pfread ( temp3 , temp6 )  then goto __Skip_Joy0_Down

	LDA temp3
	CMP #34
     BCS .skipL094
.condpart19
	LDA temp3
	LDY temp6
 jsr pfread
	BNE .skip19then
.condpart20
 jmp .__Skip_Joy0_Down

.skip19then
.skipL094
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L095 ;  player0y  =  player0y  +  1

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

.L096 ;  if !joy0left then goto __Skip_Joy0_Left

 bit SWCHA
	BVC .skipL096
.condpart21
 jmp .__Skip_Joy0_Left

.skipL096
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L097 ;  _Bit2_P0_Dir_Left{2}  =  1

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

.L098 ;  if player0x  <=  _P_Edge_Left then goto __Skip_Joy0_Left

	LDA #_P_Edge_Left
	CMP player0x
     BCC .skipL098
.condpart22
 jmp .__Skip_Joy0_Left

.skipL098
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L099 ;  temp5  =   ( player0y - 1 )  / 8

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

.L0100 ;  temp6  =   ( player0x - 18 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #18
	lsr
	lsr
	STA temp6
.
 ; 

.L0101 ;  if temp6  <  34 then if pfread ( temp6 , temp5 )  then goto __Skip_Joy0_Left

	LDA temp6
	CMP #34
     BCS .skipL0101
.condpart23
	LDA temp6
	LDY temp5
 jsr pfread
	BNE .skip23then
.condpart24
 jmp .__Skip_Joy0_Left

.skip23then
.skipL0101
.
 ; 

.L0102 ;  temp3  =   ( player0y - 8 )  / 8

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

.L0103 ;  if temp6  <  34 then if pfread ( temp6 , temp3 )  then goto __Skip_Joy0_Left

	LDA temp6
	CMP #34
     BCS .skipL0103
.condpart25
	LDA temp6
	LDY temp3
 jsr pfread
	BNE .skip25then
.condpart26
 jmp .__Skip_Joy0_Left

.skip25then
.skipL0103
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0104 ;  player0x  =  player0x  -  1

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

.L0105 ;  if !joy0right then goto __Skip_Joy0_Right

 bit SWCHA
	BPL .skipL0105
.condpart27
 jmp .__Skip_Joy0_Right

.skipL0105
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0106 ;  _Bit3_P0_Dir_Right{3}  =  1

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

.L0107 ;  if player0x  >=  _P_Edge_Right then goto __Skip_Joy0_Right

	LDA player0x
	CMP #_P_Edge_Right
     BCC .skipL0107
.condpart28
 jmp .__Skip_Joy0_Right

.skipL0107
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0108 ;  temp5  =   ( player0y - 1 )  / 8

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

.L0109 ;  temp6  =   ( player0x - 9 )  / 4

; complex statement detected
	LDA player0x
	SEC
	SBC #9
	lsr
	lsr
	STA temp6
.
 ; 

.L0110 ;  if temp6  <  34 then if pfread ( temp6 , temp5 )  then goto __Skip_Joy0_Right

	LDA temp6
	CMP #34
     BCS .skipL0110
.condpart29
	LDA temp6
	LDY temp5
 jsr pfread
	BNE .skip29then
.condpart30
 jmp .__Skip_Joy0_Right

.skip29then
.skipL0110
.
 ; 

.L0111 ;  temp3  =   ( player0y - 8 )  / 8

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

.L0112 ;  if temp6  <  34 then if pfread ( temp6 , temp3 )  then goto __Skip_Joy0_Right

	LDA temp6
	CMP #34
     BCS .skipL0112
.condpart31
	LDA temp6
	LDY temp3
 jsr pfread
	BNE .skip31then
.condpart32
 jmp .__Skip_Joy0_Right

.skip31then
.skipL0112
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0113 ;  player0x  =  player0x  +  1

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

.L0114 ;  temp1  =  player0x  +  8

	LDA player0x
	CLC
	ADC #8
	STA temp1
.L0115 ;  temp2  =  player0y  +  8

	LDA player0y
	CLC
	ADC #8
	STA temp2
.
 ; 

.L0116 ;  temp3  =  player1x  +  4

	LDA player1x
	CLC
	ADC #4
	STA temp3
.L0117 ;  temp4  =  player1y  +  4

	LDA player1y
	CLC
	ADC #4
	STA temp4
.
 ; 

.L0118 ;  temp5  =  _Minotaur_Awareness_Size

	LDA #_Minotaur_Awareness_Size
	STA temp5
.
 ; 

.L0119 ;  if _Frame_Count  &  1 then goto __Skip_AI_Right

	LDA _Frame_Count
	AND #1
     BEQ .skipL0119
.condpart33
 jmp .__Skip_AI_Right

.skipL0119
.
 ; 

.
 ; 

.L0120 ;  if player0y  >=  temp4  +  temp5 then goto __Skip_AI_Right

; complex condition detected
	LDA temp4
	CLC
	ADC temp5
  PHA
  TSX
  PLA
	LDA player0y
	CMP  1,x
     BCC .skipL0120
.condpart34
 jmp .__Skip_AI_Right

.skipL0120
.
 ; 

.
 ; 

.L0121 ;  if temp4  <  temp5 then temp4  =  temp5

	LDA temp4
	CMP temp5
     BCS .skipL0121
.condpart35
	LDA temp5
	STA temp4
.skipL0121
.L0122 ;  if temp2  <=  temp4  -  temp5 then goto __Skip_AI_Right

; complex condition detected
	LDA temp4
	SEC
	SBC temp5
	CMP temp2
     BCC .skipL0122
.condpart36
 jmp .__Skip_AI_Right

.skipL0122
.
 ; 

.
 ; 

.L0123 ;  if temp3  <  temp5 then temp3  =  temp5

	LDA temp3
	CMP temp5
     BCS .skipL0123
.condpart37
	LDA temp5
	STA temp3
.skipL0123
.L0124 ;  if temp1  <=  temp3  -  temp5 then goto __Skip_AI_Right

; complex condition detected
	LDA temp3
	SEC
	SBC temp5
	CMP temp1
     BCC .skipL0124
.condpart38
 jmp .__Skip_AI_Right

.skipL0124
.
 ; 

.
 ; 

.L0125 ;  if player0x  >=  temp3  +  temp5 then goto __Skip_AI_Right

; complex condition detected
	LDA temp3
	CLC
	ADC temp5
  PHA
  TSX
  PLA
	LDA player0x
	CMP  1,x
     BCC .skipL0125
.condpart39
 jmp .__Skip_AI_Right

.skipL0125
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0126 ;  if player1y  <=  player0y then goto __Skip_AI_Up

	LDA player0y
	CMP player1y
     BCC .skipL0126
.condpart40
 jmp .__Skip_AI_Up

.skipL0126
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0127 ;  _Bit0_P1_Dir_Up{0}  =  1

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

.L0128 ;  if player1y  <=  _P_Edge_Top then goto __Skip_AI_Up

	LDA #_P_Edge_Top
	CMP player1y
     BCC .skipL0128
.condpart41
 jmp .__Skip_AI_Up

.skipL0128
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0129 ;  temp5  =   ( player1x - 10 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #10
	lsr
	lsr
	STA temp5
.
 ; 

.L0130 ;  temp6  =   ( player1y - 9 )  / 8

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

.L0131 ;  if temp5  <  34 then if pfread ( temp5 , temp6 )  then goto __Skip_AI_Up

	LDA temp5
	CMP #34
     BCS .skipL0131
.condpart42
	LDA temp5
	LDY temp6
 jsr pfread
	BNE .skip42then
.condpart43
 jmp .__Skip_AI_Up

.skip42then
.skipL0131
.
 ; 

.L0132 ;  temp4  =   ( player1x - 17 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #17
	lsr
	lsr
	STA temp4
.
 ; 

.L0133 ;  if temp4  <  34 then if pfread ( temp4 , temp6 )  then goto __Skip_AI_Up

	LDA temp4
	CMP #34
     BCS .skipL0133
.condpart44
	LDA temp4
	LDY temp6
 jsr pfread
	BNE .skip44then
.condpart45
 jmp .__Skip_AI_Up

.skip44then
.skipL0133
.
 ; 

.L0134 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L0135 ;  if temp3  <  34 then if pfread ( temp3 , temp6 )  then goto __Skip_AI_Up

	LDA temp3
	CMP #34
     BCS .skipL0135
.condpart46
	LDA temp3
	LDY temp6
 jsr pfread
	BNE .skip46then
.condpart47
 jmp .__Skip_AI_Up

.skip46then
.skipL0135
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0136 ;  player1y  =  player1y  -  1

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

.
 ; 

.
 ; 

.L0137 ;  if player1y  >=  player0y then goto __Skip_AI_Down

	LDA player1y
	CMP player0y
     BCC .skipL0137
.condpart48
 jmp .__Skip_AI_Down

.skipL0137
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0138 ;  _Bit1_P1_Dir_Down{1}  =  1

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

.L0139 ;  if player1y  >=  _P_Edge_Bottom then goto __Skip_AI_Down

	LDA player1y
	CMP #_P_Edge_Bottom
     BCC .skipL0139
.condpart49
 jmp .__Skip_AI_Down

.skipL0139
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0140 ;  temp5  =   ( player1x - 10 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #10
	lsr
	lsr
	STA temp5
.
 ; 

.L0141 ;  temp6  =   ( player1y )  / 8

; complex statement detected
	LDA player1y
	lsr
	lsr
	lsr
	STA temp6
.
 ; 

.L0142 ;  if temp5  <  34 then if pfread ( temp5 , temp6 )  then goto __Skip_AI_Down

	LDA temp5
	CMP #34
     BCS .skipL0142
.condpart50
	LDA temp5
	LDY temp6
 jsr pfread
	BNE .skip50then
.condpart51
 jmp .__Skip_AI_Down

.skip50then
.skipL0142
.
 ; 

.L0143 ;  temp4  =   ( player1x - 17 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #17
	lsr
	lsr
	STA temp4
.
 ; 

.L0144 ;  if temp4  <  34 then if pfread ( temp4 , temp6 )  then goto __Skip_AI_Down

	LDA temp4
	CMP #34
     BCS .skipL0144
.condpart52
	LDA temp4
	LDY temp6
 jsr pfread
	BNE .skip52then
.condpart53
 jmp .__Skip_AI_Down

.skip52then
.skipL0144
.
 ; 

.L0145 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L0146 ;  if temp3  <  34 then if pfread ( temp3 , temp6 )  then goto __Skip_AI_Down

	LDA temp3
	CMP #34
     BCS .skipL0146
.condpart54
	LDA temp3
	LDY temp6
 jsr pfread
	BNE .skip54then
.condpart55
 jmp .__Skip_AI_Down

.skip54then
.skipL0146
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0147 ;  player1y  =  player1y  +  1

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

.L0148 ;  if player1x  <=  player0x then goto __Skip_AI_Left

	LDA player0x
	CMP player1x
     BCC .skipL0148
.condpart56
 jmp .__Skip_AI_Left

.skipL0148
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0149 ;  _Bit2_P1_Dir_Left{2}  =  1

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

.L0150 ;  if player1x  <=  _P_Edge_Left then goto __Skip_AI_Left

	LDA #_P_Edge_Left
	CMP player1x
     BCC .skipL0150
.condpart57
 jmp .__Skip_AI_Left

.skipL0150
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0151 ;  temp5  =   ( player1y - 1 )  / 8

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

.L0152 ;  temp6  =   ( player1x - 18 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #18
	lsr
	lsr
	STA temp6
.
 ; 

.L0153 ;  if temp6  <  34 then if pfread ( temp6 , temp5 )  then goto __Skip_AI_Left

	LDA temp6
	CMP #34
     BCS .skipL0153
.condpart58
	LDA temp6
	LDY temp5
 jsr pfread
	BNE .skip58then
.condpart59
 jmp .__Skip_AI_Left

.skip58then
.skipL0153
.
 ; 

.L0154 ;  temp3  =   ( player1y - 8 )  / 8

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

.L0155 ;  if temp6  <  34 then if pfread ( temp6 , temp3 )  then goto __Skip_AI_Left

	LDA temp6
	CMP #34
     BCS .skipL0155
.condpart60
	LDA temp6
	LDY temp3
 jsr pfread
	BNE .skip60then
.condpart61
 jmp .__Skip_AI_Left

.skip60then
.skipL0155
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0156 ;  player1x  =  player1x  -  1

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

.
 ; 

.
 ; 

.L0157 ;  if player1x  >=  player0x then goto __Skip_AI_Right

	LDA player1x
	CMP player0x
     BCC .skipL0157
.condpart62
 jmp .__Skip_AI_Right

.skipL0157
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0158 ;  _Bit3_P1_Dir_Right{3}  =  1

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

.L0159 ;  if player1x  >=  _P_Edge_Right then goto __Skip_AI_Right

	LDA player1x
	CMP #_P_Edge_Right
     BCC .skipL0159
.condpart63
 jmp .__Skip_AI_Right

.skipL0159
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0160 ;  temp5  =   ( player1y - 1 )  / 8

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

.L0161 ;  temp6  =   ( player1x - 9 )  / 4

; complex statement detected
	LDA player1x
	SEC
	SBC #9
	lsr
	lsr
	STA temp6
.
 ; 

.L0162 ;  if temp6  <  34 then if pfread ( temp6 , temp5 )  then goto __Skip_AI_Right

	LDA temp6
	CMP #34
     BCS .skipL0162
.condpart64
	LDA temp6
	LDY temp5
 jsr pfread
	BNE .skip64then
.condpart65
 jmp .__Skip_AI_Right

.skip64then
.skipL0162
.
 ; 

.L0163 ;  temp3  =   ( player1y - 8 )  / 8

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

.L0164 ;  if temp6  <  34 then if pfread ( temp6 , temp3 )  then goto __Skip_AI_Right

	LDA temp6
	CMP #34
     BCS .skipL0164
.condpart66
	LDA temp6
	LDY temp3
 jsr pfread
	BNE .skip66then
.condpart67
 jmp .__Skip_AI_Right

.skip66then
.skipL0164
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0165 ;  player1x  =  player1x  +  1

	INC player1x
.
 ; 

.__Skip_AI_Right
 ; __Skip_AI_Right

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0166 ;  if !collision(missile0,player0) then __Skip_Gem_Collection

	bit 	CXM0P
 if ((* - .__Skip_Gem_Collection) < 127) && ((* - .__Skip_Gem_Collection) > -128)
	bvc .__Skip_Gem_Collection
 else
	bvs .2skip__Skip_Gem_Collection
	jmp .__Skip_Gem_Collection
.2skip__Skip_Gem_Collection
 endif
.L0167 ;  _Bit0_Carrying_Gem{0}  =  1

	LDA _Bit0_Carrying_Gem
	ORA #1
	STA _Bit0_Carrying_Gem
.L0168 ;  missile0x  =  200  :  missile0y  =  200

	LDA #200
	STA missile0x
	STA missile0y
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

.L0169 ;  if !collision(missile1,player0) then __Skip_Gem_Deposit

	bit 	CXM1P
 if ((* - .__Skip_Gem_Deposit) < 127) && ((* - .__Skip_Gem_Deposit) > -128)
	bpl .__Skip_Gem_Deposit
 else
	bmi .3skip__Skip_Gem_Deposit
	jmp .__Skip_Gem_Deposit
.3skip__Skip_Gem_Deposit
 endif
.L0170 ;  if !_Bit0_Carrying_Gem{0} then __Skip_Gem_Deposit

	LDA _Bit0_Carrying_Gem
	LSR
 if ((* - .__Skip_Gem_Deposit) < 127) && ((* - .__Skip_Gem_Deposit) > -128)
	bcc .__Skip_Gem_Deposit
 else
	bcs .4skip__Skip_Gem_Deposit
	jmp .__Skip_Gem_Deposit
.4skip__Skip_Gem_Deposit
 endif
.L0171 ;  _Bit0_Carrying_Gem{0}  =  0

	LDA _Bit0_Carrying_Gem
	AND #254
	STA _Bit0_Carrying_Gem
.L0172 ;  score  =  score  +  1

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
.L0173 ;  missile0x  =   ( rand  &  118 )   +  23  :  missile0y  =   ( rand  &  70 )   +  9

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

.L0174 ;  if !_Bit1_Game_Over{1} then goto __Skip_Game_Over

	LDA _Bit1_Game_Over
	AND #2
	BNE .skipL0174
.condpart68
 jmp .__Skip_Game_Over

.skipL0174
.L0175 ;  COLUPF  =  14

	LDA #14
	STA COLUPF
.L0176 ;  scorecolor  =  14

	LDA #14
	STA scorecolor
.L0177 ;  COLUBK  =  $36

	LDA #$36
	STA COLUBK
.L0178 ;  playfield:

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
pflabel1
	lda PF_data1,x
	sta playfield,x
	dex
	bpl pflabel1
.
 ; 

.L0179 ;  player0x  =  200  :  player0y  =  200

	LDA #200
	STA player0x
	STA player0y
.L0180 ;  player1x  =  200  :  player1y  =  200

	LDA #200
	STA player1x
	STA player1y
.L0181 ;  missile0x  =  200  :  missile0y  =  200

	LDA #200
	STA missile0x
	STA missile0y
.L0182 ;  missile1x  =  200  :  missile1y  =  200

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

.L0183 ;  drawscreen

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

.L0184 ;  if !switchreset then _Bit0_Reset_Restrainer{0}  =  0  :  goto __Main_Loop

 lda #1
 bit SWCHB
	BEQ .skipL0184
.condpart69
	LDA _Bit0_Reset_Restrainer
	AND #254
	STA _Bit0_Reset_Restrainer
 jmp .__Main_Loop

.skipL0184
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0185 ;  if _Bit0_Reset_Restrainer{0} then goto __Main_Loop

	LDA _Bit0_Reset_Restrainer
	LSR
	BCC .skipL0185
.condpart70
 jmp .__Main_Loop

.skipL0185
.
 ; 

.
 ; 

.
 ; 

.
 ; 

.L0186 ;  goto __Start_Restart

 jmp .__Start_Restart

 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL055_0
	.byte    %11100111
	.byte    %00100100
	.byte    %00100100
	.byte    %10011001
	.byte    %01011010
	.byte    %00111100
	.byte    %00011000
	.byte    %11111111
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL056_1
	.byte    %01100110
	.byte    %00100100
	.byte    %10011001
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
