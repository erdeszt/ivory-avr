	.file	"blink.c"
__SP_H__ = 0x3e
__SP_L__ = 0x3d
__SREG__ = 0x3f
__tmp_reg__ = 0
__zero_reg__ = 1
 ;  GNU C11 (GCC) version 5.4.0 (avr)
 ; 	compiled by GNU C version 7.2.0, GMP version 6.1.2, MPFR version 4.0.1-rc1, MPC version 1.1.0
 ;  warning: MPFR header version 4.0.1-rc1 differs from library version 4.0.1.
 ;  GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
 ;  options passed:  -fpreprocessed blink.i -mn-flash=1 -mno-skip-bug
 ;  -mmcu=avr5 -Os -Wall -fno-exceptions -fno-asynchronous-unwind-tables
 ;  -fverbose-asm
 ;  options enabled:  -Wmisspelled-isr -faggressive-loop-optimizations
 ;  -falign-functions -falign-jumps -falign-labels -falign-loops
 ;  -fauto-inc-dec -fbranch-count-reg -fchkp-check-incomplete-type
 ;  -fchkp-check-read -fchkp-check-write -fchkp-instrument-calls
 ;  -fchkp-narrow-bounds -fchkp-optimize -fchkp-store-bounds
 ;  -fchkp-use-static-bounds -fchkp-use-static-const-bounds
 ;  -fchkp-use-wrappers -fcombine-stack-adjustments -fcommon -fcompare-elim
 ;  -fcprop-registers -fcrossjumping -fcse-follow-jumps -fdefer-pop
 ;  -fdevirtualize -fdevirtualize-speculatively -fdwarf2-cfi-asm
 ;  -fearly-inlining -feliminate-unused-debug-types
 ;  -fexpensive-optimizations -fforward-propagate -ffunction-cse -fgcse
 ;  -fgcse-lm -fgnu-runtime -fgnu-unique -fguess-branch-probability
 ;  -fhoist-adjacent-loads -fident -fif-conversion -fif-conversion2
 ;  -findirect-inlining -finline -finline-atomics -finline-functions
 ;  -finline-functions-called-once -finline-small-functions -fipa-cp
 ;  -fipa-cp-alignment -fipa-icf -fipa-icf-functions -fipa-icf-variables
 ;  -fipa-profile -fipa-pure-const -fipa-ra -fipa-reference -fipa-sra
 ;  -fira-hoist-pressure -fira-share-save-slots -fira-share-spill-slots
 ;  -fisolate-erroneous-paths-dereference -fivopts -fkeep-static-consts
 ;  -fleading-underscore -flifetime-dse -flra-remat -flto-odr-type-merging
 ;  -fmath-errno -fmerge-constants -fmerge-debug-strings
 ;  -fmove-loop-invariants -fomit-frame-pointer -foptimize-sibling-calls
 ;  -fpartial-inlining -fpeephole -fpeephole2 -fprefetch-loop-arrays
 ;  -freg-struct-return -freorder-blocks -freorder-functions
 ;  -frerun-cse-after-loop -fsched-critical-path-heuristic
 ;  -fsched-dep-count-heuristic -fsched-group-heuristic -fsched-interblock
 ;  -fsched-last-insn-heuristic -fsched-rank-heuristic -fsched-spec
 ;  -fsched-spec-insn-heuristic -fsched-stalled-insns-dep -fschedule-fusion
 ;  -fsemantic-interposition -fshow-column -fshrink-wrap -fsigned-zeros
 ;  -fsplit-ivs-in-unroller -fsplit-wide-types -fssa-phiopt -fstdarg-opt
 ;  -fstrict-aliasing -fstrict-overflow -fstrict-volatile-bitfields
 ;  -fsync-libcalls -fthread-jumps -ftoplevel-reorder -ftrapping-math
 ;  -ftree-bit-ccp -ftree-builtin-call-dce -ftree-ccp -ftree-ch
 ;  -ftree-coalesce-vars -ftree-copy-prop -ftree-copyrename -ftree-dce
 ;  -ftree-dominator-opts -ftree-dse -ftree-forwprop -ftree-fre
 ;  -ftree-loop-if-convert -ftree-loop-im -ftree-loop-ivcanon
 ;  -ftree-loop-optimize -ftree-parallelize-loops= -ftree-phiprop
 ;  -ftree-pre -ftree-pta -ftree-reassoc -ftree-scev-cprop -ftree-sink
 ;  -ftree-slsr -ftree-sra -ftree-switch-conversion -ftree-tail-merge
 ;  -ftree-ter -ftree-vrp -funit-at-a-time -fverbose-asm
 ;  -fzero-initialized-in-bss

	.text
.global	delay_init_8
	.type	delay_init_8, @function
delay_init_8:
/* prologue: function */
/* frame size = 0 */
/* stack size = 0 */
.L__stack_usage = 0
	in r24,0x25	 ;  D.1679, MEM[(volatile uint8_t *)69B]
	out 0x25,r24	 ;  MEM[(volatile uint8_t *)69B], D.1679
	ret
	.size	delay_init_8, .-delay_init_8
.global	delay_init
	.type	delay_init, @function
delay_init:
/* prologue: function */
/* frame size = 0 */
/* stack size = 0 */
.L__stack_usage = 0
	sts 128,__zero_reg__	 ;  MEM[(volatile uint8_t *)128B],
	ldi r24,lo8(1)	 ;  tmp44,
	sts 129,r24	 ;  MEM[(volatile uint8_t *)129B], tmp44
	ret
	.size	delay_init, .-delay_init
.global	delay_X_8
	.type	delay_X_8, @function
delay_X_8:
/* prologue: function */
/* frame size = 0 */
/* stack size = 0 */
.L__stack_usage = 0
	ldi r25,0	 ;  idx
.L4:
	cp r25,r24	 ;  idx, interval
	breq .L9	 ; ,
	out 0x26,__zero_reg__	 ;  MEM[(volatile uint8_t *)70B],
.L5:
	in r18,0x26	 ;  D.1688, MEM[(volatile uint8_t *)70B]
	cpi r18,lo8(100)	 ;  D.1688,
	brlo .L5	 ; ,
	subi r25,lo8(-(1))	 ;  idx,
	rjmp .L4	 ; 
.L9:
/* epilogue start */
	ret
	.size	delay_X_8, .-delay_X_8
.global	delay_X
	.type	delay_X, @function
delay_X:
/* prologue: function */
/* frame size = 0 */
/* stack size = 0 */
.L__stack_usage = 0
	ldi r18,0	 ;  idx
	ldi r19,0	 ;  idx
.L11:
	cp r18,r24	 ;  idx, interval
	cpc r19,r25	 ;  idx, interval
	breq .L15	 ; ,
	sts 132+1,__zero_reg__	 ;  MEM[(volatile uint16_t *)132B],
	sts 132,__zero_reg__	 ;  MEM[(volatile uint16_t *)132B],
.L12:
	lds r20,132	 ;  current_tick, MEM[(volatile uint16_t *)132B]
	lds r21,132+1	 ;  current_tick, MEM[(volatile uint16_t *)132B]
	cpi r20,-128	 ;  current_tick,
	sbci r21,62	 ;  current_tick,
	brlo .L12	 ; ,
	subi r18,-1	 ;  idx,
	sbci r19,-1	 ;  idx,
	rjmp .L11	 ; 
.L15:
/* epilogue start */
	ret
	.size	delay_X, .-delay_X
.global	main_8
	.type	main_8, @function
main_8:
/* prologue: function */
/* frame size = 0 */
/* stack size = 0 */
.L__stack_usage = 0
	ldi r24,lo8(1)	 ;  tmp44,
	out 0x4,r24	 ;  MEM[(volatile uint8_t *)36B], tmp44
	call delay_init_8	 ; 
	ldi r28,lo8(1)	 ;  tmp48,
.L17:
	out 0x5,r28	 ;  MEM[(volatile uint8_t *)37B], tmp48
	ldi r24,lo8(-56)	 ; ,
	call delay_X_8	 ; 
	out 0x5,__zero_reg__	 ;  MEM[(volatile uint8_t *)37B],
	ldi r24,lo8(-56)	 ; ,
	call delay_X_8	 ; 
	rjmp .L17	 ; 
	.size	main_8, .-main_8
.global	main_16
	.type	main_16, @function
main_16:
/* prologue: function */
/* frame size = 0 */
/* stack size = 0 */
.L__stack_usage = 0
	ldi r24,lo8(2)	 ;  tmp44,
	out 0x4,r24	 ;  MEM[(volatile uint8_t *)36B], tmp44
	call delay_init	 ; 
	ldi r28,lo8(2)	 ;  tmp48,
.L19:
	out 0x5,r28	 ;  MEM[(volatile uint8_t *)37B], tmp48
	ldi r24,lo8(-56)	 ; ,
	ldi r25,0	 ; 
	call delay_X	 ; 
	out 0x5,__zero_reg__	 ;  MEM[(volatile uint8_t *)37B],
	ldi r24,lo8(-56)	 ; ,
	ldi r25,0	 ; 
	call delay_X	 ; 
	rjmp .L19	 ; 
	.size	main_16, .-main_16
	.section	.text.startup,"ax",@progbits
.global	main
	.type	main, @function
main:
/* prologue: function */
/* frame size = 0 */
/* stack size = 0 */
.L__stack_usage = 0
	call main_16	 ; 
	.size	main, .-main
	.ident	"GCC: (GNU) 5.4.0"
