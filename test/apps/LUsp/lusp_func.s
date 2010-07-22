	.file	"lusp_func.c"
.text
	.align	3
	.global	spe_bmodd
	.type	spe_bmodd, @function
spe_bmodd:
	il	$20,8
	lqd	$16,0($5)
	il	$19,4
	hbrp	# 1
	nop	127
	shlqbyi	$12,$4,0
	il	$18,12
	lqx	$15,$5,$20
	ai	$10,$5,8
	lqx	$14,$5,$19
	ai	$11,$5,4
	lqx	$13,$5,$18
	ori	$19,$3,0
	rotqby	$17,$16,$5
	ai	$5,$5,12
	rotqby	$7,$15,$10
	rotqby	$9,$14,$11
	hbrp	# 2
	cgti	$2,$17,0
	rotqby	$6,$13,$5
	ori	$16,$17,0
	shli	$8,$7,2
	ori	$18,$9,0
	ori	$21,$6,0
	cgti	$22,$9,0
	lnop
	ai	$8,$8,4
	lnop
	brz	$2,.L24
	lnop
.L21:
	shli	$17,$21,2
	shlqbyi	$7,$19,0
	il	$14,0
	lnop
	ai	$15,$16,-1
	lnop
	brz	$22,.L25
	lnop
.L19:
	ilhu	$26,-32768
	lqd	$37,0($12)
	cgti	$23,$15,0
	lqd	$38,0($7)
	cwd	$25,0($7)
	rotqby	$35,$37,$12
	rotqby	$32,$38,$7
	frest	$36,$35
	fi	$33,$35,$36
	fm	$34,$32,$33
	fnms	$31,$34,$35,$32
	fma	$29,$31,$33,$34
	ai	$28,$29,1
	fnms	$30,$31,$28,$32
	cgti	$27,$30,-1
	selb	$3,$29,$28,$27
	xor	$13,$3,$26
	shufb	$4,$3,$38,$25
	stqd	$4,0($7)
	brz	$23,.L7
	ai	$10,$16,-1
	hbrr	.L28,.L20
	il	$5,4
	cgti	$39,$10,1
	nop	127
	brnz	$39,.L26
.L20:
	a	$61,$5,$12
	lqx	$58,$5,$7
	a	$59,$5,$7
	lqx	$60,$5,$12
	ai	$10,$10,-1
	cwx	$55,$5,$7
	rotqby	$57,$58,$59
	rotqby	$56,$60,$61
	fma	$54,$13,$56,$57
	shufb	$52,$54,$58,$55
	nop	127
	stqx	$52,$5,$7
	ai	$5,$5,4
.L28:
	brnz	$10,.L20
.L7:
	ai	$14,$14,1
	ceq	$51,$18,$14
	brnz	$51,.L10
	a	$7,$7,$17
	br	.L19
.L25:
	ai	$15,$16,-1
.L10:
	ori	$16,$15,0
	biz	$15,$lr
	ai	$19,$19,4
	a	$12,$12,$8
	br	.L21
.L24:
	bi	$lr
.L26:
	ai	$10,$10,0
	lnop
.L9:
	a	$50,$5,$7
	lqx	$49,$5,$7
	a	$48,$5,$12
	lqx	$47,$5,$12
	ori	$40,$5,0
	hbrr	.L27,.L9
	ai	$10,$10,-1
	cwx	$44,$5,$7
	ai	$5,$5,4
	rotqby	$46,$49,$50
	rotqby	$45,$47,$48
	fma	$43,$13,$45,$46
	shufb	$41,$43,$49,$44
	stqx	$41,$40,$7
.L27:
	brnz	$10,.L9
	br	.L7
	.size	spe_bmodd, .-spe_bmodd
	.align	3
	.global	spe_daxpy
	.type	spe_daxpy, @function
spe_daxpy:
	cgti	$2,$5,0
	biz	$2,$lr
	cgti	$7,$5,1
	fsmbi	$9,0
	nop	127
	brnz	$7,.L39
.L38:
	a	$29,$9,$4
	lqx	$26,$9,$3
	a	$27,$9,$3
	lqx	$28,$9,$4
	ai	$5,$5,-1
	hbrr	.L41,.L38
	cwx	$23,$9,$3
	rotqby	$25,$26,$27
	rotqby	$24,$28,$29
	fma	$22,$6,$24,$25
	shufb	$20,$22,$26,$23
	stqx	$20,$9,$3
	ai	$9,$9,4
.L41:
	brnz	$5,.L38
	nop	127
	bi	$lr
.L39:
	ai	$5,$5,0
	hbrr	.L40,.L32
.L32:
	a	$19,$9,$3
	lqx	$18,$9,$3
	a	$12,$9,$4
	lqx	$17,$9,$4
	ori	$13,$9,0
	cwx	$15,$9,$3
	ai	$5,$5,-1
	ai	$9,$9,4
	rotqby	$8,$18,$19
	rotqby	$16,$17,$12
	fma	$11,$6,$16,$8
	shufb	$10,$11,$18,$15
	stqx	$10,$13,$3
.L40:
	brnz	$5,.L32
	bi	$lr
	.size	spe_daxpy, .-spe_daxpy
	.align	3
	.global	spe_daxpy_vec
	.type	spe_daxpy_vec, @function
spe_daxpy_vec:
	rotmai	$14,$5,-2
	shlqbyi	$9,$3,0
	ila	$2,66051
	shufb	$12,$6,$6,$2
	cgti	$3,$14,0
	biz	$3,$lr
	ori	$10,$9,0
	hbrr	.L48,.L45
	ori	$11,$4,0
	fsmbi	$13,0
.L45:
	ai	$13,$13,4
	lqd	$20,16($11)
	nop	127
	lqd	$19,32($11)
	cgt	$6,$14,$13
	lqd	$18,48($11)
	nop	127
	lqd	$17,0($11)
	ai	$11,$11,64
	lqd	$21,16($10)
	lqd	$7,32($10)
	lqd	$8,48($10)
	lqd	$9,0($10)
	fma	$16,$12,$20,$21
	fma	$15,$12,$19,$7
	fma	$4,$12,$18,$8
	fma	$5,$12,$17,$9
	stqd	$16,16($10)
	stqd	$15,32($10)
	stqd	$4,48($10)
	nop	127
	stqd	$5,0($10)
	ai	$10,$10,64
.L48:
	brnz	$6,.L45
	bi	$lr
	.size	spe_daxpy_vec, .-spe_daxpy_vec
	.align	3
	.global	spe_bmod
	.type	spe_bmod, @function
spe_bmod:
	il	$25,8
	lqd	$22,0($6)
	il	$23,12
	hbrp	# 1
	nop	127
	lqd	$21,16($6)
	il	$24,4
	lqx	$18,$6,$25
	il	$19,20
	lqx	$15,$6,$23
	ai	$13,$6,8
	lqx	$16,$6,$24
	ai	$10,$6,12
	lqx	$14,$6,$19
	ai	$8,$6,4
	rotqby	$9,$22,$6
	ori	$27,$3,0
	hbrp	# 2
	nop	127
	rotqby	$20,$21,$6
	ai	$6,$6,20
	rotqby	$17,$18,$13
	ori	$19,$4,0
	rotqby	$12,$15,$10
	ori	$24,$5,0
	rotqby	$11,$16,$8
	rotmai	$16,$9,-2
	rotqby	$7,$14,$6
	cgti	$2,$17,0
	shlqbyi	$29,$20,0
	shli	$12,$12,2
	shlqbyi	$30,$17,0
	ori	$23,$11,0
	fsmbi	$25,0
	ori	$26,$7,0
	lnop
	cgti	$28,$11,0
	brz	$2,.L66
.L65:
	shli	$22,$29,2
	shlqbyi	$15,$24,0
	shli	$21,$26,2
	shlqbyi	$18,$27,0
	il	$17,0
	lnop
	cgti	$20,$16,0
	brz	$28,.L58
.L64:
	ilhu	$5,-32768
	lqd	$33,0($15)
	ila	$3,66051
	rotqby	$32,$33,$15
	xor	$4,$32,$5
	shufb	$13,$4,$4,$3
	nop	127
	brz	$20,.L55
	ori	$10,$18,0
	hbrr	.L67,.L57
	ori	$11,$19,0
	fsmbi	$14,0
.L57:
	ai	$14,$14,4
	lqd	$45,16($11)
	nop	127
	lqd	$43,32($11)
	cgt	$34,$16,$14
	lqd	$41,48($11)
	nop	127
	lqd	$39,0($11)
	ai	$11,$11,64
	lqd	$46,16($10)
	lqd	$44,32($10)
	lqd	$42,48($10)
	lqd	$40,0($10)
	fma	$38,$13,$45,$46
	fma	$37,$13,$43,$44
	fma	$36,$13,$41,$42
	fma	$35,$13,$39,$40
	stqd	$38,16($10)
	stqd	$37,32($10)
	stqd	$36,48($10)
	nop	127
	stqd	$35,0($10)
	ai	$10,$10,64
.L67:
	brnz	$34,.L57
.L55:
	ai	$17,$17,1
	ceq	$47,$23,$17
	brnz	$47,.L58
	a	$15,$15,$22
	a	$18,$18,$21
	br	.L64
.L58:
	ai	$25,$25,1
	ceq	$48,$30,$25
	binz	$48,$lr
	a	$19,$19,$12
	ai	$24,$24,4
	br	.L65
.L66:
	bi	$lr
	.size	spe_bmod, .-spe_bmod
	.align	3
	.global	spe_bdiv
	.type	spe_bdiv, @function
spe_bdiv:
	il	$17,12
	lqd	$11,0($5)
	il	$15,8
	hbr	.L87,$lr
	ai	$10,$5,12
	lqx	$13,$5,$17
	il	$16,4
	lqx	$6,$5,$15
	ai	$2,$5,4
	lqx	$12,$5,$16
	rotqby	$14,$11,$5
	ai	$5,$5,8
	rotqby	$8,$13,$10
	rotqby	$6,$6,$5
	ori	$11,$14,0
	rotqby	$9,$12,$2
	cgti	$7,$8,0
	shlqbyi	$15,$8,0
	biz	$7,$lr
	lnop
	shli	$18,$11,2
	fsmbi	$2,0
	shli	$14,$9,2
	shlqbyi	$17,$3,0
	rotmai	$19,$6,-2
	ai	$16,$2,1
	a	$20,$3,$18
	ceq	$3,$15,$16
	a	$21,$4,$14
	ai	$24,$14,4
	cgti	$23,$19,0
.L87:
	binz	$3,$lr
.L84:
	ori	$25,$21,0
	shlqbyi	$26,$20,0
	ori	$22,$16,0
	ilhu	$28,-32768
	ila	$27,66051
.L73:
	lqd	$30,0($25)
	rotqby	$29,$30,$25
	xor	$4,$28,$29
	shufb	$12,$4,$4,$27
	brz	$23,.L74
	ori	$10,$26,0
	hbrr	.L86,.L76
	ori	$11,$17,0
	fsmbi	$13,0
.L76:
	ai	$13,$13,4
	lqd	$42,16($11)
	nop	127
	lqd	$40,32($11)
	cgt	$31,$19,$13
	lqd	$38,48($11)
	nop	127
	lqd	$36,0($11)
	ai	$11,$11,64
	lqd	$43,16($10)
	lqd	$41,32($10)
	lqd	$39,48($10)
	lqd	$37,0($10)
	fma	$35,$12,$42,$43
	fma	$34,$12,$40,$41
	fma	$33,$12,$38,$39
	fma	$32,$12,$36,$37
	stqd	$35,16($10)
	stqd	$34,32($10)
	stqd	$33,48($10)
	nop	127
	stqd	$32,0($10)
	ai	$10,$10,64
.L86:
	brnz	$31,.L76
.L74:
	ai	$22,$22,1
	cgt	$44,$15,$22
	brnz	$44,.L83
	hbr	.L85,$lr
	ori	$2,$16,0
	nop	$127
	ai	$16,$2,1
	nop	$127
	ceq	$3,$15,$16
	a	$17,$17,$18
	a	$20,$20,$18
	a	$21,$21,$24
.L85:
	binz	$3,$lr
	br	.L84
.L83:
	a	$25,$25,$14
	a	$26,$26,$18
	br	.L73
	.size	spe_bdiv, .-spe_bdiv
	.align	3
	.global	execute_task
	.type	execute_task, @function
execute_task:
	ori	$25,$4,0
	lqd	$4,128($3)
	ori	$6,$3,0
	hbrp	# 1
	rotqbyi	$5,$4,1
	ceqbi	$2,$5,1
	xsbh	$3,$2
	brhnz	$3,.L91
	clgtbi	$8,$5,0
	xsbh	$7,$8
	nop	127
	brhnz	$7,.L144
	il	$27,12
	lqd	$31,0($6)
	il	$26,4
	lqd	$29,16($6)
	il	$24,8
	hbrp	# 2
	lqd	$32,0($25)
	rotqbyi	$30,$31,4
	rotqbyi	$28,$29,4
	ori	$17,$32,0
	a	$9,$32,$30
	lnop
	a	$21,$9,$28
	lqx	$23,$9,$28
	ai	$20,$21,12
	lqx	$19,$21,$27
	ai	$16,$21,4
	lqx	$15,$21,$26
	ai	$14,$21,8
	lqx	$13,$21,$24
	rotqby	$22,$23,$21
	rotqby	$18,$19,$20
	nop	127
	rotqby	$6,$15,$16
	ori	$10,$22,0
	rotqby	$12,$13,$14
	cgti	$11,$18,0
	shlqbyi	$21,$18,0
	ori	$3,$6,0
	brz	$11,.L115
	shli	$22,$3,2
	fsmbi	$26,0
	rotmai	$24,$12,-2
	shli	$18,$10,2
	a	$15,$9,$22
	cgti	$29,$24,0
	a	$16,$32,$18
	ai	$23,$22,4
.L96:
	ai	$26,$26,1
	ceq	$33,$21,$26
	nop	127
	brz	$33,.L145
.L115:
	il	$3,3
	lqd	$79,0($25)
	il	$5,0
	cwd	$4,4($sp)
	shufb	$78,$3,$79,$4
	stqd	$78,0($25)
.L93:
	ori	$3,$5,0
	bi	$lr
.L91:
	il	$67,8
	lqd	$71,0($6)
	il	$66,12
	lqd	$69,16($6)
	il	$65,4
	lqd	$72,0($25)
	rotqbyi	$70,$71,4
	rotqbyi	$68,$69,4
	ori	$18,$72,0
	a	$11,$72,$70
	a	$62,$11,$68
	lqx	$64,$11,$68
	ai	$60,$62,8
	lqx	$59,$62,$67
	ai	$54,$62,4
	lqx	$17,$62,$65
	ai	$57,$62,12
	lqx	$56,$62,$66
	rotqby	$63,$64,$62
	rotqby	$58,$59,$60
	nop	127
	rotqby	$53,$17,$54
	cgti	$51,$63,0
	rotqby	$55,$56,$57
	ori	$15,$63,0
	shli	$52,$58,2
	ori	$17,$53,0
	ori	$21,$55,0
	cgti	$22,$53,0
	lnop
	ai	$20,$52,4
	brz	$51,.L115
.L140:
	shli	$16,$21,2
	shlqbyi	$7,$18,0
	il	$13,0
	lnop
	ai	$14,$15,-1
	brz	$22,.L146
.L138:
	ilhu	$78,-32768
	lqd	$19,0($11)
	cgti	$73,$14,0
	lqd	$23,0($7)
	cwd	$77,0($7)
	rotqby	$12,$19,$11
	rotqby	$2,$23,$7
	frest	$6,$12
	fi	$8,$12,$6
	fm	$9,$2,$8
	fnms	$10,$9,$12,$2
	fma	$4,$10,$8,$9
	ai	$5,$4,1
	fnms	$3,$10,$5,$2
	cgti	$79,$3,-1
	selb	$76,$4,$5,$79
	xor	$12,$76,$78
	shufb	$74,$76,$23,$77
	stqd	$74,0($7)
	brz	$73,.L109
	ai	$9,$15,-1
	hbrr	.L152,.L139
	il	$5,4
	cgti	$24,$9,1
	nop	127
	brnz	$24,.L147
.L139:
	a	$28,$5,$11
	lqx	$26,$5,$7
	a	$6,$5,$7
	lqx	$27,$5,$11
	ai	$9,$9,-1
	cwx	$10,$5,$7
	rotqby	$24,$26,$6
	rotqby	$23,$27,$28
	fma	$2,$12,$23,$24
	shufb	$8,$2,$26,$10
	nop	127
	stqx	$8,$5,$7
	ai	$5,$5,4
.L152:
	brnz	$9,.L139
.L109:
	ai	$13,$13,1
	ceq	$37,$17,$13
	brnz	$37,.L112
	a	$7,$7,$16
	br	.L138
.L146:
	ai	$14,$15,-1
.L112:
	ori	$15,$14,0
	brz	$14,.L115
	ai	$18,$18,4
	a	$11,$11,$20
	br	.L140
.L144:
	ceqbi	$10,$5,2
	il	$5,1
	xsbh	$9,$10
	nop	127
	brhz	$9,.L93
	il	$50,8
	lqd	$21,0($6)
	il	$47,12
	lqd	$17,16($6)
	il	$49,20
	lqd	$55,0($25)
	il	$48,4
	lqd	$53,32($6)
	il	$23,0
	rotqbyi	$54,$21,4
	rotqbyi	$52,$17,4
	ori	$27,$55,0
	rotqbyi	$51,$53,4
	a	$21,$55,$54
	a	$24,$21,$52
	lnop
	a	$14,$24,$51
	lqx	$46,$24,$51
	ai	$45,$14,8
	lqx	$44,$14,$50
	ai	$40,$14,12
	lqx	$15,$14,$47
	ai	$42,$14,4
	lqx	$22,$14,$48
	ai	$38,$14,20
	lqx	$20,$14,$49
	lqd	$13,16($14)
	rotqby	$39,$46,$14
	rotqby	$43,$44,$45
	rotqby	$16,$15,$40
	nop	127
	rotqby	$41,$22,$42
	rotmai	$15,$39,-2
	rotqby	$18,$20,$38
	cgti	$7,$43,0
	rotqby	$11,$13,$14
	shli	$26,$16,2
	shlqbyi	$30,$43,0
	ori	$22,$41,0
	ori	$28,$18,0
	ori	$29,$11,0
	lnop
	cgti	$31,$41,0
	brz	$7,.L115
.L143:
	shli	$77,$23,2
	shlqbyi	$17,$27,0
	shli	$19,$29,2
	fsmbi	$16,0
	shli	$20,$28,2
	cgti	$18,$15,0
	a	$14,$24,$77
	brz	$31,.L123
.L142:
	ilhu	$58,-32768
	lqd	$61,0($14)
	ila	$57,66051
	rotqby	$60,$61,$14
	xor	$56,$60,$58
	shufb	$12,$56,$56,$57
	nop	127
	brz	$18,.L120
	ori	$10,$17,0
	hbrr	.L151,.L122
	ori	$11,$21,0
	fsmbi	$13,0
.L122:
	ai	$13,$13,4
	lqd	$73,16($11)
	nop	127
	lqd	$71,32($11)
	cgt	$62,$15,$13
	lqd	$69,48($11)
	nop	127
	lqd	$67,0($11)
	ai	$11,$11,64
	lqd	$74,16($10)
	lqd	$72,32($10)
	lqd	$70,48($10)
	lqd	$68,0($10)
	fma	$66,$12,$73,$74
	fma	$65,$12,$71,$72
	fma	$64,$12,$69,$70
	fma	$63,$12,$67,$68
	stqd	$66,16($10)
	stqd	$65,32($10)
	stqd	$64,48($10)
	nop	127
	stqd	$63,0($10)
	ai	$10,$10,64
.L151:
	brnz	$62,.L122
.L120:
	ai	$16,$16,1
	ceq	$75,$22,$16
	brnz	$75,.L123
	a	$14,$14,$19
	a	$17,$17,$20
	br	.L142
.L145:
	ori	$14,$15,0
	shlqbyi	$19,$16,0
	ori	$13,$26,0
	ilhu	$27,-32768
	ila	$28,66051
.L98:
	lqd	$36,0($14)
	rotqby	$35,$36,$14
	xor	$34,$27,$35
	shufb	$12,$34,$34,$28
	brz	$29,.L99
	ori	$10,$19,0
	hbrr	.L150,.L101
	ori	$11,$17,0
	fsmbi	$20,0
.L101:
	ai	$20,$20,4
	lqd	$48,16($11)
	nop	127
	lqd	$46,32($11)
	cgt	$37,$24,$20
	lqd	$44,48($11)
	nop	127
	lqd	$42,0($11)
	ai	$11,$11,64
	lqd	$49,16($10)
	lqd	$47,32($10)
	lqd	$45,48($10)
	lqd	$43,0($10)
	fma	$41,$12,$48,$49
	fma	$40,$12,$46,$47
	fma	$39,$12,$44,$45
	fma	$38,$12,$42,$43
	stqd	$41,16($10)
	stqd	$40,32($10)
	stqd	$39,48($10)
	nop	127
	stqd	$38,0($10)
	ai	$10,$10,64
.L150:
	brnz	$37,.L101
.L99:
	ai	$13,$13,1
	cgt	$50,$21,$13
	brnz	$50,.L148
	a	$17,$17,$18
	a	$16,$16,$18
	a	$15,$15,$23
	br	.L96
.L123:
	ai	$23,$23,1
	ceq	$76,$30,$23
	brnz	$76,.L115
	a	$21,$21,$26
	br	.L143
.L148:
	a	$14,$14,$22
	a	$19,$19,$18
	br	.L98
.L147:
	ai	$9,$9,0
.L111:
	a	$36,$5,$7
	lqx	$35,$5,$7
	a	$34,$5,$11
	lqx	$33,$5,$11
	ori	$26,$5,0
	hbrr	.L149,.L111
	ai	$9,$9,-1
	cwx	$30,$5,$7
	ai	$5,$5,4
	rotqby	$32,$35,$36
	rotqby	$31,$33,$34
	fma	$29,$12,$31,$32
	shufb	$27,$29,$35,$30
	stqx	$27,$26,$7
.L149:
	brnz	$9,.L111
	br	.L109
	.size	execute_task, .-execute_task
	.ident	"GCC: (GNU) 4.1.1"
