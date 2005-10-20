#define FRAMEPOINTER
/* #define WHASHSTRMAX_HALVED */

#ifdef __ELF__
#define GLOBAL(name) name
#else
#ifdef __STDC__
#define GLOBAL(name) _##name
#else
#define GLOBAL(name) _/**/name
#endif
#endif

.data
_T:
	.byte   1,  87,  49,  12, 176, 178, 102, 166
	.byte 121, 193,   6,  84, 249, 230,  44, 163
	.byte  14, 197, 213, 181, 161,  85, 218,  80
	.byte  64, 239,  24, 226, 236, 142,  38, 200
	.byte 110, 177, 104, 103, 141, 253, 255,  50
	.byte  77, 101,  81,  18,  45,  96,  31, 222
	.byte  25, 107, 190,  70,  86, 237, 240,  34
	.byte  72, 242,  20, 214, 244, 227, 149, 235
	.byte  97, 234,  57,  22,  60, 250,  82, 175
	.byte 208,   5, 127, 199, 111,  62, 135, 248
	.byte 174, 169, 211,  58,  66, 154, 106, 195
	.byte 245, 171,  17, 187, 182, 179,   0, 243
	.byte 132,  56, 148,  75, 128, 133, 158, 100
	.byte 130, 126,  91,  13, 153, 246, 216, 219
	.byte 119,  68, 223,  78,  83,  88, 201,  99
	.byte 122,  11,  92,  32, 136, 114,  52,  10
	.byte 138,  30,  48, 183, 156,  35,  61,  26
	.byte 143,  74, 251,  94, 129, 162,  63, 152
	.byte 170,   7, 115, 167, 241, 206,   3, 150
	.byte  55,  59, 151, 220,  90,  53,  23, 131
	.byte 125, 173,  15, 238,  79,  95,  89,  16
	.byte 105, 137, 225, 224, 217, 160,  37, 123
	.byte 118,  73,   2, 157,  46, 116,   9, 145
	.byte 134, 228, 207, 212, 202, 215,  69, 229
	.byte  27, 188,  67, 124, 168, 252,  42,   4
	.byte  29, 108,  21, 247,  19, 205,  39, 203
	.byte 233,  40, 186, 147, 198, 192, 155,  33
	.byte 164, 191,  98, 204, 165, 180, 117,  76
	.byte 140,  36, 210, 172,  41,  54, 159,   8
	.byte 185, 232, 113, 196, 231,  47, 146, 120
	.byte  51,  65,  28, 144, 254, 221,  93, 189
	.byte 194, 139, 112,  43,  71, 109, 184, 209

.text

/*
 * This whashstr() implementation rounds up the
 * maximum length to an odd number.
 */

/*
 * %al : low byte
 * %cl, %dl : alternating high byte and character read
 * %edi: read pointer
 * %esi: _T
 * %ebx: counter
 */

	.align 2
.globl GLOBAL(whashstr)
GLOBAL(whashstr):
#ifdef FRAMEPOINTER
	pushl %ebp
	movl %esp,%ebp
	pushl %ebx
	pushl %edi
	pushl %esi
	movl 8(%ebp),%edi
	movl 12(%ebp),%ebx
#else
	pushl %ebx
	pushl %edi
	pushl %esi
	movl 16(%esp),%edi
	movl 20(%esp),%ebx
#endif
	movzbl (%edi),%eax
	testl %eax,%eax
	je Lret
#ifndef WHASHSTRMAX_HALVED
	shrl $1,%ebx
#endif
	movl %eax,%edx
	movl $_T,%esi
	incb %al
	leal 1(%edi,%ebx,2),%edi
	negl %ebx
	xorl %ecx,%ecx
	.align 2,0x90
Loop:
	movb (%edi,%ebx,2),%cl
	testb %cl,%cl
	je Lcombine_da
	xorb %cl,%al
	movb (%esi,%eax),%al
	xorb (%esi,%edx),%cl
	movb 1(%edi,%ebx,2),%dl
	testb %dl,%dl
	je Lcombine_ca
	xorb %dl,%al
	movb (%esi,%eax),%al
	xorb (%esi,%ecx),%dl
	incl %ebx
	jne Loop
	.align 2,0x90
Lcombine_da:
	movb %dl,%cl
Lcombine_ca:
	movb (%esi,%ecx),%ah
Lret:
	popl %esi
	popl %edi
	popl %ebx
#ifdef FRAMEPOINTER
	popl %ebp
#endif
	ret

	.align 2
.globl GLOBAL(chashstr)
GLOBAL(chashstr):
	xorl %eax,%eax
#ifdef FRAMEPOINTER
	pushl %ebp
	movl %esp,%ebp
	movl 12(%ebp),%ecx
	movl 8(%ebp),%edx
#else
	movl 8(%esp),%ecx
	movl 4(%esp),%edx
#endif
	pushl %ebx
	.align 2,0x90
L13:
	movb (%edx),%bl
	testb %bl,%bl
	je L11
	incl %edx
	xorb %bl,%al
	movb _T(%eax),%al
	decl %ecx
	jne L13
L11:
	popl %ebx
#ifdef FRAMEPOINTER
	popl %ebp
#endif
	ret
