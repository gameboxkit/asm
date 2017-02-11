/*
 * Gamebox 65C816 Assembler
 *
 * Copyright (C) 2017 Charles E. Youse (charles@gameboxkit.com)
 *
 */

#define LINE_SIZE 	256
#define BUCKETS 	64		

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <ctype.h>

struct mnemonic {
	char		*cname;
	struct string	*name;
	void		(*handler)(void);

	unsigned char	inh,		/* (implicit operand) */
			abs,		/* $1234 */
			ax, 		/* $1234,X */
			ay,		/* $1234,Y */
			al,		/* <$123456 */
			alx,		/* <$123456,X */
			ia,		/* ($1234) */
			iax,		/* ($1234,X) */
			ial,		/* [$1234] */
			d,		/* >$12 */
			ds,		/* >$12,S  */
			dx,		/* >$12,X */
			dy,		/* >$12,Y */
			id,		/* (>$12) */
			idl,		/* [>$12] */
			idyl,		/* [>$12],Y */
			idsy,		/* (>$12,S),Y */
			idx,		/* (>$12,X) */
			idy,		/* (>$12),Y */
			imm, 		/* #>$12 */
			i16,		/* #$1234 */
			rel,		/* >$12 (relative) */
			r16;		/* $1234 (relative) */

	struct mnemonic	*link;
};

void brk_handler(void);
void byte_handler(void), word_handler(void), long_handler(void);
void org_handler(void), bss_handler(void), fcs_handler(void);

struct mnemonic mnemonics[] = {
	{ "ADC", .abs=0x6d, .ax=0x7d, .ay=0x79, .al=0x6F, .alx=0x7F, 
		.ia=0x17, .id=0x72, .idl=0x67, .idsy=0x73, .idx=0x61, 
		.idy=0x71, .idyl=0x77, .d = 0x65, .ds=0x63, .dx=0x75, 
		.imm=0x69, .i16=0x69 },
	{ "AND", .abs=0x2d, .ax=0x3e, .ay=0x39, .al=0x2F, .alx=0x3F,
		.id=0x32, .idl=0x27, .idsy=0x33, .idx=0x91, .idy=0x31, 
		.idyl=0x37, .d = 0x25, .ds=0x23, .dx=0x36,
		.imm=0x29, .i16=0x29 },
	{ "ASL", .inh=0x0a, .abs=0x0e, .ax=0x1e, .d=0x06, .dx=0x16 },
	{ "BCC", .rel=0x90 },
	{ "BCS", .rel=0xb0 },
	{ "BEQ", .rel=0xf0 },
	{ "BIT", .abs=0x2c, .ax=0x3c, .d=0x24, .dx=0x34, .imm=0x89, .i16=0x89 },
	{ "BMI", .rel=0x30 },
	{ "BNE", .rel=0xd0 },
	{ "BPL", .rel=0x10 },
	{ "BRA", .rel=0x80, .r16=0x82 },
	{ "BRK", .handler = brk_handler },
	{ "BSS", .handler = bss_handler },
	{ "BVC", .rel=0x50 },
	{ "BVS", .rel=0x70 },
	{ "BYTE", .handler = byte_handler },
	{ "CLC", .inh=0x18 },
	{ "CLD", .inh=0xd8 },
	{ "CLI", .inh=0x58 },
	{ "CLV", .inh=0xb8 },
	{ "CMP", .abs=0xcd, .ax=0xdd, .ay=0xd9, .al=0xcf, .alx=0xdf,
		.id=0xd2, .idl=0xc7, .idsy=0xd3, .idx=0xc1, .idy=0xd1, 
		.idyl=0xd7, .d = 0xc5, .ds=0xc3, .dx=0xd5, .imm=0xc9,
		.i16=0xc9 },
	{ "COP", .inh= 0x02 },
	{ "CPX", .abs=0xec, .d = 0xe4, .imm=0xe0, .i16=0xe0 },
	{ "CPY", .abs=0xcc, .d = 0xc4, .imm=0xc0, .i16=0xc0 },
	{ "DEC", .inh=0x3a, .abs=0xce, .ax=0xde, .d=0xc6, .dx=0xd6 },
	{ "DEX", .inh=0xca },
	{ "DEY", .inh=0x88 },
	{ "EOR", .abs=0x4d, .ax=0x5d, .ay=0x59, .al=0x4f, .alx=0x5f, 
		.id=0x52, .idl=0x47, .idsy=0x53, .idx=0x41, .idy=0x51, 
		.idyl=0x57, .iax=0x5d, .d=0x45, .ds=0x43, .dx=0x55,
		.imm=0x49, .i16=0x49 },
	{ "FCS", .handler = fcs_handler },
	{ "INC", .inh=0x1a, .abs=0xee, .ax=0xfe, .d=0xe6, .dx=0xf6 },
	{ "INX", .inh=0xe8 },
	{ "INY", .inh=0xc8 },
	{ "JMP", .abs=0x4c, .al=0x5c, .ial=0xdc, .ia=0x6c, .iax=0x7c },
	{ "JSR", .abs=0x20, .al=0x22, .iax=0xfc },
	{ "LDA", .abs=0xad, .ax=0xbd, .ay=0xb9, .al=0xaf, .alx=0xbf,
		.id=0xb2, .idl=0xa7, .idsy=0xb3, .idx=0xa1, .idy=0xb1, 
		.idyl=0xb7, .d = 0xa5, .ds=0xa3, .dx=0xb5,
		.imm=0xa9, .i16=0xa9 },
	{ "LDX", .abs=0xae, .ay=0xbe, .d=0xa6, .dy=0xb6, .imm=0xa2, .i16=0xa2 },
	{ "LDY", .abs=0xac, .ax=0xbc, .d=0xa4, .dx=0xb4, .imm=0xa0, .i16=0xa0 },
	{ "LONG", .handler = long_handler },
	{ "LSR", .inh=0x4a, .abs=0x4e, .ax=0x5e, .d=0x46, .dx=0x56 },
	{ "MVN", .abs=0x54 },
	{ "MVP", .abs=0x44 },
	{ "NOP", .inh=0xea },
	{ "ORA", .abs=0x0d, .ax=0x1d, .ay=0x19, .al=0x0f, .alx=0x1f,
		.id=0x12, .idl=0x07, .idsy=0x13, .idx=0x01, .idy=0x11, 
		.idyl=0x17, .d = 0x05, .ds=0x03, .dx=0x15,
		.imm=0x09, .i16=0x09 },
	{ "ORG", .handler = org_handler },
	{ "PEA", .abs=0xf4 },
	{ "PEI", .d = 0xd4 },
	{ "PER", .r16 = 0x62 },
	{ "PHA", .inh=0x48 },
	{ "PHB", .inh=0x8b },
	{ "PHD", .inh=0x0b },
	{ "PHK", .inh=0x4b },
 	{ "PHP", .inh=0x08 },
	{ "PHX", .inh=0xda },
	{ "PHY", .inh=0x5a },
	{ "PLA", .inh=0x68 },
	{ "PLB", .inh=0xab },
	{ "PLD", .inh=0x2b },
	{ "PLP", .inh=0x28 },
	{ "PLX", .inh=0xfa },
	{ "PLY", .inh=0x7a },
	{ "REP", .imm=0xc2 },
	{ "ROL", .inh=0x2a, .abs=0x2e, .ax=0x3e, .d=0x26, .dx=0x36 },
	{ "ROR", .inh=0x6a, .abs=0x6e, .ax=0x7e, .d=0x66, .dx=0x76 },
	{ "RTI", .inh=0x40 },
	{ "RTL", .inh=0x6b },
	{ "RTS", .inh=0x60 },
	{ "SBC", .abs=0xed, .ax=0xfd, .ay=0xf9, .alx=0xff, .id=0xf2, 
		.idl=0xe7, .idsy=0xf3, .idx=0xe1, .idy=0xf1, .idyl=0xf7,
		.d=0xe5, .ds=0xe3, .dx=0xf5, .imm=0xe9, .i16=0xe9 },
	{ "SEC", .inh=0x38 },
	{ "SED", .inh=0xf8 },
	{ "SEI", .inh=0x78 },
	{ "SEP", .imm=0xe2 },
	{ "STA", .abs=0x8d, .ax=0x9d, .al=0x8f, .alx=0x9f, .id=0x92, 
		.idl=0x87, .idsy=0x93, .idx=0x81, .idy=0x91, .idyl=0x97,
		.d=0x85, .ds=0x83, .dx=0x95 },
	{ "STP", .inh=0xdb },
	{ "STX", .abs=0x8e, .d=0x86, .dy=0x96 },
	{ "STY", .abs=0x8c, .d=0x84, .dx=0x94 },
	{ "STZ", .abs=0x9c, .ax=0x9e, .d=0x64, .dx=0x74 },
	{ "TAX", .inh=0xaa },
	{ "TAY", .inh=0xa8 },
	{ "TCD", .inh=0x5b },
	{ "TCS", .inh=0x1b },
	{ "TDC", .inh=0x7b },
	{ "TRB", .abs=0x1c, .d=0x14 },
	{ "TSB", .abs=0x0c, .d=0x04 },
	{ "TSC", .inh=0x3b },
	{ "TSX", .inh=0xba },
	{ "TXA", .inh=0x8a },
	{ "TXS", .inh=0x9a },
	{ "TXY", .inh=0x9b },
	{ "TYA", .inh=0x98 },
	{ "TYX", .inh=0xbb },
	{ "WAI", .inh=0xcb },
	{ "WDM", .inh=0x42 },
	{ "WORD", .handler = word_handler },
	{ "XBA", .inh=0xeb },
	{ "XCE", .inh=0xfb }
};

#define NR_MNEMONICS (sizeof(mnemonics) / sizeof(struct mnemonic))

struct string {
	unsigned	hash;
	int		length;
	char 		*s;
	struct string 	*link;
};

#define SYMBOL_DEFINED 	0x00000001	/* symbol has been resolved */
#define SYMBOL_CONSTANT	0x00000002	/* it's an equate, not a label */

struct symbol {
	int		flags, value;
	struct string	*name;
	struct symbol 	*link;
};

struct mnemonic *mnemonic_bucket[BUCKETS];

struct string *string_bucket[BUCKETS];

struct symbol *symbol_bucket[BUCKETS];
struct symbol *localf[10], *localb[10], *locals, *next_local;

char 	*inpath, *outpath, *listpath;
FILE	*infp, *outfp, *listfp;
int	out_base = -1, out_size;
int 	lineno, pass2, next_arg, pc, linepc, emitted;
char	line[LINE_SIZE], *curp, **args;

#define xvalue(x) (isdigit(x) ? ((x) - '0') : (10 + ((x) - 'A')))
#define isbdigit(x) (((x) == '0') || ((x) == '1'))
#define isletter(x) (isalpha(x) || ((x) == '_'))

void error(const char *format, ...)
{
	va_list args;

	va_start(args, format);
	if (inpath) {
		fprintf(stderr, "%s", inpath);
		if (lineno) fprintf(stderr, " (%d)", lineno);
		fprintf(stderr, ": ");
	}

	fprintf(stderr, "error: ");
	vfprintf(stderr, format, args);

	va_end(args);

	fputc('\n', stderr);

	if (outfp) { fclose(outfp); unlink(outpath); }
	if (listfp) { fclose(listfp); unlink(listpath); }

	exit(1);
}

void emit(int sz, int v)
{
	static int curpos = -1;

	while (sz--) {
		if (pass2) {
			int pos = pc - out_base;
	
			if ((pos < 0) || (out_size && (pos >= out_size)))
				error("output address %06X not in output file",
					pc);

			if (curpos != pos) {
				if (fseek(outfp, pos, SEEK_SET) == -1) 
					error("fseek: %s", strerror(errno));
				curpos = pos;
			}

			fputc(v & 0xFF, outfp);
			curpos++;
		} 

		emitted++;
		pc++;
		v >>= 8; 
	}
}

void *alloc(int bytes)
{
	void *p = malloc(bytes);

	if (p == NULL) error("out of memory");
	memset(p, 0, bytes);

	return p;
}

int next_line(void)
{
	for (;;) {
		if (infp == NULL) {
			if (!args[next_arg]) return -1;

			inpath = args[next_arg++];
			lineno = 0;
	
			infp = fopen(inpath, "r");
			if (!infp) error("%s", strerror(errno));

			if (listfp && pass2) 
				fprintf(listfp, "\n%s\n\n", inpath);
		}

		lineno++;
		if (!fgets(line, LINE_SIZE, infp)) {
			if (feof(infp)) {
				fclose(infp);
				infp = NULL;
				continue;
			}

			error("%s", strerror(errno));
		}

		if (!(curp = strchr(line, '\n')))
			error("line too long (max %d characters)", LINE_SIZE);

		*curp = 0;
		for (curp = line; *curp && (*curp != ';'); curp++) 
			*curp = toupper(*curp);

		curp = line;
		return 0;
	}
}

void listbytes(void)
{
	int i = 0;
	int c;
	
	for (i = 0; (i < 5) && emitted; i++, emitted--) {
		c = getc(outfp);
		fprintf(listfp, "%02X ", c);
	}

	while (i < 5) { fprintf(listfp, "   "); i++; }
}

void list(void)
{
	long pos;
	int i;

	pos = ftell(outfp);
	fseek(outfp, -emitted, SEEK_CUR);

	fprintf(listfp, "  %04d %06X  ", lineno, linepc);
	listbytes();
	fprintf(listfp, "  %s\n", line);

	while (emitted) {
		fprintf(listfp, "               ");
		listbytes();
		fputc('\n', listfp);
	}
	
	fseek(outfp, pos, SEEK_SET);
}

void summary(void)
{
	struct symbol 	*symp;
	int 		i;

	fprintf(listfp, "\n*** SYMBOLS ***\n\n");
	
	for (i = 0; i < BUCKETS; i++) 
		for (symp = symbol_bucket[i]; symp; symp = symp->link)
			fprintf(listfp, "  %06X %s\n", 
				symp->value, symp->name->s);

	fputc('\n', listfp);
}

void expect(int ch) { if (*curp != ch) error("'%c' expected", ch); }
void match(int ch) { expect(ch); curp++; }

struct symbol *get_local()
{
	struct symbol *symp;

	if (pass2) {
		symp = next_local;
		next_local = symp->link;
		if (!(symp->flags & SYMBOL_DEFINED)) 
			error("unresolved local forward");
	} else {
		symp = alloc(sizeof(struct symbol));
		if (locals) {
			next_local->link = symp;
			next_local = symp;
		} else	{
			locals = next_local = symp;
		}
	}

	return symp;
}

struct string *string(char *s, int length, int dup)
{
	unsigned	hash, i, bucket;
	struct string 	*strp;
	
	for (i = hash = 0; i < length; i++) {
		hash <<= 3;
		hash |= s[i] & 0x07;
	}

	bucket = hash % BUCKETS;
	for (strp = string_bucket[bucket]; strp; strp = strp->link) {
		if (strp->length == length) 
			if (strp->hash == hash) 
				if (!strncmp(strp->s, s, length)) 
					return strp;
	}

	strp = alloc(sizeof(struct string));

	strp->length = length;
	strp->hash = hash;
	if (dup) {
		strp->s = alloc(length + 1);
		strncpy(strp->s, s, length);
	} else	strp->s = s;

	strp->link = string_bucket[bucket];
	string_bucket[bucket] = strp;
	
	return strp;
}

struct mnemonic *get_mnemonic(struct string *strp)
{
	struct mnemonic	*mnep;
	int 		bucket = strp->hash % BUCKETS;

	for (mnep = mnemonic_bucket[bucket]; mnep; mnep = mnep->link)
		if (mnep->name == strp) return mnep;

	return NULL;
}

struct string *get_name(void)
{
	char *startp;

	startp = curp;

	while (isletter(*curp) || isdigit(*curp)) curp++;

	return string(startp, curp - startp, 1);
}

struct symbol *get_symbol(struct string *name)
{
	int 		bucket = name->hash % BUCKETS;
	struct symbol	*symp;

	for (symp = symbol_bucket[bucket]; symp; symp = symp->link) 
		if (symp->name == name) {
			if (pass2 && !(symp->flags & SYMBOL_DEFINED))
				error("%s unresolved", name->s);

			return symp;
		}

	symp = alloc(sizeof(struct symbol));
	symp->name = name;
	symp->link = symbol_bucket[bucket];
	symbol_bucket[bucket] = symp;

	return symp;
}


int get_factor(int mustdef)
{
	struct symbol *symp;
	struct string *strp;
	int value = 0;
	int comp = 0;

	mustdef += pass2;

	if (*curp == '~') {
		curp++;
		comp++;
	}

	if (*curp == '.') {
		curp++;
		value = pc;
	} else if (*curp == '$') {
		curp++;
		if (!isxdigit(*curp)) error("malformed hex value");
		while (isxdigit(*curp)) {
			value <<= 4;
			value += xvalue(*curp);
			curp++;
		}
	} else if (*curp == '%') {
		curp++;
		if (!isbdigit(*curp)) error("malformed binary value");
		while (isbdigit(*curp)) {
			value <<= 1;
			value += xvalue(*curp);
			curp++;
		}
	} else if (isdigit(*curp)) {
		value = *curp - '0';
		curp++;
		if (*curp == 'B') {
			curp++;
			symp = localb[value];
			if (!symp) error("unresolved local (backward)");
			value = symp->value;
		} else if (*curp == 'F') {
			curp++;
			symp = localf[value];
			if (!symp) localf[value] = symp = get_local();

			value = symp->value;
			if (mustdef && !(symp->flags & SYMBOL_DEFINED))
				error("unresolved local (forward)");
		} else while (isdigit(*curp)) {
			value *= 10;
			value += *curp - '0';
			curp++;
		}
	} else if (isletter(*curp)) {
		strp = get_name();
		symp = get_symbol(strp);
		value = symp->value;
		if (mustdef && !(symp->flags & SYMBOL_DEFINED))
			error("%s unresolved", symp->name->s);
	} else error("value expected");

	if (comp) value = ~value;
	return value;
}

int get_term(int mustdef)
{
	int factor, value = 1;
	int mult = 1;

	for (;;) {
		factor = get_factor(mustdef);
		if (mult) 
			value *= factor;
		else	value /= factor;

		if (*curp == '*') {
			mult = 1;
		} else if (*curp == '/') {
			mult = 0;
		} else return value;

		curp++;
	} 
}

int get_value(int mustdef)
{
	int term, sign, value = 0;

	do {
		if (*curp == '-') { sign = -1; curp++; }
		else {
			sign = 1;
			if (*curp == '+') curp++;
		}
		term = get_term(mustdef);
		term *= sign;
		value += term;
	} while((*curp == '-') || (*curp == '+'));

	return value;
}

void set_value(struct symbol *symp, int value)
{
	if (!pass2 && (symp->flags & SYMBOL_DEFINED)) 
		error("%s redefined", symp->name->s);

	symp->flags |= SYMBOL_DEFINED;
	symp->value = value;
}

void def(void)
{
	int i;

	if (isdigit(*curp)) {
		i = *curp - '0';

		curp++;
		match(':');

		if (localf[i]) {
			localb[i] = localf[i];
			localf[i] = NULL;
		} else 
			localb[i] = get_local();
		
		set_value(localb[i], pc);
	} else if (isletter(*curp)) {
		struct string *name = get_name();
		struct symbol *symp = get_symbol(name);

		if (*curp == ':') {
			match(':');
			set_value(symp, pc);
		} else {
			match('=');
			set_value(symp, get_value(1));
		}
	} else error("symbol expected");
}

void brk_handler(void) { emit(1, 0); }

void con(int sz)
{
	for (;;) {
		emit(sz, get_value(0));

		if (*curp != ',') break;
		curp++;
	}	
}

void byte_handler(void) { con(1); }
void word_handler(void) { con(2); }
void long_handler(void) { con(3); }

void bss_handler(void) { pc += get_value(1); }

void fcs_handler(void) 
{
	int i = *curp;
	char *start;

	if (!isprint(i)) error("missing string");

	start = ++curp;
	while (*curp && (*curp != i)) curp++;
	if (!*curp) error("missing closing delimiter");

	while (start < curp) {
		i = *start++;
		if (start == curp) i |= 0x80;
		emit(1, i);
	}

	curp++;
}

void org_handler(void) { 
	int value = get_value(1); 

	if (value < pc) error("origin moves backwards");
	pc = value;
}

void insn(int opcode, int operand_sz, int operand)
{
	if (opcode == 0) error("illegal address mode");
	emit(1, opcode);
	emit(operand_sz, operand);
}

void assemble(struct mnemonic *mnep)
{
	int value;
	int i;

	if (!*curp || (*curp == ';')) 
		insn(mnep->inh, 0, 0);
	else if (*curp == '#') {
		curp++;
		if (*curp == '>') {
			curp++;
			insn(mnep->imm, 1, get_value(0));
		} else	insn(mnep->i16, 2, get_value(0));
	} else if (*curp == '>') {
		curp++;
		value = get_value(0);
		if (mnep->rel) {
			value = value - (pc + 2);

			if (pass2 && ((value < -128) || (value > 127)))
				error("target out of range");

			insn(mnep->rel, 1, value);
		} else {
			i = mnep->d;
		
			if (*curp == ',') {
				curp++;
				switch (*curp) {
					case 'S': i = mnep->ds; break;
					case 'X': i = mnep->dx; break;
					default: 
						match('Y');
						i = mnep->dy; 
						break;
				}
				curp++;
			} 

			insn(i, 1, value);
		}
	} else if (*curp == '[') {
		curp++;
		if (*curp == '>') {
			curp++;
			i = mnep->idl;

			value = get_value(0);
			match(']');
			if (*curp == ',') {
				curp++;
				match('Y');
				i = mnep->idyl;
			}

			insn(i, 1, value);
		} else {
			insn(mnep->ial, 2, get_value(0));
			match(']');
		}
	} else if (*curp == '(') {
		curp++;
		if (*curp == '>') {
			curp++;
			value = get_value(0);
			
			if (*curp == ')') {
				curp++;
				if (*curp == ',') {
					curp++;
					match('Y');
					insn(mnep->idy, 1, value);
				} else 	insn(mnep->id, 1, value);
			} else {
				match(',');
				if (*curp == 'X') {
					curp++;
					match(')');
					insn(mnep->idx, 1, value);
				} else {
					match('S');
					match(')');
					match(',');
					match('Y');
					insn(mnep->idsy, 1, value);
				}
			}
		} else {
			value = get_value(0);

			if (*curp == ',') {
				curp++;
				match('X');
				match(')');
				insn(mnep->iax, 2, value);
			} else {
				match(')');
				insn(mnep->ia, 2, value);
			}
		}
	} else if (*curp == '<') {
		curp++;
		value = get_value(0);
		i = mnep->al;

		if (*curp == ',') {
			curp++;
			match('X');
			i = mnep->alx;
		}
		insn(i, 3, value);
	} else {
		value = get_value(0);
		if (*curp == ',') {
			curp++;

			if (*curp == 'X') {
				curp++;
				insn(mnep->ax, 2, value);
			} else {
				match('Y');
				insn(mnep->ay, 2, value);
			} 
		} else {
			if (mnep->r16) {
				value = value - (pc + 3);
				if ((value < -32768) || (value > 32767))
					if (pass2) 
						error("target out of range");

				insn(mnep->r16, 2, value);
			} else 	insn(mnep->abs, 2, value);
		}
	}
}

void pass(void)
{
	struct mnemonic	*mnep;
	struct string	*strp;
	int 		i;

	next_arg = 0;
	pc = 0;
	next_local = locals;
	for (i = 0; i < 10; i++) localf[i] = localb[i] = NULL;

	while (next_line() != -1) {
		linepc = pc;
		emitted = 0;

		if (*curp && !isspace(*curp) && (*curp != ';')) def();
		while (isspace(*curp)) curp++;
		if (*curp && (*curp != ';')) {
			if (!isalpha(*curp)) error("mnemonic expected");
			strp = get_name();
			mnep = get_mnemonic(strp);
			if (!mnep) error("unrecognized mnemonic %s", strp->s);
			while (isspace(*curp)) curp++;

			if (mnep->handler) 
				mnep->handler();
			else	assemble(mnep);

			while (isspace(*curp)) curp++;
			if (*curp && (*curp != ';')) error("trailing garbage");
		}
			
		if (pass2 && listfp) list();
	}

	if (pass2 && listfp) summary();
}

void syntax(void)
{
	fprintf(stderr, 
		"syntax: asm [-o outpath] [-l listpath] { source... }\n"
		"outpath and at least one source path must be specified\n");
	exit(1);
}

int narg(const char *desc, char *arg, int min, int max)
{
	long l;
	char *cp;

	errno = 0;
	l = strtol(arg, &cp, 0);
	if ((cp == optarg) || errno) error("invalid %s", desc);
	if ((l < min) || (l > max)) error("value out of range");

	return l;
}

int main(int argc, char **argv)
{
	struct mnemonic *mnep;
	struct symbol 	*symp;
	int		i;

	while ((i = getopt(argc, argv, "l:o:b:s:")) != -1) {
		switch (i) {
			case 'b':
				if (out_base != -1) syntax();
				out_base = narg("base address", optarg, 
					0, 0xFFFFFF);
				break;

			case 's':
				if (out_size) syntax();
				out_size = narg("size", optarg, 0, 0xFFFFFF);
				break;
				
			case 'o':
				if (outpath) syntax();
				outpath = optarg;
				break;

			case 'l':
				if (listpath) syntax();
				listpath = optarg;
				break;

			case '?':
				exit(1);
		}
	}

	if (!argv[optind] || !outpath) syntax();
	if (out_base < 0) out_base = 0;

	args = &(argv[optind]);

	for (i = 0, mnep = mnemonics; i < NR_MNEMONICS; i++, mnep++) {
		int bucket;

		mnep->name = string(mnep->cname, strlen(mnep->cname), 0);
		bucket = mnep->name->hash % BUCKETS;
		mnep->link = mnemonic_bucket[bucket];
		mnemonic_bucket[bucket] = mnep;
	}

	pass();
	
	pass2++;
	outfp = fopen(outpath, "w+b");
	if (outfp == NULL) 
		error("could not open %s: %s", outpath, strerror(errno));
	if (out_size) {
		pc = out_base;
		for (i = 0; i < out_size; i++) emit(1, 0xFF);
	}

	if (listpath) {
		listfp = fopen(listpath, "w");
		if (listfp == NULL) 
			error("could not open %s: %s",
				listpath,
				strerror(errno));
	}
	pass();

	fclose(outfp);
	if (listfp) fclose(listfp);

	return 0;
}
