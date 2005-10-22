/*
 * Signal catcher
 *	++Jam
 *
 * add -Dsolaris to Makefile if compiling at Solaris 2.x
 * add -lelf to libs in Makefile
 */

/* Bat (ldmud) includes */
#include "xalloc.h"
extern void error(char *text);
#define malloc(xxx)	xalloc(xxx)
#define free(xxx)	xfree(xxx)


#define MAX_FAULTS 1000

#include <stdio.h>
#include <signal.h>

#ifdef sparc
#ifdef solaris
#include <siginfo.h>
#include <ucontext.h>

#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/signal.h>
#include <sys/fault.h>
#include <sys/syscall.h>
#include <sys/procfs.h>

#include <libelf.h>
#include <sys/elf_SPARC.h>
#else
#include <frame.h>
#endif
#elif defined(linux)

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <elf.h>
#ifndef __USE_GNU
#define __USE_GNU
#include <ucontext.h>
#undef __USE_GNU
#else
#include <ucontext.h>
#endif

#endif

#include <string.h>
#include "frame.h"

#define INNER_THAN	<	/* Stack grows downward.  */

static int signal_error_count = 0;

typedef struct _symlist_t {
  char *addr;
  struct _symlist_t *next;
  char lib[16];
  char name[1];
} symlist_t;

static symlist_t *s_head = NULL;

static void insert_func(char *addr, char *name, char *lib)
{
  symlist_t **slpp, *new;

  /*  fprintf(stderr, "0x%08x: %s\n", addr, name); */

  new = (symlist_t *)malloc(sizeof(symlist_t) + strlen(name));
  new->addr = addr;
  strcpy(new->name, name);
  if (lib) {
    strncpy(new->lib, lib, 15);
    new->lib[15] = '\0';
  } else {
    new->lib[0] = '\0';
  }

  if (!s_head) {
    new->next = NULL;
    s_head = new;
    return;
  }

  slpp = &s_head;
  while (*slpp && (*slpp)->addr > addr)
    slpp = &((*slpp)->next);

  new->next = *slpp;
  *slpp = new;
}

static char *addr_to_name(char *addr, int *offset, char **lib)
{
  symlist_t *slp;
  
  for (slp=s_head; slp; slp=slp->next) {
    if ((slp->addr < addr) &&
	(addr - slp->addr < 0x100000)) {
      if (offset) {
	*offset = addr - slp->addr;
      }
      if (lib) {
	if (slp->lib[0])
	  *lib = slp->lib;
	else
	  *lib = NULL;
      }
      return slp->name;
    }
  }

  if (offset) {
    /*    *offset = (int)addr; */
    *offset = (int)0;
  }
  if (lib)
    *lib = NULL;
  return "???";
}

#ifdef solaris

void init_signal(int sig)
{
  /* Init signal-handler */
    sigemptyset(&act.sa_mask);
#if 0                           /* not working 2.3 */
    ss.ss_sp =
      (char *)((((int)reserved_area + RESERVED_SIZE - 500) >> 4) << 4);
    ss.ss_size = (ss.ss_sp - reserved_area ) & (~0xf);
    ss.ss_flags =0;
    (void)sigaltstack(&ss, NULL);
    act.sa_flags =SA_ONSTACK|SA_SIGINFO;
#else
    act.sa_flags =SA_SIGINFO;
#endif
    act.sa_handler =bug_handler;
    sigemptyset(&act.sa_mask);
    (void)sigaction(SIGSEGV, &act, NULL);
    (void)sigaction(SIGBUS, &act, NULL);
    (void)sigaction(SIGFPE, &act, NULL);
    act.sa_handler =dump_stack_frame;
    act.sa_flags =SA_SIGINFO|SA_RESTART;
    (void)sigaction(SIGXCPU, &act, NULL);
}

void init_bug_handler()
{
  int procfd, memfd;
  char procpath[20];
  Elf *arf, *elf;
  Elf_Cmd cmd;

  switch (1) {
  default:
    sprintf(procpath, "/proc/%i", getpid());
    if ((procfd=open(procpath, O_RDONLY)) == -1) {
      fprintf(stderr, "Tried: '%s'\n", procpath);
      perror("open proc");
      break;
    }
    memfd = ioctl(procfd, PIOCOPENM, NULL);
    if (memfd == -1) {
      perror("ioctl PIOCOPENM");
      close(procfd);
      break;
    }
    close(procfd);
    if (elf_version(EV_CURRENT) == EV_NONE) {
      /* library out of date */
      /* recover from error */
      fprintf(stderr, "Wrong version of elf-library!\n");
      close(memfd);
      break;
    }
    cmd = ELF_C_READ;
    arf = elf_begin(memfd, cmd, (Elf *)0);
    while ((elf = elf_begin(memfd, cmd, arf)) != 0)
      {
	Elf32_Ehdr *ehdr;
	Elf_Scn *scn;
	
	/* process the file ... */
	if ((ehdr = elf32_getehdr(elf)) != NULL) {
	  ;
	}
	scn = NULL;
	while ((scn = elf_nextscn(elf, scn)) != 0) {
	  Elf32_Shdr *shdr;
	  char *cp, *cp2, *name;
	  
	  /* process section */
	  shdr = elf32_getshdr(scn);
	  if (shdr->sh_type == SHT_SYMTAB) {
	    Elf_Data *edata;
	    
	    edata = NULL;
	    while ((edata=elf_getdata(scn,edata)) != NULL) {
	      Elf32_Sym *S;
	      int count;
	      
	      if (edata->d_buf == NULL || edata->d_size == 0)
		continue;
	      if (edata->d_type == ELF_T_SYM)
		for (count=0; count<edata->d_size; count+=sizeof(Elf32_Sym)) {
		  
		  /* handle symbol */
		  S = (Elf32_Sym *)((char *)edata->d_buf + count);
		  
/* Kludge Alert: 22 is what we need. But where it comes?? ++Jam */
		  name = elf_strptr(elf, 22, (size_t)S->st_name);
/* Kludge Alert end */
		  
		  if (ELF32_ST_TYPE(S->st_info) == STT_FUNC && name)
		    insert_func((char *)(S->st_value), name, NULL);
		  
		}	/* for syms */
	    } /* while sym data */
	    
	  } /* if symtab */
	}	/* while sections */
	
	cmd = elf_next(elf);
	elf_end(elf);
      } /* while elf */
    
#if 0
    elf_end(arf);
    close(memfd);
#endif
  } /* switch 1 */

  /* Then some specific code to prepare? */

  return;
}

static void print_fpu_status(status)
     FPU_FSR_TYPE status;
{
  char *text;

  fprintf(stderr, "FPU Status (0x%08x):\n", status);

  fprintf(stderr, " Current Exception (%x):", status&FSR_CEXC);
  if (status & FSR_CEXC_NX)
    fprintf(stderr, " 'inexact'");
  if (status & FSR_CEXC_DZ)
    fprintf(stderr, " 'divide-by-zero'");
  if (status & FSR_CEXC_UF)
    fprintf(stderr, " 'underflow'");
  if (status & FSR_CEXC_OF)
    fprintf(stderr, " 'overflow'");
  if (status & FSR_CEXC_NV)
    fprintf(stderr, " 'invalid'");
  fprintf(stderr, "\n");

  fprintf(stderr, " Accrued Exception (%x):", status&FSR_AEXC);
  if (status & FSR_AEXC_NX)
    fprintf(stderr, " 'inexact'");
  if (status & FSR_AEXC_DZ)
    fprintf(stderr, " 'divide-by-zero'");
  if (status & FSR_AEXC_UF)
    fprintf(stderr, " 'underflow'");
  if (status & FSR_AEXC_OF)
    fprintf(stderr, " 'overflow'");
  if (status & FSR_AEXC_NV)
    fprintf(stderr, " 'invalid'");
  fprintf(stderr, "\n");
  
  fprintf(stderr, " Floating-point Condition Codes: 0x%x\n", status&FSR_FCC);
  fprintf(stderr, " Partial Remainder: 0x%x\n", status&FSR_PR);
  fprintf(stderr, " Queue not empty: 0x%x\n", status&FSR_QNE);

  switch (status & FSR_FTT) {
  case FTT_NONE:
    text = "no exceptions";
    break;
  case FSR_FTT_IEEE:
    text = "IEEE exception";
    break;
  case FSR_FTT_UNFIN:
    text = "unfinished fpop";
    break;
  case FSR_FTT_UNIMP:
    text = "unimplemented fpop";
    break;
  case FSR_FTT_SEQ:
    text = "sequence error";
    break;
  case FSR_FTT_ALIGN:
    text = "alignment, by software convention";
    break;
  case FSR_FTT_DFAULT:
    text = "data fault, by software convention";
    break;
  default:
    text = "<unknown>";
    break;
  }
  fprintf(stderr, " Floating-point Trap Type (%x): %s\n",status&FSR_FTT, text);

  fprintf(stderr, " Trap Enable Mask (%x):", status&FSR_TEM);
  if (status & FSR_TEM_NX)
    fprintf(stderr, " 'inexact'");
  if (status & FSR_TEM_DZ)
    fprintf(stderr, " 'divide-by-zero'");
  if (status & FSR_TEM_UF)
    fprintf(stderr, " 'underflow'");
  if (status & FSR_TEM_OF)
    fprintf(stderr, " 'overflow'");
  if (status & FSR_TEM_NV)
    fprintf(stderr, " 'invalid'");
  fprintf(stderr, "\n");

  switch ((status & FSR_RP) >> 28) {
  case RP_DBLEXT:
    text = "double-extended";
    break;
  case RP_SINGLE:
    text = "single";
    break;
  case RP_DOUBLE:
    text = "double";
    break;
  case RP_RESERVED:
    text = "unused and reserved";
    break;
  default:
    text = "<unknown>";
    break;
  }
  fprintf(stderr, " Rounding Precision (%x): %s\n", status&FSR_RP, text);

  switch ((status & FSR_RD) >> 30) {
  case RD_NEAR:
    text = "nearest or even if tie";
    break;
  case RD_ZER0:
    text = "to zero";
    break;
  case RD_POSINF:
    text = "positive infinity";
    break;
  case RD_NEGINF:
    text = "negative infinity";
    break;
  default:
    text = "<unknown>";
    break;
  }
  fprintf(stderr, " Rounding Direction (%x): %s\n", status&FSR_RD, text);
}

void dump_stack_frame(sig, sip, ucp)
     int sig;
     siginfo_t *sip;
     ucontext_t *ucp;
{
  int code;
  struct rwindow *sregs;
  char *name;
  int offset;

  if (sip) {
    code =sip->si_code;
    fprintf(stderr, "Signal %d code %d address 0x%08x\n",
	   sig, code, (unsigned long)sip->si_addr);
  } else {
    code = -1;
    fprintf(stderr, "Signal %d\n", sig);
  }

  switch(sig) {
  case SIGSEGV: 
    fprintf(stderr, "Segmentation fault: ");
    switch(code) {
    case SEGV_MAPERR:
      fprintf(stderr, "Address not mapped to object.\n");
      break;
    case SEGV_ACCERR:
      fprintf(stderr, "Invalid permissions for mapped object.\n");
      break;
    default:
      fprintf(stderr, "Unknown reason.\n");
      break;
    }
    break;
  case SIGBUS:
    fprintf(stderr, "Bus error: ");
    switch(code) {
    case BUS_ADRALN:
      fprintf(stderr, "Invalid address alignment.\n");
      break;
    case BUS_ADRERR:
      fprintf(stderr, "non-existent physical address.\n");
      break;
    case BUS_OBJERR:
      fprintf(stderr, "Object specific hardware error.\n");
      break;
    default:
      fprintf(stderr, "Unknown reason.\n");
      break;
    }
    break;
 case SIGFPE:
    fprintf(stderr, "Arithmetic Exception (FPE).\n");
    print_fpu_status(ucp->uc_mcontext.fpregs.fpu_fsr);
    break;
 case SIGPROF:
    fprintf(stderr, "PROFiling signal.\n");
    break;
  default:
    break;
  }

  sregs = (struct rwindow *)ucp->uc_mcontext.gregs[REG_SP];
  fprintf(stderr, "This user context address: %08x\n", ucp);
  fprintf(stderr, "sp:%08x fp:%08x top:%08x\n",
          ucp->uc_mcontext.gregs[REG_SP],
          sregs->rw_fp,
	  ucp->uc_stack.ss_sp);
  name = addr_to_name((char *)ucp->uc_mcontext.gregs[REG_PC], &offset, NULL);
  fprintf(stderr, "pc: %s()+%x (%08x) (pc?:%08x) (flags:%s%s%s%s%s)\n",
	  name, offset,
          ucp->uc_mcontext.gregs[REG_PC],
	  ucp->uc_mcontext.gregs[REG_O7],
	  ucp->uc_flags & UC_SIGMASK ? ucp->uc_flags & UC_SIGMASK ?
	  " Sigmask":"" : " None",
	  ucp->uc_flags & UC_STACK ? " Stack":"",
	  ucp->uc_flags & UC_CPU   ? " Cpu":"",
	  ucp->uc_flags & UC_MAU   ? " Mau":"",
	  ucp->uc_flags & UC_FPU   ? " Fpu":"",
	  ucp->uc_flags & UC_INTR  ? " Intr":"");
  if (ucp->uc_mcontext.gwins) {
    gwindows_t *gwins = ucp->uc_mcontext.gwins;
    int i;
    fprintf(stderr, "Register windows (%2x) are in save area.\n",
            gwins->wbcnt);
    for (i=0; i<gwins->wbcnt && i<SPARC_MAXREGWINDOW; i++)
      fprintf(stderr, "%2x: sp:%08x fp:%08x ret:%08x\n",
               gwins->wbcnt - i, gwins->spbuf[i],
               gwins->wbuf[i].rw_fp, gwins->wbuf[i].rw_rtn);
  }

  while (((char *)sregs->rw_fp) &&
	 (((char *)sregs->rw_fp) INNER_THAN
	  ((char *)(ucp->uc_stack.ss_sp) + ucp->uc_stack.ss_size)) &&
	 ((char *)(ucp->uc_stack.ss_sp) INNER_THAN ((char *)sregs->rw_fp))) {
    name = addr_to_name((char *)sregs->rw_rtn, &offset, NULL);
    fprintf(stderr, "rtn: %s()+%x (%08x) (args: %08x %08x %08x %08x %08x)\n",
	    name, offset, sregs->rw_rtn,
	    sregs->rw_in[0], sregs->rw_in[1], sregs->rw_in[2],
	    sregs->rw_in[3], sregs->rw_in[4]);
    sregs = (struct rwindow *)sregs->rw_fp;
  } /* while fp */

  if (ucp->uc_link) {
    ucontext_t *tmpucp;
    int count=0;

    fprintf(stderr, "There is user context link to %08x.\n", ucp->uc_link);
    for (tmpucp=ucp->uc_link; tmpucp; tmpucp=tmpucp->uc_link) {
      if (tmpucp INNER_THAN ucp ||
	  (tmpucp->uc_link && !(tmpucp INNER_THAN tmpucp->uc_link))) {
	fprintf(stderr, "Incorrect ucontext in link: %08x\n", tmpucp);
	break;
      }
      count++;
    }
    fprintf(stderr, "Total of ucontexts: %i\n", count);
    if (count > 10)
      count = 10;
    for (tmpucp=ucp->uc_link; tmpucp&&count>0; tmpucp=tmpucp->uc_link,count--)
      fprintf(stderr, "Ucontext at %08x (pc:%08x).\n",
	      tmpucp, tmpucp->uc_mcontext.gregs[REG_PC]);
  }
 
  fflush(stdout);
  fflush(stderr);
}

void bug_handler(sig, sip, ucp)
     int sig;
     siginfo_t *sip;
     ucontext_t *ucp;
{
   if (signal_error_count++ > 1000)
     exit(42);
   dump_stack_frame(sig, sip, ucp);
   error("System error\n");
}


#elif defined(linux)

static void load_elf(char *fpath, unsigned base, char *lib)
{
  Elf32_Ehdr ehdr;
  Elf32_Shdr shdr;
  Elf32_Shdr stab;
  Elf32_Off shstr = 0;
  char *strtab = NULL;
  Elf32_Sym *symbols;
  char buff[1025];
  char *name;
  int count;
  int procfd;

  if ((procfd=open(fpath, O_RDONLY)) == -1) {
    fprintf(stderr, "Tried: '%s'\n", fpath);
    perror("open");
    return;
  }

  /*
   * Prog:
   * [Nr] Name      Type    Addr     Off    Size   ES Flg Lk Inf Al
   * [28] .symtab   SYMTAB  00000000 009b1c 000680 10     29  40  4
   * [29] .strtab   STRTAB  00000000 00a19c 0003ca 00      0   0  1
   * .symtab:
   * Num:    Value  Size Type    Bind   Vis      Ndx Name
   *  98: 08049044    65 FUNC    GLOBAL DEFAULT   12 do_something
   *
   * Lib:
   * [Nr] Name      Type    Addr     Off    Size   ES Flg Lk Inf Al
   * [ 2] .dynsym   DYNSYM  000003ec 0003ec 000730 10   A  3  19  4
   * [ 3] .dynstr   STRTAB  00000b1c 000b1c 000419 00   A  0   0  1
   * [21] .shstrtab STRTAB  00000000 00c398 0000b5 00      0   0  1
   * .dynsym:
   * Num:    Value  Size Type    Bind   Vis      Ndx Name
   *  90: 000050c4    37 FUNC    GLOBAL DEFAULT   10 elf64_getehdr
   */

  if (read(procfd, &ehdr, sizeof(ehdr)) != sizeof(ehdr) ||
      strncmp(ehdr.e_ident, ELFMAG, SELFMAG) != 0) {
    fprintf (stderr, "ehdr failed\n");
    close(procfd);
    return;
  }

#define READ_SHDR(xxx)	\
    lseek(procfd, ehdr.e_shoff + xxx * sizeof(shdr), SEEK_SET),\
    read(procfd, &shdr, sizeof(shdr))

  /* Find header strings */
  READ_SHDR(ehdr.e_shstrndx);
  shstr = shdr.sh_offset;

  /* Find needed sections */
  count = 0;
  READ_SHDR(count);
  while (count < ehdr.e_shnum) {
    if (shdr.sh_name) {
      lseek(procfd, shstr + shdr.sh_name, SEEK_SET);
      read(procfd, &buff, sizeof(buff));
      /*      printf ("[%d] %d, %s\n", count, shdr.sh_name, buff); */
    }

    if (count != ehdr.e_shstrndx) {
      switch (shdr.sh_type) {
      case SHT_DYNSYM:
      case SHT_SYMTAB:		/* use lastone */
	stab = shdr;
	/*	fprintf(stderr, " section size: %d\n", shdr.sh_size); */
	break;
      case SHT_STRTAB:
	strtab = (char *)malloc(shdr.sh_size);
	lseek(procfd, shdr.sh_offset, SEEK_SET);
	read(procfd, strtab, shdr.sh_size);
	/*	fprintf(stderr, " str size: %d\n", shdr.sh_size); */
	break;
      default:
	break;
      } /* switch type */
    }

    count++;
    READ_SHDR(count);
  }
  
  symbols = (Elf32_Sym *)malloc(stab.sh_size);
  lseek(procfd, stab.sh_offset, SEEK_SET);
  read(procfd, symbols, stab.sh_size);
  for (count = 0; count * sizeof(Elf32_Sym) < stab.sh_size; count++) {
    /* handle symbol */
    if (ELF32_ST_TYPE(symbols[count].st_info) == STT_FUNC &&
	symbols[count].st_shndx > 0 &&
	symbols[count].st_shndx < ehdr.e_shnum &&
	/*	symbols[count].st_size > 0 && */
	symbols[count].st_name && strtab) {
      name = strtab + symbols[count].st_name;
      /* Get only global functions */
      insert_func((char *)(symbols[count].st_value) + base, name, lib);
    }
  } /* for symbols */
  
  free(symbols);
  free(strtab);
  close(procfd);
  return;
}

void init_bug_handler()
{
  char line[1025], plib[1025] = {0};
  char *lib = NULL;
  char *fpath = "/proc/self/maps";
  int done;
  FILE *pfile;
 
  /* Load symbols from program */
  load_elf("/proc/self/exe", 0, NULL);

  /* Find librarys */
  if ((pfile = fopen(fpath, "r")) == NULL) {
    fprintf(stderr, "Tried: '%s'\n", fpath);
    perror("open");
    return;
  }

  /* read program itself off */
  fgets(line, sizeof(line)-1, pfile);
  lib = index(line, '/');
  if (lib) {
    strtok(lib, "\n");
    strcpy(plib, lib);
    /*    fprintf(stderr, "Me: '%s'\n", plib); */
  }

  /* Load symbols from library */
  done = 0;
  while (!done) {
    unsigned addr = 0;

    fscanf(pfile, "%x", &addr);
    fgets(line, sizeof(line)-1, pfile);
    lib = index(line, '/');
    if (lib) {
      strtok(lib, "\n");

      if (strcmp(plib, lib)) {
	char *libname = rindex(lib, '/');
	if (libname)
	  libname++;
	strcpy(plib, lib);
	/*	fprintf(stderr, "Lib: '%s' (%s) @%x\n", plib, libname, addr);*/
	load_elf(plib, addr, libname);
      }

    }
    done = !addr;
  }
  return;
}

extern void *dump_trace(Bool how, void **rvec);
void xcpu_handler(sig, scp, ucp)
     int sig;
     siginfo_t *scp;
     ucontext_t *ucp;
{
   dump_stack_frame(sig, scp, ucp);
   dump_trace(MY_TRUE, NULL);
}

void init_signal(int sig, int what)
{
  struct sigaction act;

  sigemptyset(&act.sa_mask);
  switch (what) {
  case 1:
    act.sa_flags =SA_SIGINFO;
    act.sa_sigaction =bug_handler;
    break;
  case 2:
    act.sa_flags =SA_SIGINFO|SA_RESTART;
    act.sa_sigaction =dump_stack_frame;
    break;
  case 3:
    act.sa_flags =SA_SIGINFO|SA_RESTART;
    act.sa_sigaction =xcpu_handler;
    break;
  default:
    return;
  }
  (void)sigaction(sig, &act, NULL);
}

/* Linux: void (*sa_sigaction)(int, siginfo_t *, void *); */

void dump_stack_frame(sig, scp, ucp)
     int sig;
     siginfo_t *scp;
     ucontext_t *ucp;
{
  char *name;
  int offset;
  char *frame = NULL, *pframe, *libname = NULL;

  fprintf(stderr, "Signal %d errno %d code %d\n",
	  sig, scp->si_errno, scp->si_code);

  switch(sig) {
  case SIGSEGV: 
    fprintf(stderr, "Segmentation fault to 0x%08x: ", (unsigned)scp->si_addr);
    switch(scp->si_code) {
    case SEGV_MAPERR:
      fprintf(stderr, "No mapping at the fault address.\n");
      break;
    case SEGV_ACCERR:
      fprintf(stderr, "Access exceeded protections.\n");
      break;
    default:
      fprintf(stderr, "Unknown reason\n");
      break;
    }
    break;
  case SIGBUS:
    fprintf(stderr, "Bus error: ");
    switch(scp->si_code) {
    case BUS_ADRALN:
      fprintf(stderr, "Invalid address alignment.\n");
      break;
    case BUS_ADRERR:
      fprintf(stderr, "Non-existant physical address.\n");
      break;
    case BUS_OBJERR:
      fprintf(stderr, "Object specific hardware error:\n");
      break;
    default:
      fprintf(stderr, "Unknown reason\n");
      break;
    }
    break;
  case SIGFPE:
    fprintf(stderr, "Arithmetic Exception (FPE).\n");
    switch(scp->si_code) {
    case FPE_INTDIV:
      fprintf(stderr, "Integer divide by zero.\n");
      break;
    case FPE_INTOVF:
      fprintf(stderr, "Integer overflow.\n");
      break;
    case FPE_FLTDIV:
      fprintf(stderr, "Floating point divide by zero.\n");
      break;
    case FPE_FLTOVF:
      fprintf(stderr, "Floating point overflow.\n");
      break;
    case FPE_FLTUND:
      fprintf(stderr, "Floating point underflow.\n");
      break;
    case FPE_FLTRES:
      fprintf(stderr, "Floating point inexact result.\n");
      break;
    case FPE_FLTINV:
      fprintf(stderr, "Floating point invalid operation.\n");
      break;
    case FPE_FLTSUB:
      fprintf(stderr, "Subscript out of range.\n");
      break;
    default:
      fprintf(stderr, "Unknown reason\n");
      break;
    }
    break;
  case SIGPROF:
    fprintf(stderr, "PROFiling signal.\n");
    break;
  case SIGXCPU:
    fprintf(stderr, "XCPU signal.\n");
    break;
  default:
    break;
  }
  if (scp->si_errno) {
    fprintf(stderr, "Perror: %s\n", strerror(scp->si_errno));
  }

  /* Dump call trace */
  fprintf(stderr, "This user context address: 0x%08x\n", (unsigned)ucp);
  if ((unsigned long)ucp < (unsigned long)0x1000) {
    /* Uh. Get some.. */
    static ucontext_t own_ucp;
    ucp = &own_ucp;
    getcontext(ucp);
    fprintf(stderr, "Fetched user context (addr: 0x%08x)\n", (unsigned)ucp);
  }
  if ((unsigned long)ucp > (unsigned long)0x1000) {
    fprintf(stderr, "fp: %08x sp:%08x top:%08x + %08x\n",
	    ucp->uc_mcontext.gregs[REG_EBP],
	    ucp->uc_mcontext.gregs[REG_ESP],
	    (unsigned)ucp->uc_stack.ss_sp,  ucp->uc_stack.ss_size);
    name = addr_to_name((char *)(ucp->uc_mcontext.gregs[REG_EIP]),
			&offset, &libname);
    if (libname)
      fprintf(stderr, " pc: %08x %s@%s()+%x\n",
	      ucp->uc_mcontext.gregs[REG_EIP],
	      name, libname, offset);
    else
      fprintf(stderr, " pc: %08x %s()+%x\n",
	      ucp->uc_mcontext.gregs[REG_EIP],
	      name, offset);

    /* Walk frames */ 
    frame = (char *)(ucp->uc_mcontext.gregs[REG_EBP]);
    pframe = frame - 1;
    while (frame != NULL && (pframe INNER_THAN frame)) {
      libname = NULL;
      name = addr_to_name((char *)(((unsigned *)frame)[1]),
			  &offset, &libname);
      if (libname)
	fprintf(stderr, "rtn: %08x %s@%s()+%x (args: %08x %08x %08x)\n",
		((unsigned *)frame)[1], name, libname, offset,
		((unsigned *)frame)[2],
		((unsigned *)frame)[3],
		((unsigned *)frame)[4]);
      else
	fprintf(stderr, "rtn: %08x %s()+%x (args: %08x %08x %08x)\n",
		((unsigned *)frame)[1], name, offset,
		((unsigned *)frame)[2],
		((unsigned *)frame)[3],
		((unsigned *)frame)[4]);
      pframe = frame;
      frame = (char *)(((unsigned *)frame)[0]);
    }

    if (ucp->uc_link) { /* Not implemented in kernel */
      ucontext_t *tmpucp;
      int count=0;
      
      fprintf(stderr, "There is user context link to %08x.\n",
	      (unsigned)ucp->uc_link);
      for (tmpucp=ucp->uc_link; tmpucp; tmpucp=tmpucp->uc_link) {
	if (tmpucp INNER_THAN ucp ||
	    (tmpucp->uc_link && !(tmpucp INNER_THAN tmpucp->uc_link))) {
	  fprintf(stderr, "Incorrect ucontext in link: %08x\n",
		  (unsigned)tmpucp);
	  break;
	}
	count++;
      }
      fprintf(stderr, "Total of ucontexts: %i\n", count);
      if (count > 10)
	count = 10;
      for (tmpucp=ucp->uc_link; tmpucp&&count>0; tmpucp=tmpucp->uc_link,count--)
	fprintf(stderr, "Ucontext at %08x (stack:%08x).\n",
		(unsigned)tmpucp, tmpucp->uc_mcontext.gregs[REG_ESP]);
    }
  }

  fflush(stdout);
  fflush(stderr);

  { /* unblock */
    sigset_t mask;
    sigemptyset(&mask);
    sigaddset(&mask, sig);
    sigprocmask(SIG_UNBLOCK, &mask, NULL);
  }
}

void bug_handler(sig, scp, ucp)
     int sig;
     siginfo_t *scp;
     ucontext_t *ucp;
{
   if (signal_error_count++ > MAX_FAULTS)
     exit(42);
   dump_stack_frame(sig, scp, ucp);
   error("System error\n");
}

#else /* solaris, linux */

void init_bug_handler()
{
  return;
}

void init_signal(int sig, int what)
{
  struct  sigaction  *act;

  sigemptyset(&act.sa_mask);
  swith (what) {
  case 1:
    act.sa_flags =SA_SIGINFO;
    act.sa_sigaction =bug_handler;
    break;
  case 2:
    act.sa_flags =SA_SIGINFO|SA_RESTART;
    act.sa_sigaction =dump_stack_frame;
    break;
  default:
    return;
  }
  (void)sigaction(sig, &act, NULL);
}

void dump_stack_frame(sig, code, scp, addr)
     int sig, code;
     struct sigcontext *scp;
     char *addr;
{
#ifdef sparc
  struct frame *fr;
  unsigned int pc;
#endif

  fprintf(stderr, "Signal %d code %d address 0x%08x\n",
	  sig, code, (unsigned long)addr);

  switch(sig) {
  case SIGSEGV: 
    fprintf(stderr, "Segmentation fault: ");
#ifdef sparc
    switch(SEGV_CODE(code)) {
    case SEGV_NOMAP:
      fprintf(stderr, "No mapping at the fault address.\n");
      break;
    case SEGV_PROT:
      fprintf(stderr, "Access exceeded protections.\n");
      break;
    case SEGV_OBJERR:
      fprintf(stderr, "Object returned errno value = %d.\n", SEGV_ERRNO(code));
      break;
    default:
      fprintf(stderr, "Unknown reason\n");
      break;
    }
#endif
    break;
  case SIGBUS:
    fprintf(stderr, "Bus error: ");
#ifdef sparc
    switch(BUS_CODE(code)) {
    case BUS_HWERR:
      fprintf(stderr, "Misc hardware error (timeout).\n");
      break;
    case BUS_ALIGN:
      fprintf(stderr, "Hardware alignment error.\n");
      break;
    case BUS_OBJERR:
      fprintf(stderr, "Object returned errno value = %d.\n", BUS_ERRNO(code));
      break;
    default:
      fprintf(stderr, "Unknown reason\n");
      break;
    }
#endif
    break;
 case SIGFPE:
    fprintf(stderr, "Arithmetic Exception (FPE).\n");
    break;
 case SIGPROF:
    fprintf(stderr, "PROFiling signal.\n");
    break;
  default:
    break;
  }

#ifdef sparc
  pc=scp->sc_pc;

  for(fr = (struct frame *)scp->sc_sp;  fr; fr = fr->fr_savfp) {
    fprintf(stderr, "%08x %08x %08x %08x %08x %08x %08x\n", 
	    pc, fr->fr_arg[0], fr->fr_arg[1], fr->fr_arg[2],
	    fr->fr_arg[3], fr->fr_arg[4], fr->fr_arg[5]);

    pc = fr->fr_savpc;
  }
#endif

  fflush(stdout);
  fflush(stderr);
}

void bug_handler(sig, code, scp, addr)
     int sig, code;
     struct sigcontext *scp;
     char *addr;
{
   if (signal_error_count++ > 1000)
     exit(42);
   dump_stack_frame(sig, code, scp, addr);
   error("System error\n");
}
#endif /* solaris */
