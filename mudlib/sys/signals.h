#ifndef LPC_SIGNALS_H_
#define LPC_SIGNALS_H_ 1

// Our defines for some POSIX signals the driver defers to the mudlib.
// This abstraction is intentional.
#define LPC_SIGHUP    1
#define LPC_SIGINT    2
#define LPC_SIGTERM   15
#define LPC_SIGUSR1   16
#define LPC_SIGUSR2   17

// __STD__ is defined during the driver compilation (the driver includes this
// file as well) but not in LPC programs. In LPC we may safely define the
// signals name without the LPC_ prefix.
#if !defined(__STDC__)
#define SIGHUP       LPC_SIGHUP
#define SIGINT       LPC_SIGINT
#define SIGTERM      LPC_SIGTERM
#define SIGUSR1      LPC_SIGUSR1
#define SIGUSR2      LPC_SIGUSR2
#endif // __STD__

#endif /* LPC_SIGNALS_H_ */
