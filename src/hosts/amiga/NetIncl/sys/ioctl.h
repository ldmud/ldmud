/*
**	ioctl.h
**
**	s_ioctl() socket ioctl operations
**
**	(C) Copyright 1991 Commodore-Amiga, Inc.
**	    All Rights Reserved
*/

#ifndef SYS_IOCTL_H
#define SYS_IOCTL_H

#define IOCTL(type, op) (((type)<<8) | ((op)&0xff))
#define ROUTE	'r'
#define NETIF	'i'
#define MISC	'm'

#define	SIOCADDRT	IOCTL(ROUTE, 1)	/* add routing entry to list	*/
#define	SIOCDELRT	IOCTL(ROUTE, 2)	/* delete routing entry 	*/

#define	SIOCSIFADDR	IOCTL(NETIF, 3)	/* set interface address	*/
#define	SIOCGIFADDR	IOCTL(NETIF, 4)	/* get interface address	*/
#define	SIOCSIFDSTADDR	IOCTL(NETIF, 5)	/* set interface dest address	*/
#define	SIOCGIFDSTADDR	IOCTL(NETIF, 6)	/* get interface dest address	*/
#define	SIOCSIFFLAGS	IOCTL(NETIF, 7)	/* set interface flags		*/
#define	SIOCGIFFLAGS	IOCTL(NETIF, 8)	/* get interface flags		*/
#define	SIOCGIFCONF	IOCTL(NETIF, 9)	/* get interface configuration	*/
#define	SIOCSIFMTU	IOCTL(NETIF,10)	/* get interface MTU		*/
#define	SIOCGIFMTU	IOCTL(NETIF,11)	/* set interface MTU		*/
#define	SIOCGIFBRDADDR	IOCTL(NETIF,12)	/* get interface brdcst address	*/
#define	SIOCSIFBRDADDR	IOCTL(NETIF,13)	/* set interface brdcst address	*/
#define	SIOCGIFNETMASK	IOCTL(NETIF,14)	/* get interface netmask	*/
#define	SIOCSIFNETMASK	IOCTL(NETIF,15)	/* set interface netmask	*/
#define	SIOCGIFMETRIC	IOCTL(NETIF,16)	/* set interface metric		*/
#define	SIOCSIFMETRIC	IOCTL(NETIF,17)	/* get interface metric		*/
#define SIOCSARP	IOCTL(NETIF,18)	/* set ARP resolution		*/
#define SIOCGARP	IOCTL(NETIF,19)	/* get ARP entry		*/
#define SIOCDARP	IOCTL(NETIF,20)	/* delete arp entry		*/
#define SIOCATMARK	IOCTL(NETIF,21)	/* OOB at mark			*/
#define SIOCSSLIPDEV	IOCTL(NETIF,22)	/* set slip device		*/

#define FIONBIO		IOCTL(MISC, 22)
#define FIONREAD	IOCTL(MISC, 23)
#define FIOASYNC	IOCTL(MISC, 24)

#define SIOCSPGRP	IOCTL(MISC, 25) /* set signalled process	*/
#define SIOCGPGRP	IOCTL(MISC, 26)	/* get signalled process	*/

#endif
