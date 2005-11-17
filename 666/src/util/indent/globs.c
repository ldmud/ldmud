/* Copyright (C) 1986, 1989 Free Software Foundation, Inc.

This file is part of GNU indent.

GNU indent is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU indent is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU indent; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "indent_globs.h"

/* Like malloc but get error if no storage available.  */

char *
xmalloc (size)
     long size;
{
  register char *val = (char *) calloc (1, size);
  if (!val)
    {
      fprintf (stderr,"indent: Virtual memory exhausted.\n");
      exit (1);
    }
  return val;
}

/* Like realloc but get error if no storage available.  */

char *
xrealloc (ptr, size)
     char *ptr;
     long size;
{
  register char *val = (char *) realloc (ptr, size);
  if (!val)
    {
      fprintf (stderr,"indent: Virtual memory exhausted.\n");
      exit (1);
    }
  return val;
}

