/*
 * hs_bytestring_mmap.c
 *
 * License   : BSD3
 *
 * Copyright (C) 2003   David Roundy
 *               2005-7 Don Stewart
 *
 * Maintainer: Don Stewart <dons@galois.com>
 */
#include "hs_bytestring_mmap.h"

/* 
 * mmap len bytes from fd into memory, read only.
 */
unsigned char *hs_bytestring_mmap(size_t len, int fd) {
      void *result = mmap(0, len, PROT_READ, MAP_SHARED, fd, 0);
      if (result == MAP_FAILED)
            return (unsigned char *)0;
      else
            return (unsigned char *)result;
}
