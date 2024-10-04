/*
 * hs_bytestring_mmap.h
 *
 * License   : BSD3
 *
 * Copyright (C) 2003   David Roundy
 *               2005-7 Don Stewart
 *
 * Maintainer: Don Stewart <dons@galois.com>
 */

#include <sys/types.h>
#include <sys/mman.h>

unsigned char *hs_bytestring_mmap(size_t len, int fd);
