/*
 * ============================================================================
 *  Title:    File I/O Routines
 *  Author:   J. Zbiciak
 *  $Id: file.c,v 1.5 2001/11/02 02:00:03 im14u2c Exp $
 * ============================================================================
 *  This module contains routines for reading/writing files, including ROM
 *  images, CFG files, etc.
 *
 *  Currently, these routines operate on FILE*'s rather than on filenames,
 *  since I'd like these to be able to work in structured files someday.
 *  (eg. so I can read a ROM image out of an archive, or such.)
 * ============================================================================
 *  FILE_READ_ROM16      -- Reads a 16-bit big-endian ROM image.
 *  FILE_READ_ROM8P2     -- Reads a 10-bit ROM image in 8 plus 2 format
 *  FILE_READ_ROM10      -- Reads an 8-bit ROM image (eg. GROM).
 *  FILE_PARSE_CFG       -- Parses a CFG file and returns a linked list of 
 *                          configuration actions to be handled by the
 *                          machine configuration engine.
 * ============================================================================
 */


#include <stdio.h>
#include <stdlib.h>

#include "config.h"
#include "file.h"

char *exe_path;


/* ======================================================================== */
/*  FILE_READ_ROM16  -- Reads a 16-bit ROM image up to 64K x 16.            */
/*                                                                          */
/*                      Leaves file pointer pointing at end of ROM image    */
/*                      if read is successful.  Returns 0 on success, -1    */
/*                      of failure.                                         */
/* ======================================================================== */
int         file_read_rom16     (FILE *f, int len, uint_16 img[])
{
    int r;

    /* -------------------------------------------------------------------- */
    /*  Sanity check:  To all the arguments make sense?                     */
    /* -------------------------------------------------------------------- */
    if (!f || !img || len < 0)
    {
        fprintf(stderr, "file_read_rom16:  Bad parameters!\n"
                        "                  %p, %10d, %p\n", f, len, img);
        exit(1);
    }

    /* -------------------------------------------------------------------- */
    /*  Read in the ROM image.                                              */
    /* -------------------------------------------------------------------- */
    len = fread((void*) img, 2, len, f);


    /* ---------------------------------------------------------------- */
    /*  Bring the ROM image into the host endian.                       */
    /* ---------------------------------------------------------------- */
    for (r = 0; r < len; r++)
        img[r] = be_to_host_16(img[r]);

    return len;
}
    
/* ======================================================================== */
/*  FILE_READ_ROM8P2 -- Reads a 10-bit ROM image up to 64K x 16 in packed   */
/*                      8 plus 2 format.  The first 'len' bytes are         */
/*                      the 8 LSB's of the ROM's decles.  The next          */
/*                      'len / 4' bytes hold the 2 MSBs, packed in little-  */
/*                      endian order.  This format is used by the VOL1,     */
/*                      VOL2 resource files, and is included for            */
/*                      completeness.                                       */
/*                                                                          */
/*                      Leaves file pointer pointing at end of ROM image    */
/*                      if read is successful.  Returns 0 on success, -1    */
/*                      of failure.                                         */
/* ======================================================================== */
int         file_read_rom8p2    (FILE *f, int len, uint_16 img[])
{
    int r, blk8sz, blk2sz, blk8, blk2, shl;
    uint_8 *tmp;

    /* -------------------------------------------------------------------- */
    /*  Sanity check:  To all the arguments make sense?                     */
    /* -------------------------------------------------------------------- */
    if (!f || !img || len < 0)
    {
        fprintf(stderr, "file_read_rom8p2:  Bad parameters!\n"
                        "                   %p, %10d, %p\n", f, len, img);
        exit(1);
    }

    /* -------------------------------------------------------------------- */
    /*  Calculate the sizes of the 8-bit and 2-bit sections, being careful  */
    /*  to round the decle count up to handle non-multiple-of-4 images.     */
    /* -------------------------------------------------------------------- */
    blk8sz = len;
    blk2sz = (len + 3) >> 2;

    /* -------------------------------------------------------------------- */
    /*  Read in the ROM image to a temporary storage buffer for unpacking.  */
    /* -------------------------------------------------------------------- */
    tmp = CALLOC(uint_8, blk8sz + blk2sz);

    if (!tmp)
    {
        fprintf(stderr, "file_read_rom8p2:  Out of memory.\n");
        exit(1);
    }

    r = fread(tmp, 1, blk8sz + blk2sz, f);

    if (r != blk8sz + blk2sz)
    {
        fprintf(stderr, "file_read_rom8p2:  Error reading ROM image.\n");
        perror("fread()");

        free(tmp);
        return -1;
    }

    /* -------------------------------------------------------------------- */
    /*  Unpack the ROM image into the user's buffer.                        */
    /* -------------------------------------------------------------------- */
    for (blk8 = 0, blk2 = blk8sz; blk8 < blk8sz; blk8++)
    {
        shl = 8 - ((blk8 & 3) << 1);

        img[blk8] = tmp[blk8] | (0x0300 & (tmp[blk2] << shl));

        if ((blk8 & 3) == 3) blk2++;
    }

    free(tmp);

    return len;
}


/* ======================================================================== */
/*  FILE_READ_ROM8   -- Reads an 8-bit ROM image up to 64K x 16.            */
/*                                                                          */
/*                      Leaves file pointer pointing at end of ROM image    */
/*                      if read is successful.  Returns 0 on success, -1    */
/*                      of failure.                                         */
/* ======================================================================== */
int         file_read_rom8      (FILE *f, int len, uint_16 img[])
{
    int r; 
    uint_16 packed;

    /* -------------------------------------------------------------------- */
    /*  Sanity check:  To all the arguments make sense?                     */
    /* -------------------------------------------------------------------- */
    if (!f || !img || len < 0)
    {
        fprintf(stderr, "file_read_rom8:  Bad parameters!\n"
                        "                 %p, %10d, %p\n", f, len, img);
        exit(1);
    }

    /* -------------------------------------------------------------------- */
    /*  Read in the ROM image.                                              */
    /* -------------------------------------------------------------------- */
    len = fread((void*) img, 1, len, f);

    /* -------------------------------------------------------------------- */
    /*  Unpack the ROM image.                                               */
    /* -------------------------------------------------------------------- */
    for (r = len; r >= 0; r -= 2)
    {
        packed = host_to_le_16(img[r >> 1]);

        img[r + 1] = packed >> 8;
        img[r + 0] = packed & 0xFF;
    }

    return len;
}

/* ======================================================================== */
/*  FILE_EXISTS     -- Determines if a given file exists.                   */
/* ======================================================================== */
int file_exists
(
    const char *pathname
)
{
    /* -------------------------------------------------------------------- */
    /*  NOTE: access() isn't portable, so fall back to fopen() if needed.   */
    /* -------------------------------------------------------------------- */
#ifndef NO_ACCESS
    return access(pathname, R_OK|F_OK) != -1;
#else
    FILE *f = fopen(pathname, "r");

    if (f)
        fclose(f);

    return f != NULL;
#endif
}

/* ======================================================================== */
/*  IS_ABSOLUTE_PATH -- Returns non-zero if the path is absolute.           */
/* ======================================================================== */
int is_absolute_path(const char *fname)
{
    if (fname[0] == PATH_SEP)
        return 1;

#ifdef WIN32
    /* Look for a drive letter */
    if (isalpha(fname[0]) && fname[1] == ':' && fname[2] == PATH_SEP)
        return 1;
#endif

#if defined(__AMIGAOS4__) || defined(WII)
    /* Look for a prefix of the form "VOL:".  Allow everything but the
     * path separator to appear before a ":".  */
    {
        const char *s;

        s = fname;
        while (*s)
        {
            if (*s == PATH_SEP)
                break;

            if (*s == ':')
                return 1;

            s++;
        }
    }
#endif

    return 0;
}

/* ======================================================================== */
/*  PATH_FOPEN   -- Wrapper on fopen() that searches down a path.           */
/*                  Warning:  Don't use this with mode = "w" or "wb".       */
/* ======================================================================== */
FILE *path_fopen(path_t *path, const char *fname, const char *mode)
{
    int f_len, b_len;
    char *buf;
    FILE *f;

    /* -------------------------------------------------------------------- */
    /*  If the path is empty, or the filename specifies an absolute path,   */
    /*  just do a bare fopen.                                               */
    /* -------------------------------------------------------------------- */
    if (!path || is_absolute_path(fname))
        return fopen(fname, mode);

    /* -------------------------------------------------------------------- */
    /*  Dynamically allocate string buffer to avoid overflows.              */
    /* -------------------------------------------------------------------- */
    f_len = strlen(fname);
    b_len = f_len * 2 + 2;
    buf   = CALLOC(char, b_len);

    /* -------------------------------------------------------------------- */
    /*  Check all the path elements.                                        */
    /* -------------------------------------------------------------------- */
    while (path)
    {
        if (b_len < f_len + path->p_len + 2)
        {
            b_len = 2 * (f_len + path->p_len) + 2;
            buf   = (char *)realloc(buf, b_len);
        }

        strcpy(buf, path->name);
        buf[path->p_len] = PATH_SEP;
        strcpy(buf + path->p_len + 1, fname);

        if ((f = fopen(buf, mode)) != NULL)
        {
            free(buf);
            return f;
        }

        path = path->next;
    }

    /* -------------------------------------------------------------------- */
    /*  Didn't find it?  Give up.                                           */
    /* -------------------------------------------------------------------- */
    free(buf);

    return NULL;
}

/* ======================================================================== */
/*  EXISTS_IN_PATH -- Looks for file along the given path, returns the      */
/*                    full path if it finds it and it's readable.           */
/* ======================================================================== */
char *exists_in_path(path_t *path, const char *fname)
{
    int f_len, b_len;
    char *buf;

    /* -------------------------------------------------------------------- */
    /*  If the path is empty, just look in CWD.                             */
    /* -------------------------------------------------------------------- */
    if (!path || is_absolute_path(fname))
    {
        if (file_exists(fname))
            return strdup(fname);
        else
            return NULL;
    }

    /* -------------------------------------------------------------------- */
    /*  Dynamically allocate string buffer to avoid overflows.              */
    /* -------------------------------------------------------------------- */
    f_len = strlen(fname);
    b_len = f_len * 2 + 2;
    buf   = CALLOC(char, b_len);

    /* -------------------------------------------------------------------- */
    /*  Check all the path elements.                                        */
    /* -------------------------------------------------------------------- */
    while (path)
    {
        if (b_len < f_len + path->p_len + 2)
        {
            b_len = 2 * (f_len + path->p_len) + 2;
            buf   = (char *)realloc(buf, b_len);
        }

        strcpy(buf, path->name);
        buf[path->p_len] = PATH_SEP;
        strcpy(buf + path->p_len + 1, fname);

        if (file_exists(buf))
            return buf;
    }

    /* -------------------------------------------------------------------- */
    /*  Didn't find it?  Give up.                                           */
    /* -------------------------------------------------------------------- */
    free(buf);

    return NULL;
}

/* ======================================================================== */
/*  APPEND_PATH  -- Given an existing path, add a new path on the end.      */
/*                  Sure, this will be slow on ridiculously long paths.     */
/* ======================================================================== */
path_t *append_path(path_t *path, const char *fname)
{
    path_t *head = path, **node;
    char *local_fname;

    if (fname[0] == '=')
    {
        int l;
        char *n;

        if (!exe_path)
            return path;

        l = strlen(exe_path) + strlen(fname) + 1;

        if (!(n = CALLOC(char, l + 1)))
        {
            fprintf(stderr, "out of memory\n");
            exit(1);
        }
        
        snprintf(n, l+1, "%s%c%s", exe_path, PATH_SEP, fname + 1);

        local_fname = n;
    } else
    {
        local_fname = strdup(fname);
    }

    for (node = &head; *node; node = &(*node)->next)
        if (!strcmp((*node)->name, local_fname))
        {
            free(local_fname);
            return head;
        }

    *node = (path_t *)calloc(sizeof(path_t), 1);
    (*node)->p_len = strlen(local_fname);
    (*node)->name  = local_fname;

    return head;
}

/* ======================================================================== */
/*  PARSE_PATH_STRING                                                       */
/* ======================================================================== */
path_t *parse_path_string(path_t *path, const char *pstr)
{
    char *str, *p;

    if (!pstr || !strlen(pstr))
        return path;
    
    /* get writeable local copy for strtok */
    str = strdup(pstr);

    p = strtok(str, PATH_COMPONENT_SEP);
    while (p)
    {
        path = append_path(path, p);
        p = strtok(NULL, PATH_COMPONENT_SEP);
    }

    free(str);  /* dump writeable local copy */
    return path;
}

/* ======================================================================== */
/*  DUMP_SEARCH_PATH                                                        */
/* ======================================================================== */
void dump_search_path(path_t *path)
{
    fprintf(stderr, "\nSearch path:\n");

    while (path)
    {
        fprintf(stderr, "  %s\n", path->name);
        path = path->next;
    }

    fprintf(stderr, "\n");
}

/* ======================================================================== */
/*  MAKE_ABSOLUTE_PATH                                                      */
/*                                                                          */
/*  Given a notion of "current working directory", try to make an absolute  */
/*  a path string.  Always returns a freshly allocated string that must be  */
/*  freed by the caller.                                                    */
/* ======================================================================== */
char *make_absolute_path(const char *cwd, const char *path)
{
    char *new_path;
    int  c_len = strlen(cwd);
    int  p_len = strlen(path);

    if (!is_absolute_path(path))
    {
        new_path = (char *)malloc(c_len + p_len + 2);

        memcpy(new_path,             cwd,  c_len);
        memcpy(new_path + c_len + 1, path, p_len);

        new_path[c_len            ] = PATH_SEP;
        new_path[c_len + p_len + 1] = 0;
    } else
    {
        new_path = strdup(path);
    }

    return new_path;
}

/* ======================================================================== */
/*  LOAD_TEXT_FILE                                                          */
/*                                                                          */
/*  Loads a text file into a line-oriented structure.  Attempts to deal     */
/*  with DOS (CR+LF), Mac (CR) and UNIX (LF) newline styles.  Strips off    */
/*  newlines in resulting structure.                                        */
/*                                                                          */
/*  The text_file structure itself contains a pointer to the file body,     */
/*  along with an array of offsets into that file.  Using offsets saves     */
/*  RAM on machines where sizeof(char *) > sizeof(uint_32).                 */
/* ======================================================================== */
text_file *load_text_file(FILE *f, int ts)
{
    long len;
    char *body;
    size_t r;
    text_file *tf;
    uint_32 *l_ptr, idx, l_start;
    int lines = 0, l_alloc, got_line = 0;

    /* -------------------------------------------------------------------- */
    /*  Allocate one large buffer for the entire file.                      */
    /* -------------------------------------------------------------------- */
    fseek(f, 0, SEEK_END);
    len = ftell(f);

    if (len > 1 << 30 || len < 1)
        return NULL;

    if (!(body = (char *)malloc(len)))
        return NULL;

    fseek(f, 0, SEEK_SET);

    /* -------------------------------------------------------------------- */
    /*  Slurp in the entire file.                                           */
    /* -------------------------------------------------------------------- */
    r = fread(body, 1, len, f);
    if ((int)r < len)
    {
        free(body);
        return NULL;
    }

    /* -------------------------------------------------------------------- */
    /*  Convert tabs to spaces if asked to do so.                           */
    /* -------------------------------------------------------------------- */
    if (ts > 0)
    {
        int new_len = len;
        int col = 0;
        char *os, *ns;

        /* Initial scan:  Count up how much white space we'll add. */
        if (ts > 1) 
            for (idx = 0; idx < (uint_32)len; idx++)
            {
                int ch = body[idx];

                if (ch == '\t')
                {
                    int tab;

                    tab      = ts - (col % ts);
                    col     += tab;
                    new_len += tab - 1;
                } else 
                {
                    col++;
                }
                
                if (ch == '\012' || ch == '\015')
                    col = 0;
            }

        /* Next, if the file got larger, reallocate the buffer and slide    */
        /* it to the end so we can do this in-place.                        */
        if (new_len > len)
        {
            body = (char *)realloc(body, new_len);
            if (!body)
                return NULL;

            os = body + new_len - len;
            ns = body;
            memmove(os, ns, len);
        } else
        {
            os = ns = body;
        }

        /* Now expand-in-place. */
        for (idx = 0; idx < (uint_32)len; idx++)
        {
            int ch = *os++;

            if (ch == '\t')
            {
                int tab;

                tab  = ts - (col % ts);
                col += tab;

                while (tab-- > 0)
                    *ns++ = ' ';
            } else
            {
                col++;
                *ns++ = ch;
            }
            
            if (ch == '\012' || ch == '\015')
                col = 0;

            assert(ns <= os);
        }

        len = new_len;
    }

    /* -------------------------------------------------------------------- */
    /*  Start with an initial line-pointer buffer.                          */
    /* -------------------------------------------------------------------- */
    l_alloc = 4096;
    l_ptr   = (uint_32 *)malloc(sizeof(uint_32) * l_alloc);


    /* -------------------------------------------------------------------- */
    /*  Find the line breaks and poke in NULs.                              */
    /* -------------------------------------------------------------------- */
    l_start = 0;
    idx     = 0;
    while (idx < (uint_32)len)
    {
        if (body[idx] == '\012')    /* LF:  Assume it's UNIX    */
        {
            body[idx] = 0;
            got_line  = 1;
        }

        if (body[idx] == '\015')    /* CR:  Could be DOS or Mac */
        {
            body[idx] = 0;
            if (body[idx + 1] == '\012')    /* Skip LF immediately after CR */
                idx++;
            got_line = 1;
        }

        idx++;

        if (got_line)
        {
            if (lines >= l_alloc)
            {
                l_alloc <<= 1;
                l_ptr   = (uint_32 *)realloc(l_ptr, l_alloc * sizeof(uint_32));
            }
            l_ptr[lines++] = l_start;
            l_start  = idx;
            got_line = 0;
        }
    }

    /* -------------------------------------------------------------------- */
    /*  Finally, construct the text_file structure.                         */
    /* -------------------------------------------------------------------- */
    tf = (text_file *)calloc(sizeof(text_file), 1);
    tf->body  = body;
    tf->line  = l_ptr;
    tf->lines = lines;

    return tf;
}


/* ======================================================================== */
/*  This program is free software; you can redistribute it and/or modify    */
/*  it under the terms of the GNU General Public License as published by    */
/*  the Free Software Foundation; either version 2 of the License, or       */
/*  (at your option) any later version.                                     */
/*                                                                          */
/*  This program is distributed in the hope that it will be useful,         */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of          */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       */
/*  General Public License for more details.                                */
/*                                                                          */
/*  You should have received a copy of the GNU General Public License       */
/*  along with this program; if not, write to the Free Software             */
/*  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               */
/* ======================================================================== */
/*                 Copyright (c) 1998-1999, Joseph Zbiciak                  */
/* ======================================================================== */
