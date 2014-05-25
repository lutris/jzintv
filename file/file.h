/*
 * ============================================================================
 *  Title:    File I/O Routines
 *  Author:   J. Zbiciak
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
 * ============================================================================
 */


#ifndef FILE_H_
#define FILE_H_

typedef struct path_t
{
    struct path_t *next;
    int            p_len;
    const char    *name;
} path_t;

extern char *exe_path;

/* ======================================================================== */
/*  FILE_READ_ROM16  -- Reads a 16-bit ROM image up to 64K x 16.            */
/*                                                                          */
/*                      Leaves file pointer pointing at end of ROM image    */
/*                      if read is successful.  Returns 0 on success, -1    */
/*                      of failure.                                         */
/* ======================================================================== */
int         file_read_rom16     (FILE *f, int len, uint_16 img[]);
    
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
int         file_read_rom8p2    (FILE *f, int len, uint_16 img[]);


/* ======================================================================== */
/*  FILE_READ_ROM8   -- Reads an 8-bit ROM image up to 64K x 16.            */
/*                                                                          */
/*                      Leaves file pointer pointing at end of ROM image    */
/*                      if read is successful.  Returns 0 on success, -1    */
/*                      of failure.                                         */
/* ======================================================================== */
int         file_read_rom8      (FILE *f, int len, uint_16 img[]);

/* ======================================================================== */
/*  FILE_EXISTS     -- Determines if a given file exists.                   */
/* ======================================================================== */
int file_exists
(
    const char *pathname
);

/* ======================================================================== */
/*  IS_ABSOLUTE_PATH -- Returns non-zero if the path is absolute.           */
/* ======================================================================== */
int is_absolute_path(const char *fname);

/* ======================================================================== */
/*  PATH_FOPEN   -- Wrapper on fopen() that searches down a path.           */
/*                  Warning:  Don't use this with mode = "w" or "wb".       */
/* ======================================================================== */
FILE *path_fopen(path_t *path, const char *fname, const char *mode);

/* ======================================================================== */
/*  EXISTS_IN_PATH -- Looks for file along the given path, returns the      */
/*                    full path if it finds it and it's readable.           */
/* ======================================================================== */
char *exists_in_path(path_t *path, const char *fname);

/* ======================================================================== */
/*  APPEND_PATH  -- Given an existing path, add a new path on the end.      */
/*                  Sure, this will be slow on ridiculously long paths.     */
/* ======================================================================== */
path_t *append_path(path_t *path, const char *fname);

/* ======================================================================== */
/*  PARSE_PATH_STRING                                                       */
/* ======================================================================== */
path_t *parse_path_string(path_t *path, const char *pstr);

/* ======================================================================== */
/*  DUMP_SEARCH_PATH                                                        */
/* ======================================================================== */
void dump_search_path(path_t *path);

/* ======================================================================== */
/*  MAKE_ABSOLUTE_PATH                                                      */
/*                                                                          */
/*  Given a notion of "current working directory", try to make an absolute  */
/*  a path string.  Always returns a freshly allocated string that must be  */
/*  freed by the caller.                                                    */
/* ======================================================================== */
char *make_absolute_path(const char *cwd, const char *path);

/* ======================================================================== */
/*  LOAD_TEXT_FILE                                                          */
/*                                                                          */
/*  Loads a text file into a line-oriented structure.  Attempts to deal     */
/*  with DOS (CR+LF), Mac (CR) and UNIX (LF) newline styles.  Strips off    */
/*  newlines in resulting structure.                                        */
/*                                                                          */
/*  If 'ts' is greater than zero, it will also expand tabs, assuming a      */
/*  tab-stop interval equal to the value of ts.                             */
/*                                                                          */
/*  The text_file structure itself contains a pointer to the file body,     */
/*  along with an array of offsets into that file.  Using offsets saves     */
/*  RAM on machines where sizeof(char *) > sizeof(uint_32).                 */
/*                                                                          */
/*  Line numbers in this structure are 0-based, not 1-based.                */
/* ======================================================================== */
typedef struct text_file
{
    const char *body;
    uint_32    *line;
    uint_32     lines;
} text_file;

text_file *load_text_file(FILE *f, int ts);
#endif

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
/*                 Copyright (c) 1998-2006, Joseph Zbiciak                  */
/* ======================================================================== */
