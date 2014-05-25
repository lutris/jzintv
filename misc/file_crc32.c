#include "config.h"
#include "misc/crc32.h"
#include "misc/file_crc32.h"

/* ======================================================================== */
/*  FILE_CRC32   -- Return the CRC-32 for a file.                           */
/* ======================================================================== */
uint_32 file_crc32(const char *fname)
{
    FILE *f;
    uint_32 crc = 0xFFFFFFFFU;
    int c;

    if (fname == NULL)
        f = stdin;
    else if (!(f = fopen(fname, "rb")))
        return crc;

    while ((c = fgetc(f)) != EOF)
        crc = crc32_update(crc, c & 0xFF);

    if (fname)
        fclose(f);

    return crc ^ 0xFFFFFFFFU;
}
