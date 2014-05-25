/* Quick and Dirty header for interfacing to Frank Palazzolo's Disassembler */

#ifndef _DASM1600_H
#define _DASM1600_H

int dasm1600(char *outbuf, int addr, int dbd, int w1, int w2, int w3,
             symtab_t *symtab);

int set_symb_addr_format(int);

#endif

