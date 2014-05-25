#ifndef C_WRAP_H_
#define C_WRAP_H_

#ifdef __cplusplus
extern "C" 
{
#endif

struct parser_callbacks
{
    int         (*getline)(char*, int len, int*, void*);      void  *gl_opaque;
    const char* (*get_pos)(int *line, void *opaque);          void  *gp_opaque;
    int         (*get_eof)(void *opaque);                     void  *ge_opaque;
    int         (*reexam )(char*, int len, int*, void*);      void  *rx_opaque;
    void        (*report_error)(const char *buf, void*op);    void  *re_opaque;
};

int  init_parser (struct parser_callbacks *pc);
void close_parser(void);
char *get_parsed_line(char **buf, int *maxlen, int *ignore);

#ifdef __cplusplus
}
#endif

#endif
