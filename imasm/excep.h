#ifndef EXCEP_H_
#define EXCEP_H_

#include <cstdio>
#include <cstring>
#include <string>

using namespace std;

struct FileNotFound
{
    string msg;
    const char *dir;
    const char *fname;

    FileNotFound(const char *d, const char *f) : dir(d), fname(f)
    {
        msg = string("Could not open ") + dir + " file '" + fname + "'";
    }
};

struct InternalError
{
    const char* module;
    const char* error;
    const char* file;
    int         line;
    string msg;

    InternalError
    (
        const char *mo, 
        const char *es,
        const char *fi = NULL,
        int         li = 0
    ) : module(mo), error(es), file(fi), line(li)
    {
        if (file && line)
        {
            char buf[10];
            sprintf(buf, "%d", line);

            msg = string("INTERNAL ERROR in ") + module + 
                  " (" + fi + ":" + buf + "):\n\t" + es;
        } else
        {
            msg = string("INTERNAL ERROR in ")+module+":\n\t"+es;
        }
    }
    InternalError
    (
        const char *mo, 
        const string &es,
        const char *fi = NULL,
        int         li = 0
    ) : module(mo), error(es.c_str()), file(fi), line(li)
    {
        if (file && line)
        {
            char buf[10];
            sprintf(buf, "%d", line);

            msg = string("INTERNAL ERROR in ") + module + 
                  " (" + fi + ":" + buf + "):\n\t" + es;
        } else
        {
            msg = string("INTERNAL ERROR in ")+module+":\n\t"+es;
        }
    }
};


#endif
