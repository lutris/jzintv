/*

IMASM Macro Precompiler

Copyright (C) 2003  Joe Fisher, Shiny Technologies, LLC
Portions Copyright (C) 2003 Joseph Zbiciak.
http://www.shinytechnologies.com

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of 
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/


#ifndef STRFIFO_H_
#define STRFIFO_H_ 1

#include <fstream>
#include <list>
#include <string>
#include <iostream>

using namespace std;

/* ======================================================================== */
/*  StringFIFO is a generic FIFOing mechanism for providing I/O between     */
/*  line-oriented entities.  StringFIFOs can be "push" or "pull".           */
/*                                                                          */
/*  Strings (more specifically, char*) are pushed onto the FIFO with the    */
/*  putLine() member function.  Strings are popped off the FIFO with the    */
/*  getLine() member function.                                              */
/*                                                                          */
/*  The class supports both "push" and "pull" uses.  Input streams tend to  */
/*  be "pull", and output streams tend to be "push".  More complex hookups  */
/*  tend to be a little of both.                                            */
/*                                                                          */
/*  Two subclasses of StringFIFO specialize FIFOs that are fed by files or  */
/*  which feed files:  StringFIFO_fromFile and StringFIFO_toFile.           */
/*                                                                          */
/*  Another subclass of StringFIFO provides a specialized FIFO that is      */
/*  fed via a callback:  StringFIFO_fromCallback.                           */
/* ======================================================================== */
class StringFIFO
{
    private:
        static const int max_looping = 4000;
        static const int max_queue   = 4000;

    public:
        StringFIFO() : looping(0), q_depth(0),
                       lineNo(0), fname("<internal>") { }
        virtual ~StringFIFO() { }

        virtual bool getLine(char *buf,  int  maxlen, bool &Ignore);
        virtual bool getLine(char **buf, int *buflen, bool &Ignore);

        virtual bool getLine(string &str, bool &Ignore)
        {
            if (queue.empty())
                return false;

            // Base class never increments this
            if (looping > max_looping)
                throw LoopingExceeded(max_looping);

            str    = *(queue.begin());
            Ignore = *(Ignore_queue.begin());

            queue.pop_front();
            Ignore_queue.pop_front();
            q_depth--;

            return true;
        }

        virtual void putLine(const char *buf, bool Ignore)
        {
            q_depth++;
            queue.push_back(string(buf));
            Ignore_queue.push_back(Ignore);

            if (q_depth > max_queue)
                throw QueueDepthExceeded(max_queue);
        }

        virtual void putLine(const string &str, bool Ignore)
        {
            q_depth++;
            queue.push_back(str);
            Ignore_queue.push_back(Ignore);

            if (q_depth > max_queue)
                throw QueueDepthExceeded(max_queue);
        }

        virtual void ungetLine(const char *buf, bool Ignore)
        {
            q_depth++;
            queue.push_front(string(buf));
            Ignore_queue.push_front(Ignore);

            if (q_depth > max_queue)
                throw QueueDepthExceeded(max_queue);
        }

        virtual void ungetLine(const string &str, bool Ignore)
        {
            q_depth++;
            queue.push_front(str);
            Ignore_queue.push_front(Ignore);

            if (q_depth > max_queue)
                throw QueueDepthExceeded(max_queue);
        }


        virtual bool isEmpty(void) { return queue.empty(); }
        virtual bool isEOF(void)   { return queue.empty(); }

        virtual void didLoop(void) 
        { 
            looping++;            

            if (looping > max_looping)
            {
                queue.clear();          // dump remaining queue
                Ignore_queue.clear();   // dump remaining queue
                q_depth = 0;
                throw LoopingExceeded(max_looping);
            }
        }
        virtual void endLoop(void) { looping = 0;          }

        struct BufferOverflow
        {
            int buffer_size, string_length;
            BufferOverflow(int bs, int sl) :
                buffer_size(bs), string_length(sl) { }
        };

        struct LoopingExceeded
        {
            int max_loops;
            LoopingExceeded(int ml) : max_loops(ml) { }
        };

        struct QueueDepthExceeded
        {
            int max_queue;
            QueueDepthExceeded(int mq) : max_queue(mq) { }
        };

        struct loc
        {
            const char *fname;
            int         lineNo;
        };

        virtual const loc *getLocation(void)
        {
            location.fname  = fname;
            location.lineNo = lineNo;
            return &location;
        }


    protected:
        int looping;
        int q_depth;

        list<string> queue;
        list<bool> Ignore_queue;
        
        loc location;
        int lineNo;
        const char *fname;
};


class StringFIFO_fromFile : public StringFIFO
{
    public:
        StringFIFO_fromFile(const char *f_name);
        ~StringFIFO_fromFile();

        bool getLine(char *buf,  int  maxlen, bool &Ignore);
        bool getLine(char **buf, int *buflen, bool &Ignore);
        bool getLine(string &str, bool &Ignore);
        bool isEOF(void);
    private:
        ifstream inFile;
};


class StringFIFO_toFile : public StringFIFO
{
    public:
        StringFIFO_toFile(const char *f_name);
        ~StringFIFO_toFile();

        bool getLine(char  *buf, int  maxlen, bool &Ignore);
        bool getLine(char **buf, int *buflen, bool &Ignore);
        bool getLine(string &str, bool &Ignore);
        void putLine(const char *buf, bool Ignore);
        void putLine(const string &str, bool Ignore);
        void ungetLine(const char *buf, bool Ignore);
        void ungetLine(const string &str, bool Ignore);
        bool isEmpty(void) { return true;  }
        bool isEOF(void)   { return false; }

    private:
        ofstream outFile;
};

static const int hbSize = 1 << 16;

class StringFIFO_fromCallback : public StringFIFO
{
    public:
        StringFIFO_fromCallback
        (
            int (*gl)(char*,int,int*,void*), // getline callback function
            void *glo               = NULL,  // opaque ptr passed to getline
            const char*(*gp)(int *,void*) = NULL,  // get_pos callback function
            void *gpo               = NULL,  // opaque ptr passed to get_pos
            int (*ge)(void*)        = NULL,  // get_eof callback function
            void *geo               = NULL,  // opaque ptr passed to get_eof
            int (*rx)(char*,int,int*,void*) = NULL, // reexam callback fxn
            void *rxo               = NULL   // opaque ptr passed to reexam
        ) : getline(gl), gl_opaque(glo), 
            get_pos(gp), gp_opaque(gpo),
            get_eof(ge), ge_opaque(geo),
            reexam (rx), rx_opaque(rxo)
        {
            is_eof = false;
            lineNo = 0;
            fname  = "<internal>";
            location.fname = fname;
        };
        ~StringFIFO_fromCallback() 
        {
            if (location.fname && location.fname != fname)
                delete[] location.fname;
        };

        bool getLine(char  *buf, int  maxlen, bool &Ignore);
        bool getLine(char **buf, int *buflen, bool &Ignore);
        bool getLine(string &str, bool &Ignore);
        void putLine(const char *buf, bool Ignore);
        void putLine(const string &str, bool Ignore);
        void ungetLine(const char *buf, bool Ignore);
        void ungetLine(const string &str, bool Ignore);

        bool isEmpty(void) { return queue.empty();  }
        bool isEOF(void);
        const loc *getLocation(void);


    private:
        int  (*getline)(char *buf, int maxlen, int *, void *);
        void *gl_opaque;
        const char*(*get_pos)(int*, void*);
        void *gp_opaque;
        int  (*get_eof)(void*);
        void *ge_opaque;
        bool is_eof;
        int  (*reexam)(char *buf, int maxlen, int *, void *);
        void *rx_opaque;

        //static const int hbSize = 4096;
        static char hugeBuf[hbSize];
};


#endif

