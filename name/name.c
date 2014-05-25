#include "config.h"
#include "name.h"

const char *find_cart_name(uint_32 crc32, int *year, 
                           int *default_ecs, int *default_ivc)
{
    int i;

    for (i = 0; name_list[i].name; i++)
    {
        if (name_list[i].crc32 == crc32)
        {
            if (year)           *year        = name_list[i].year;
            if (default_ecs)    *default_ecs = name_list[i].ecs;
            if (default_ivc)    *default_ivc = name_list[i].ivc;

            return name_list[i].name;
        }
    }

    return NULL;
}

