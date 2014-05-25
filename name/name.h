
typedef struct cart_name_t
{
    uint_32     crc32;
    short       year;
    char        ecs, ivc;
    const char *name;
} cart_name_t;

const char *find_cart_name(uint_32 crc32, int *year, int *ecs, int *ivc);
extern const cart_name_t *name_list;
