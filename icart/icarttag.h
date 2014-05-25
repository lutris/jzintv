
#ifndef ICARTTAG_H_
#define ICARTTAG_H_ 1

/* ======================================================================== */
/*  ICT_TYPE_T   -- Defined cartridge tag types.                            */
/* ======================================================================== */
typedef enum ict_type_t
{
    /* -------------------------------------------------------------------- */
    /*  General Game Information tag ID #'s.                                */
    /* -------------------------------------------------------------------- */
    ICT_IGNORE      = 0x00,     ICT_RSVD_10     = 0x10, 
    ICT_TITLE       = 0x01,     ICT_RSVD_11     = 0x11, 
    ICT_PUBLISHER   = 0x02,     ICT_RSVD_12     = 0x12, 
    ICT_CREDITS     = 0x03,     ICT_RSVD_13     = 0x13, 
    ICT_INFOURLS    = 0x04,     ICT_RSVD_14     = 0x14, 
    ICT_RLSDATE     = 0x05,     ICT_RSVD_15     = 0x15, 
    ICT_ATTRIBS     = 0x06,     ICT_RSVD_16     = 0x16, 
    ICT_BINDINGS    = 0x07,     ICT_RSVD_17     = 0x17, 
    ICT_RSVD_08     = 0x08,     ICT_RSVD_18     = 0x18,     
    ICT_RSVD_09     = 0x09,     ICT_RSVD_19     = 0x19,     
    ICT_RSVD_0A     = 0x0A,     ICT_RSVD_1A     = 0x1A,     
    ICT_RSVD_0B     = 0x0B,     ICT_RSVD_1B     = 0x1B,     
    ICT_RSVD_0C     = 0x0C,     ICT_RSVD_1C     = 0x1C,     
    ICT_RSVD_0D     = 0x0D,     ICT_RSVD_1D     = 0x1D,     
    ICT_RSVD_0E     = 0x0E,     ICT_RSVD_1E     = 0x1E,     
    ICT_RSVD_0F     = 0x0F,     ICT_RSVD_1F     = 0x1F,     

    /* -------------------------------------------------------------------- */
    /*  Debugging Information tag ID #'s.                                   */
    /* -------------------------------------------------------------------- */
    ICT_SYMTAB      = 0x20,     ICT_RSVD_30     = 0x30,
    ICT_MEMATTR     = 0x21,     ICT_RSVD_31     = 0x31,
    ICT_LINEMAP     = 0x22,     ICT_RSVD_32     = 0x32,
    ICT_RSVD_23     = 0x23,     ICT_RSVD_33     = 0x33,
    ICT_RSVD_24     = 0x24,     ICT_RSVD_34     = 0x34,
    ICT_RSVD_25     = 0x25,     ICT_RSVD_35     = 0x35,
    ICT_RSVD_26     = 0x26,     ICT_RSVD_36     = 0x36,
    ICT_RSVD_27     = 0x27,     ICT_RSVD_37     = 0x37,
    ICT_RSVD_28     = 0x28,     ICT_RSVD_38     = 0x38,
    ICT_RSVD_29     = 0x29,     ICT_RSVD_39     = 0x39,
    ICT_RSVD_2A     = 0x2A,     ICT_RSVD_3A     = 0x3A,
    ICT_RSVD_2B     = 0x2B,     ICT_RSVD_3B     = 0x3B,
    ICT_RSVD_2C     = 0x2C,     ICT_RSVD_3C     = 0x3C,
    ICT_RSVD_2D     = 0x2D,     ICT_RSVD_3D     = 0x3D,
    ICT_RSVD_2E     = 0x2E,     ICT_RSVD_3E     = 0x3E,
    ICT_RSVD_2F     = 0x2F,     ICT_RSVD_3F     = 0x3F,
    
    /* -------------------------------------------------------------------- */
    /*  RESERVED:  0x40-0xEF                                                */
    /* -------------------------------------------------------------------- */
    ICT_RSVD_40=0x40, ICT_RSVD_50=0x50, ICT_RSVD_60=0x60, ICT_RSVD_70=0x70,
    ICT_RSVD_41=0x41, ICT_RSVD_51=0x51, ICT_RSVD_61=0x61, ICT_RSVD_71=0x71,
    ICT_RSVD_42=0x42, ICT_RSVD_52=0x52, ICT_RSVD_62=0x62, ICT_RSVD_72=0x72,
    ICT_RSVD_43=0x43, ICT_RSVD_53=0x53, ICT_RSVD_63=0x63, ICT_RSVD_73=0x73,
    ICT_RSVD_44=0x44, ICT_RSVD_54=0x54, ICT_RSVD_64=0x64, ICT_RSVD_74=0x74,
    ICT_RSVD_45=0x45, ICT_RSVD_55=0x55, ICT_RSVD_65=0x65, ICT_RSVD_75=0x75,
    ICT_RSVD_46=0x46, ICT_RSVD_56=0x56, ICT_RSVD_66=0x66, ICT_RSVD_76=0x76,
    ICT_RSVD_47=0x47, ICT_RSVD_57=0x57, ICT_RSVD_67=0x67, ICT_RSVD_77=0x77,
    ICT_RSVD_48=0x48, ICT_RSVD_58=0x58, ICT_RSVD_68=0x68, ICT_RSVD_78=0x78,
    ICT_RSVD_49=0x49, ICT_RSVD_59=0x59, ICT_RSVD_69=0x69, ICT_RSVD_79=0x79,
    ICT_RSVD_4A=0x4A, ICT_RSVD_5A=0x5A, ICT_RSVD_6A=0x6A, ICT_RSVD_7A=0x7A,
    ICT_RSVD_4B=0x4B, ICT_RSVD_5B=0x5B, ICT_RSVD_6B=0x6B, ICT_RSVD_7B=0x7B,
    ICT_RSVD_4C=0x4C, ICT_RSVD_5C=0x5C, ICT_RSVD_6C=0x6C, ICT_RSVD_7C=0x7C,
    ICT_RSVD_4D=0x4D, ICT_RSVD_5D=0x5D, ICT_RSVD_6D=0x6D, ICT_RSVD_7D=0x7D,
    ICT_RSVD_4E=0x4E, ICT_RSVD_5E=0x5E, ICT_RSVD_6E=0x6E, ICT_RSVD_7E=0x7E,
    ICT_RSVD_4F=0x4F, ICT_RSVD_5F=0x5F, ICT_RSVD_6F=0x6F, ICT_RSVD_7F=0x7F,

    ICT_RSVD_80=0x80, ICT_RSVD_90=0x90, ICT_RSVD_A0=0xA0, ICT_RSVD_B0=0xB0,
    ICT_RSVD_81=0x81, ICT_RSVD_91=0x91, ICT_RSVD_A1=0xA1, ICT_RSVD_B1=0xB1,
    ICT_RSVD_82=0x82, ICT_RSVD_92=0x92, ICT_RSVD_A2=0xA2, ICT_RSVD_B2=0xB2,
    ICT_RSVD_83=0x83, ICT_RSVD_93=0x93, ICT_RSVD_A3=0xA3, ICT_RSVD_B3=0xB3,
    ICT_RSVD_84=0x84, ICT_RSVD_94=0x94, ICT_RSVD_A4=0xA4, ICT_RSVD_B4=0xB4,
    ICT_RSVD_85=0x85, ICT_RSVD_95=0x95, ICT_RSVD_A5=0xA5, ICT_RSVD_B5=0xB5,
    ICT_RSVD_86=0x86, ICT_RSVD_96=0x96, ICT_RSVD_A6=0xA6, ICT_RSVD_B6=0xB6,
    ICT_RSVD_87=0x87, ICT_RSVD_97=0x97, ICT_RSVD_A7=0xA7, ICT_RSVD_B7=0xB7,
    ICT_RSVD_88=0x88, ICT_RSVD_98=0x98, ICT_RSVD_A8=0xA8, ICT_RSVD_B8=0xB8,
    ICT_RSVD_89=0x89, ICT_RSVD_99=0x99, ICT_RSVD_A9=0xA9, ICT_RSVD_B9=0xB9,
    ICT_RSVD_8A=0x8A, ICT_RSVD_9A=0x9A, ICT_RSVD_AA=0xAA, ICT_RSVD_BA=0xBA,
    ICT_RSVD_8B=0x8B, ICT_RSVD_9B=0x9B, ICT_RSVD_AB=0xAB, ICT_RSVD_BB=0xBB,
    ICT_RSVD_8C=0x8C, ICT_RSVD_9C=0x9C, ICT_RSVD_AC=0xAC, ICT_RSVD_BC=0xBC,
    ICT_RSVD_8D=0x8D, ICT_RSVD_9D=0x9D, ICT_RSVD_AD=0xAD, ICT_RSVD_BD=0xBD,
    ICT_RSVD_8E=0x8E, ICT_RSVD_9E=0x9E, ICT_RSVD_AE=0xAE, ICT_RSVD_BE=0xBE,
    ICT_RSVD_8F=0x8F, ICT_RSVD_9F=0x9F, ICT_RSVD_AF=0xAF, ICT_RSVD_BF=0xBF,

    ICT_RSVD_C0=0xC0, ICT_RSVD_D0=0xD0, ICT_RSVD_E0=0xE0,
    ICT_RSVD_C1=0xC1, ICT_RSVD_D1=0xD1, ICT_RSVD_E1=0xE1,
    ICT_RSVD_C2=0xC2, ICT_RSVD_D2=0xD2, ICT_RSVD_E2=0xE2,
    ICT_RSVD_C3=0xC3, ICT_RSVD_D3=0xD3, ICT_RSVD_E3=0xE3,
    ICT_RSVD_C4=0xC4, ICT_RSVD_D4=0xD4, ICT_RSVD_E4=0xE4,
    ICT_RSVD_C5=0xC5, ICT_RSVD_D5=0xD5, ICT_RSVD_E5=0xE5,
    ICT_RSVD_C6=0xC6, ICT_RSVD_D6=0xD6, ICT_RSVD_E6=0xE6,
    ICT_RSVD_C7=0xC7, ICT_RSVD_D7=0xD7, ICT_RSVD_E7=0xE7,
    ICT_RSVD_C8=0xC8, ICT_RSVD_D8=0xD8, ICT_RSVD_E8=0xE8,
    ICT_RSVD_C9=0xC9, ICT_RSVD_D9=0xD9, ICT_RSVD_E9=0xE9,
    ICT_RSVD_CA=0xCA, ICT_RSVD_DA=0xDA, ICT_RSVD_EA=0xEA,
    ICT_RSVD_CB=0xCB, ICT_RSVD_DB=0xDB, ICT_RSVD_EB=0xEB,
    ICT_RSVD_CC=0xCC, ICT_RSVD_DC=0xDC, ICT_RSVD_EC=0xEC,
    ICT_RSVD_CD=0xCD, ICT_RSVD_DD=0xDD, ICT_RSVD_ED=0xED,
    ICT_RSVD_CE=0xCE, ICT_RSVD_DE=0xDE, ICT_RSVD_EE=0xEE,
    ICT_RSVD_CF=0xCF, ICT_RSVD_DF=0xDF, ICT_RSVD_EF=0xEF,

    /* -------------------------------------------------------------------- */
    /*  Extended Tags:  0xF0 - 0xFF                                         */
    /* -------------------------------------------------------------------- */
    ICT_EXTENDED    = 0xF0,     ICT_RSVD_F8     = 0xF8,
    ICT_RSVD_F1     = 0xF1,     ICT_RSVD_F9     = 0xF9,
    ICT_RSVD_F2     = 0xF2,     ICT_RSVD_FA     = 0xFA,
    ICT_RSVD_F3     = 0xF3,     ICT_RSVD_FB     = 0xFB,
    ICT_RSVD_F4     = 0xF4,     ICT_RSVD_FC     = 0xFC,
    ICT_RSVD_F5     = 0xF5,     ICT_RSVD_FD     = 0xFD,
    ICT_RSVD_F6     = 0xF6,     ICT_RSVD_FE     = 0xFE,
    ICT_RSVD_F7     = 0xF7,     ICT_RSVD_FF     = 0xFF,
} ict_type_t;

/* ======================================================================== */
/*  Flag bitfields used by many of the tag types.                           */
/* ======================================================================== */
#define ICT_CRED_PROG       (0x01)
#define ICT_CRED_GAMEART    (0x02)
#define ICT_CRED_MUSIC      (0x04)
#define ICT_CRED_SFX        (0x08)
#define ICT_CRED_VOICE      (0x10)
#define ICT_CRED_DOCS       (0x20)
#define ICT_CRED_CONCEPT    (0x40)
#define ICT_CRED_BOXART     (0x80)

#define ICT_CMPT_KEYBD      (0x000003)
#define ICT_CMPT_VOICE      (0x00000C)
#define ICT_CMPT_4CTRL      (0x000030)
#define ICT_CMPT_ECS        (0x0000C0)
#define ICT_CMPT_INTY2      (0x000300)
#define ICT_CMPT_RSVD5      (0x000C00)
#define ICT_CMPT_RSVD6      (0x003000)
#define ICT_CMPT_RSVD7      (0x00C000)

#define ICT_CMPT_DONTCARE   (0x000000)
#define ICT_CMPT_SUPPORTS   (0x005555)
#define ICT_CMPT_REQUIRES   (0x00AAAA)
#define ICT_CMPT_INCOMPAT   (0x00FFFF)

#define ICT_ATTR_NPLAYER    (0x030000)
#define ICT_ATTR_NP_1PLAY   (0x000000)
#define ICT_ATTR_NP_2PLAY   (0x010000)
#define ICT_ATTR_NP_1OR2    (0x020000)
#define ICT_ATTR_NP_UPTO4   (0x030000)

#define ICT_ATTR_MULTI      (0x040000)

#define ICT_MEMA_CODE       (0x10)
#define ICT_MEMA_DATA       (0x20)
#define ICT_MEMA_DBDATA     (0x40)
#define ICT_MEMA_STRING     (0x80)

/* ======================================================================== */
/*  Structures for the tag list and for each of the tag types.              */
/* ======================================================================== */
typedef struct icarttag_t       icarttag_t;
typedef struct ict_credits_t    ict_credits_t;
typedef struct ict_infourls_t   ict_infourls_t;
typedef struct ict_bindings_t   ict_bindings_t;
typedef struct ict_symtab_t     ict_symtab_t;
typedef struct ict_memattr_t    ict_memattr_t;
typedef struct ict_linemap_t    ict_linemap_t;
typedef struct ict_extended_t   ict_extended_t;
typedef struct ict_unknown_t    ict_unknown_t;

struct ict_credits_t
{
    int                 count;          /* Number of credits.           */
    int                 alloc;          /* Number of credits allocated  */
    char                **name;         /* Array of names.              */
    uint_32             *flags;         /* Array of flags for each name */
};

struct ict_infourls_t
{
    int                 count;          /* Number of URLs.              */
    int                 alloc;          /* Number of URLs allocated.    */
    char                **desc;         /* Description of URL.          */
    char                **url;          /* The URL (http, mailto, etc.) */
};

struct ict_bindings_t
{
    char                *binding[256];  /* Bindings by key.             */
};

struct ict_symtab_t
{
    PAVLTree            by_name;        /* Symbol table sorted by name. */
    PAVLTree            by_addr;        /* Symbol table sorted by addr. */
};

struct ict_memattr_t
{
    int                 count;          /* Number of ranges             */
    int                 alloc;          /* Number of ranges allocated.  */
    uint_16             *addr_lo;       /* 
    uint_16             *addr_hi;       /* inclusive range */
    uint_8              *flags;
};

struct ict_linemap_t
{
    int                 count;          /* Number of mappings           */
    int                 alloc;          /* Number of mappings alloc'd.  */
    int                 *line_no;
    uint_16             *addr;
};

struct ict_unknown_t
{
    uint_8              *data;
    uint_32             len;
};

struct ict_extended_t
{
    char                *name;
    ict_unknown_t       body;
};


struct icarttag_t
{
    icarttag_t  *next;
    ict_type_t  type;
    union
    {
        char            *title;
        char            *publisher;
        ict_credits_t   *credits;
        ict_infourls_t  *info_urls;
        uint_8          date[3];
        uint_32         game_attr;
        ict_bindings_t  *bindings;
        ict_symtab_t    *sym_tab;
        ict_memattr_t   *mem_attr;
        ict_linemap_t   *line_map;
        ict_extended_t  *extended;
        ict_unknown_t   *unknown;
    } d;
};

/* ======================================================================== */
/*  API functions for reading/writing tags.                                 */
/* ======================================================================== */
icarttag_t  *ict_decode(uint_8 *rom_img, int ignore_crc, int drop_unknown);
uint_8      *ict_gentag(icarttag_t *tags);

/* ======================================================================== */
/*  API functions for querying the tags.                                    */
/* ======================================================================== */
icarttag_t  *ict_findtag        (icarttag_t *tag, ict_type_t type);

char        *ict_get_title      (icarttag_t *tag);
char        *ict_get_publisher  (icarttag_t *tag);
char        *ict_get_credits    (icarttag_t *tag);
char        *ict_get_date       (icarttag_t *tag);   /* Human Readable Date */

int         ict_get_supports    (icarttag_t *tag, uint_32 what);
int         ict_get_requires    (icarttag_t *tag, uint_32 what);
int         ict_get_incompat    (icarttag_t *tag, uint_32 what);

char*       ict_addr2symb       (icarttag_t *tag, uint_32 addr);
uint_32     ict_symb2addr       (icarttag_t *tag, char *symb);

/* ======================================================================== */
/*  API functions for generating tags.                                      */
/* ======================================================================== */
void        ict_set_title       (icarttag_t *tag, char *title);
void        ict_set_publisher   (icarttag_t *tag, char *name);
void        ict_add_credit      (icarttag_t *tag, char *name, uint_8 flags);
void        ict_set_date        (icarttag_t *tag, int yr, int mo, int day);

void        ict_set_supports    (icarttag_t *tag, uint_32 what);
void        ict_set_requires    (icarttag_t *tag, uint_32 what);
void        ict_set_incompat    (icarttag_t *tag, uint_32 what);

int         ict_def_symbol      (icarttag_t *tag, char *symb, uint_32 addr);

/* ======================================================================== */
/*  Useful arrays                                                           */
/* ======================================================================== */
extern char *ict_publisher_names[];
extern char *ict_credit_names[];


#endif
