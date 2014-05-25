/* ======================================================================== */
/*  Registry of opaque types.  This is here to reduce the dependences       */
/*  among all the header files, since many structs and prototypes need      */
/*  an opaque type declaration without the actual struct definition.        */
/* ======================================================================== */

#ifndef OPAQUE_H_
#define OPAQUE_H_


/* ay8910 */
typedef struct ay8910_t             ay8910_t;


/* bincfg */
typedef struct bc_memspan_t         bc_memspan_t;
typedef struct bc_mac_addr_t        bc_mac_addr_t;
typedef struct bc_mac_load_t        bc_mac_load_t;
typedef struct bc_mac_poke_t        bc_mac_poke_t;
typedef struct bc_mac_reg_t         bc_mac_reg_t;
typedef struct bc_mac_watch_t       bc_mac_watch_t;
typedef enum   bc_macro_cmd_t       bc_macro_cmd_t;
typedef struct bc_macro_t           bc_macro_t;
typedef struct bc_strnum_t          bc_strnum_t;
typedef struct bc_var_t             bc_var_t;
typedef struct bc_varlike_types_t   bc_varlike_types_t;
typedef struct bc_varlike_t         bc_varlike_t;
typedef struct bc_diagtype_t        bc_diagtype_t;
typedef struct bc_diag_t            bc_diag_t;
typedef struct bc_cfgfile_t         bc_cfgfile_t;
typedef struct legacy_t             legacy_t;
                                   
                                   
/* cfg */                          
typedef struct cfg_evt_t            cfg_evt_t;
typedef struct cfg_kbd_t            cfg_kbd_t;
typedef struct cfg_t                cfg_t;
                                   
                                   
typedef struct cp1600_t             cp1600_t;
typedef struct cp1600_t             *cp1600_p;
typedef struct instr_t              *instr_p;
typedef const struct instr_t        *instr_kp;
                                   
typedef struct op_impl_1op_a        op_impl_1op_a;
typedef struct op_impl_1op_b        op_impl_1op_b;
typedef struct op_jump              op_jump;
typedef struct op_reg_1op           op_reg_1op;
typedef struct op_gswd              op_gswd;
typedef struct op_nop_sin           op_nop_sin;
typedef struct op_rot_1op           op_rot_1op;
typedef struct op_reg_2op           op_reg_2op;
typedef struct op_cond_br           op_cond_br;
typedef struct op_dir_2op           op_dir_2op;
typedef struct op_ind_2op           op_ind_2op;
typedef struct op_imm_2op           op_imm_2op;
typedef struct op_decoded           op_decoded;
typedef enum   instr_fmt_t          instr_fmt_t;
typedef union  opcode_t             opcode_t;
typedef struct instr_t              instr_t;
                                   
typedef struct req_bus_t            req_bus_t;
typedef struct debug_t              debug_t;

/* debug */
typedef struct debug_t              debug_t;

/* demo */
typedef struct demo_t               demo_t;

/* event */
typedef struct event_name_t         event_name_t;
typedef struct event_mask_t         event_mask_t;
typedef struct event_t              event_t;

/* gfx */
typedef struct gfx_pvt_t            *gfx_pvt_p;
typedef struct gfx_t                gfx_t;

/* gif */
typedef struct gif_t                gif_t;

/* icart */
typedef struct icart_t              icart_t;
typedef struct icartrom_t           icartrom_t;

/* ivoice */
typedef struct lpc12_t              lpc12_t;
typedef struct ivoice_t             ivoice_t;

/* mem */
typedef struct mem_t                mem_t;

/* misc */
typedef struct tAVLNode             *PAVLNode;
typedef struct tAVLNode             TAVLNode;
typedef PAVLNode                    *PPAVLNode;
typedef struct tAVLTree             TAVLTree;
typedef struct tAVLTree             *PAVLTree;
typedef struct ll_t                 ll_t;


/* mvi */
typedef struct mvi_t                mvi_t;

/* pads */
typedef struct pad_t                pad_t;
typedef struct pad_cgc_t            pad_cgc_t;
typedef struct pad_intv2pc_t        pad_intv2pc_t;

/* periph */
typedef struct periph_t             *periph_p;
typedef struct periph_t             periph_t;
typedef struct periph_bus_t         periph_bus_t;

/* snd */
typedef struct snd_buf_t            snd_buf_t;
typedef struct snd_t                snd_t;

/* speed */
typedef struct speed_t              speed_t;

/* stic */
typedef struct stic_t               stic_t;


#endif
