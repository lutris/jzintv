#ifndef EMITTERS_H_
#define EMITTERS_H_ 1

void emit_listing_mode(listing_mode m);
void emit_comment(int is_user, const char *format, ...);
void emit_set_equ(unsigned int value);
void emit_location(int seg, int loc, int type, const char *mode);
void emit_mark_with_mode(int lo, int hi, const char *mode);
void emit_reserve(int len);
void emit_entering_file(const char *name);
void emit_exiting_file (const char *name);
void emit_warnerr(const char *file, int line, warnerr type,
                  const char *format, ...);
void emit_raw_error(const char *raw_error);
void emit_listed_line(const char *buf);
void emit_unlisted_line(void);
void emit_generated_instr(const char *buf);


void intermed_start_pass_1(void);
void intermed_start_pass_2(void);
void intermed_finish(int debugmode);


irec_union *pass2_next_rec(void);
void        pass2_release_rec(irec_union *irec);

#endif
