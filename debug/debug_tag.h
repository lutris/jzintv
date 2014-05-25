#ifndef DEBUG_TAG_H_
#define DEBUG_TAG_H_

#define DEBUG_MA_DATA  (1 << 0)
#define DEBUG_MA_SDBD  (1 << 1)
#define DEBUG_MA_CODE  (1 << 2)
#define DEBUG_MA_WRITE (1 << 3)

void debug_tag_range(uint_32 lo, uint_32 hi, uint_32 flag);

#endif
