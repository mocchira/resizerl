#ifndef __ERL_COMM__
#define __ERL_COMM__

typedef unsigned char byte;

#define SWAP_ENDIAN_INT32(num) (((num>>24) & 0xff)     | \
                                ((num<<8)  & 0xff0000) | \
                                ((num>>8)  & 0xff00)   | \
                                ((num<<24) & 0xff000000));

int read_cmd(byte **buf, int *size);
int write_cmd(const byte *buf, int len);
int read_exact(byte *buf, int len);
int write_exact(const byte *buf, int len);

#endif
