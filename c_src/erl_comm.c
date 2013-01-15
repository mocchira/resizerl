#include <unistd.h>
#include <sys/types.h>
#include <stdlib.h>
#include <errno.h>

#include "erl_comm.h"

int read_cmd(byte **buf, int *size)
{
  int len;

  if (read_exact(*buf, 4) != 4)
    return(-1);

  len = ((*buf)[0] << 24) | \
        ((*buf)[1] << 16) | \
        ((*buf)[2] << 8)  | \
         (*buf)[3];

  if (len > *size) {
    byte* tmp = (byte*)realloc((void*)*buf, len);
    if (tmp == NULL)
      return -1;
    else
      *buf = tmp;
    *size = len;
  }
  return read_exact(*buf, len);
}

int write_cmd(const byte *buf, int len)
{
  int big_len = SWAP_ENDIAN_INT32(len);

  write_exact((byte*)&big_len, 4);

  return write_exact(buf, len);
}

int read_exact(byte *buf, int len)
{
  int i, got=0;

  do {
    if ((i = read(0, buf+got, len-got)) <= 0) {
      if (errno == EINTR) {
        continue;
      } else {
        return i;
      }
    }
    got += i;
  } while (got<len);

  return len;
}

int write_exact(const byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0) {
      if (errno == EINTR) {
        continue;
      } else {
        return i;
      }
    }
    wrote += i;
  } while (wrote<len);

  return len;
}
