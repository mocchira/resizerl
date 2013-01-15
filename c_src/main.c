#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "erl_interface.h"
#include "ei.h"

#include <opencv2/core/core_c.h>
#include <opencv2/core/types_c.h>
#include <opencv2/imgproc/imgproc_c.h>
#include <opencv2/highgui/highgui_c.h>

#include "erl_comm.h"

#define INIT_BUF_SIZE (1024 * 1024)

// FIXME: error handling
static void opencv_exec(unsigned char* src, int src_size, unsigned char* path, int path_size, int w, int h, /*out*/ void** descriptor) {
  char ext[256]; // NULL terminated cstr
  int ext_pos = (sizeof(ext) < (path_size+1)) ? path_size+1 - sizeof(ext) : 0;
  memcpy(ext, path, path_size - ext_pos);
  ext[path_size - ext_pos] = 0;
  CvMat tmp = cvMat(16384, 16384, CV_8UC4, (void*)src);
  CvMat* src_img = cvDecodeImageM(&tmp, CV_LOAD_IMAGE_COLOR);
  CvMat dst_img = cvMat(h, w, src_img->type, NULL);
  cvCreateData(&dst_img);
  cvResize(src_img, &dst_img, CV_INTER_AREA); // FIXME: using CV_INTER_AREA only if got scaled down
  CvMat* enc_img = cvEncodeImage(ext, &dst_img, 0);
//out:
  cvReleaseMat(&src_img);
  cvReleaseData(&dst_img);
  *descriptor = (void*)enc_img;
}

static void opencv_get_result(void* descriptor, unsigned char** dst, int* dst_size) {
  CvMat* enc_img = (CvMat*)descriptor;
  *dst_size = enc_img->rows * enc_img->cols * CV_ELEM_SIZE(enc_img->type);
  *dst = enc_img->data.ptr;
}

static void opencv_release(void** descriptor) {
  cvReleaseMat((CvMat**)descriptor);
}

typedef struct {
  void (*exec)(unsigned char*, int, unsigned char*, int, int, int, void**);
  void (*get_result)(void*, unsigned char**, int*);
  void (*release)(void**);
} resizerl_handler;

static resizerl_handler resizerl_opencv = {
  opencv_exec,
  opencv_get_result,
  opencv_release
};

static resizerl_handler* resizerl_impls[] = {
  &resizerl_opencv,
  NULL
};
 
int main(int argc, char* argv[]) {
  ETERM *tuplep, *fnp, *pathp, *binp, *widthp, *heightp;
  ETERM *res_tuplep;
  ETERM *res_arrp[2];
  ETERM *res_ok_atomp, *res_error_atomp, *res_error_cause_atomp;

  int idx = 1;
  byte *buf = malloc(INIT_BUF_SIZE);
  int buf_len = INIT_BUF_SIZE;
  int res_len;

  erl_init(NULL, 0);

  //Create common atoms(ok, error)
  res_ok_atomp = erl_mk_atom("ok");
  res_error_atomp = erl_mk_atom("error");
  res_error_cause_atomp = erl_mk_atom("resize failed");

  while (read_cmd(&buf, &buf_len) > 0) {
    tuplep = erl_decode(buf);
    fnp     = erl_element(idx++, tuplep);
    pathp   = erl_element(idx++, tuplep);
    binp    = erl_element(idx++, tuplep);
    widthp  = erl_element(idx++, tuplep);
    heightp = erl_element(idx++, tuplep);
    
    if (strncmp(ERL_ATOM_PTR(fnp), "resize", 6) == 0) {
      char* algop = ERL_ATOM_PTR(fnp) + 6;
      resizerl_handler* rhp = (strncmp(algop, "_opencv", 7) == 0) ? resizerl_impls[0] : NULL;
      if (rhp == NULL) {
        exit(1);
      }
      // ERL_INT_VALUE, ERL_BIN_PTR, ERL_BIN_SIZE
      void* dp = NULL;
      rhp->exec(ERL_BIN_PTR(binp),
                ERL_BIN_SIZE(binp),
                ERL_BIN_PTR(pathp),
                ERL_BIN_SIZE(pathp),
                ERL_INT_VALUE(widthp),
                ERL_INT_VALUE(heightp), &dp);
      if (dp == NULL) {
        exit(2);
      }
      unsigned char* dst;
      int dst_size;
      rhp->get_result(dp, &dst, &dst_size);
      res_arrp[0] = res_ok_atomp;
      res_arrp[1] = erl_mk_binary((const char*)dst, dst_size);
      res_tuplep = erl_mk_tuple(res_arrp, 2);
      res_len = erl_term_len(res_tuplep);
      if (res_len > buf_len) {
        byte* new_buf = (byte*)realloc((void*)buf, res_len);
        if (new_buf == NULL) {
          exit(3);
        }
        buf = new_buf;
        buf_len = res_len;
      }
      erl_encode(res_tuplep, buf);
      write_cmd(buf, erl_term_len(res_tuplep));
      erl_free_term(res_arrp[1]);
      erl_free_term(res_tuplep);
      rhp->release(&dp);
    } else {
      res_arrp[0] = res_error_atomp;
      res_arrp[1] = res_error_cause_atomp;
      res_tuplep = erl_mk_tuple(res_arrp, 2);
      if (res_len > buf_len) {
        byte* new_buf = (byte*)realloc((void*)buf, res_len);
        if (new_buf == NULL) {
          exit(4);
        }
        buf = new_buf;
        buf_len = res_len;
      }
      erl_encode(res_tuplep, buf);
      write_cmd(buf, erl_term_len(res_tuplep));
      erl_free_term(res_tuplep);
    }

    erl_free_compound(tuplep);
    erl_free_term(fnp);
    erl_free_term(pathp);
    erl_free_term(binp);
    erl_free_term(widthp);
    erl_free_term(heightp);
    idx = 1;
  }
  // normal
  return 0;

}
