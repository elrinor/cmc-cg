#ifndef _LOADBMP_H
#define _LOADBMP_H

#ifdef __cplusplus
extern "C" {
#endif


typedef struct _IMAGE
  {
  int width;
  int height;
  unsigned char* data;
  } IMAGE;

int LoadBMP(const char* file, IMAGE* out_img);

#ifdef __cplusplus
}
#endif


#endif