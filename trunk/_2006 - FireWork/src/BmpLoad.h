#ifndef __BMPLOAD_H

#define __BMPLOAD_H

unsigned char *LoadIndexedBMPFile(const char *path,int *width,int *height);
unsigned char *LoadTrueColorBMPFile(const char *path,int *width,int *height);
unsigned char *ConstructTexture(int *w,int *h, char*PathTo24bitTexture, char*PathTo24bitAlphaChannel);

#endif