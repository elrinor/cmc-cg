#ifndef __BMPLOAD_H

#define __BMPLOAD_H

unsigned char *LoadIndexedBMPFile(const char *path,int *width,int *height);
unsigned char *LoadTrueColorBMPFile(const char *path,int *width,int *height);

#endif