#ifndef __BMP
#define __BMP
AUX_RGBImageRec *LoadBMP(char *Filename);
void TextureAddAlpha(AUX_RGBImageRec* Texture, AUX_RGBImageRec* Alpha);
#endif