#include <gl\glaux.h>
#include <stdio.h>
#include <gl\glaux.h>

AUX_RGBImageRec *LoadBMP(char *Filename)			
{
	FILE *File=NULL;								
	if (!Filename)									
		return NULL;								
  File=fopen(Filename,"r");						
	if (File)										
	{
		fclose(File);								
		return auxDIBImageLoad(Filename);			
	}
	return NULL;									
}

void TextureAddAlpha(AUX_RGBImageRec* Texture, AUX_RGBImageRec* Alpha)
{
	unsigned char *result = new unsigned char [Texture->sizeX*Texture->sizeY*4];
	for(int i = 0;i<Texture->sizeX*Texture->sizeY;i++) 
  {
	  result[4*i] = Texture->data[3*i];
		result[4*i + 1] = Texture->data[3*i + 1];
		result[4*i + 2] = Texture->data[3*i + 2];
		result[4*i + 3] = (unsigned char)(((int)Alpha->data[3*i] + (int)Alpha->data[3*i + 1] + (int)Alpha->data[3*i + 2])/3);
	}
  delete[] Texture->data;
  Texture->data=result;
}
