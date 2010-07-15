#ifndef	__FORMATS_H

#define	__FORMATS_H

// BMP file format definition

#pragma pack(1)

struct BMPHeader {
	short type;           // File type = 0x4D42
	int size;	 	
	short reserved1;
	short reserved2;
	int offset;           // Offset from file start to bitmap data
};

struct BMPInfoHeader {
	int size;             // Size of this structure in bytes
	int width;
	int height;
	short planes;         // Should be equal to 1
	short bitsPerPixel;
	unsigned compression; // Compression flags ( 0 - no compression )
	unsigned imageSize;   // Size of image in bytes
	int xPelsPerMeter;		
	int yPelsPerMeter;	
	int clrUsed;
	int clrImportant;
};

struct BMPPaletteItem {
	unsigned char blue;
	unsigned char green;
	unsigned char red;
	unsigned char unused;
};

#pragma pack()

#endif
