// Simple library to load basic bmp textures
#ifdef  _WIN32
    #include    <windows.h>
    #include    <io.h>
#else
    #include    <unistd.h>
    #include    <sys/types.h>
    #define O_BINARY    0
#endif
#pragma	warning (disable:4786)
#include    <GL/gl.h>
#include    <GL/glu.h>
#include    <gl/glut.h>
#include    <gl/glext.h>
#include    <fcntl.h>
#include  	<io.h>
#include    <stdio.h>
#include    <string.h>
#include    <ctype.h>
#include    <math.h>
#include    <malloc.h>
#include  	<memory.h>
#include    <sys/stat.h>
typedef	unsigned char		uint8;
typedef	signed	 char		int8;
typedef	unsigned short		uint16;
typedef	signed	 short		int16;
typedef	unsigned long		uint32;
typedef	signed	 long		int32;
#ifdef	_WIN32
typedef	unsigned __int64	uint64;
typedef	signed	 __int64	int64;
#else
typedef	unsigned long long	uint64;
typedef signed   long long	int64;
#endif
typedef	unsigned char		byte;
typedef	uint16				word;
typedef	uint32				dword;


class	Data
{
	byte  * bits;
	int		length;
	int		pos;

public:
	Data ( const char * fileName );
	Data ( void * ptr, int len )
	{
		bits      = (byte *) ptr;
		length    = len;
		pos       = 0;
	}

	bool	isOk () const;

	bool	isEmpty () const
	{
		return pos >= length;
	}

	int	getLength () const
	{
		return length;
	}

	int	getByte ()
	{
		if ( pos < length )
			return bits [pos++];
		else
			return -1;
	}

	int16	getShort ()
	{
		if ( pos + 1 >= length )
			return -1;

		int16 	v = *(int16 *) (bits + pos);

		pos += 2;

		return v;
	}

	uint16	getUnsignedShort ()
	{
		if ( pos + 1 >= length )
			return -1;

		uint16 v = *(uint16 *) (bits + pos);

		pos += 2;

		return v;
	}

	long getLong ()
	{
		if ( pos + 3 >= length )
			return -1;

		long 	v = *(long *) (bits + pos);

		pos += 4;

		return v;
	}

	dword getUnsignedLong ()
	{
		if ( pos + 3 >= length )
			return -1;

		dword v = *(dword *) (bits + pos);

		pos += 4;

		return v;
	}

	void * getPtr () const
	{
		return bits + pos;
	}

	void * getPtr ( int offs ) const;

	int	seekCur ( int delta )
	{
		pos += delta;

		if ( pos > length )
			pos = length;

		if ( pos < 0 )
			pos = 0;

		return pos;
	}

	int	seekAbs ( int offs )
	{
		pos = offs;

		if ( pos > length )
			pos = length;

		if ( pos < 0 )
			pos = 0;

		return pos;
	}
 
	int		getBytes  ( void * ptr, int len );
};

Data :: Data ( const char * fileName )
{
						// make a fix for windows to replace '/' in file path to 
						// Windoze style '\\' if under windoze
	char * name = strdup ( fileName );

#ifdef	_WIN32
	char * ptr;

	while ( ( ptr = strchr ( name, '/' ) ) != NULL )
		*ptr = '\\';
#endif

	bits      = NULL;
	length    = 0;
	pos       = 0;

	int	file = open ( name, O_RDONLY | O_BINARY );

	free ( name );

	if ( file == -1 )
		return;

#ifndef _WIN32
	struct	stat statBuf;
	
	fstat ( file, &statBuf );
	
	long	len = statBuf.st_size; 
#else	
	long	len = filelength ( file );
#endif

	if ( len == -1 )
	{
		close ( file );

		return;
	}

	bits = (byte *) malloc ( len );

	if ( bits == NULL )
	{
		close ( file );

		return;
	}

	length = read ( file, bits, len );

	close ( file );
}

bool	Data :: isOk () const
{
	return bits != NULL;
}

void * Data :: getPtr ( int offs ) const
{
	if ( offs < 0 || offs >= length )
		return NULL;

	return bits + offs;
}

int	Data :: getBytes ( void * ptr, int len )
{
	if ( pos >= length )
		return -1;

	if ( pos + len > length )
		len = length - pos;

	memcpy ( ptr, bits + pos, len );

	pos += len;

	return len;
}


class   Texture
{
protected:
    int         width;
    int         height;
    int         numComponents;
    int         format;
    byte      * data;
    int         levels;
    bool        mipmapped;
    bool        compressed;

public:
    Texture  ();
    Texture  ( int theWidth, int theHeight, int theNumComponents );
    virtual ~Texture ();

    int getWidth () const
    {
        return width;
    }

    int getHeight () const
    {
        return height;
    }

    int getNumComponents () const
    {
        return numComponents;
    }

    int getFormat () const
    {
        return format;
    }

    int getLevels () const
    {
        return levels;
    }

    bool    isCompressed () const
    {
        return compressed;
    }

    bool    isMipmapped () const
    {
        return mipmapped;
    }

    int getBytesPerLine () const
    {
        return width * numComponents;
    }

    byte * getData () const
    {
        return data;
    }

    void    setFormat ( int newFormat )
    {
        format = newFormat;
    }

    void    generateId ();
    void    putLine    ( int y, dword * bits );

    virtual bool    upload ( int target, bool mipmap = true );
};

inline  dword   makeRgba ( int red, int green, int blue, int alpha = 255 )
{
    return red + (green << 8) + (blue << 16) + (alpha << 24);
}

Texture :: Texture ()
{
    width         = 0;
    height        = 0;
    numComponents = 0;
    data          = NULL;
    levels        = 0;
    compressed    = false;
    mipmapped     = false;
    format        = GL_NONE;
}

Texture :: Texture ( int theWidth, int theHeight, int theNumComponents )
{
    width         = theWidth;
    height        = theHeight;
    numComponents = theNumComponents;
    data          = (byte *) malloc ( width * height * numComponents );
    levels        = 1;
    compressed    = false;
    mipmapped     = false;

    switch ( numComponents )
    {
        case 1:
            format = GL_ALPHA;
            break;

        case 3:
            format = GL_RGB;
            break;

        case 4:
            format = GL_RGBA;
            break;

        default:
            format = -1;
    }
}

Texture :: ~Texture ()
{
    if ( data != NULL )
        free ( data );
}
                                            // store 32-bit RGBA image into texture in a
                                            // specified line
void    Texture :: putLine ( int y, dword * bits )
{
    if ( y < 0 || y >= height )
        return;

    int    offs = y * width * numComponents;
    byte * ptr  = data + offs;

    if ( numComponents == 4 )               // RGBA image
        memcpy ( ptr, bits, 4 * width );
    else
    if ( numComponents == 3 )               // RGB image
    {
        byte * src = (byte *) bits;

        for ( int i = 0; i < width; i++, src += 4 )
        {
            *ptr++ = src [0];
            *ptr++ = src [1];
            *ptr++ = src [2];
        }
    }
    else
    if ( numComponents == 1 )               // greyscale image
    {
        for ( int i = 0; i < width ; i++, bits++ )
            *ptr++ = *(byte *) bits;
    }
}

bool    Texture :: upload ( int target, bool mipmap )
{
    if ( target == GL_TEXTURE_1D )
    {
        if ( mipmap )
            gluBuild1DMipmaps ( target, getNumComponents (), getWidth (),
                                getFormat (), GL_UNSIGNED_BYTE, getData () );
        else
            glTexImage1D      ( target, 0, getNumComponents (), getWidth (), 0,
                                getFormat (), GL_UNSIGNED_BYTE, getData () );
    }
    else
    {
        if ( mipmap )
            gluBuild2DMipmaps ( target, getNumComponents (), getWidth (), getHeight (),
                                getFormat (), GL_UNSIGNED_BYTE, getData () );
        else
            glTexImage2D      ( target, 0, getNumComponents (), getWidth (), getHeight (), 0,
                                getFormat (), GL_UNSIGNED_BYTE, getData () );
    }

    mipmapped = mipmap;

    return true;
}


class	BmpLoader
{
public:
	BmpLoader () {}

	Texture * load ( Data * data );

private:
	bool	loadMono  ( Data * data, Texture * tex, dword * buf );
	bool	loadRGB4  ( Data * data, Texture * tex, dword * but, dword palette [] );
	bool	loadRGB8  ( Data * data, Texture * tex, dword * buf, dword palette [] );
	bool	loadRGB24 ( Data * data, Texture * tex, dword * buf );
	bool	loadRGB32 ( Data * data, Texture * tex, dword * buf );
	bool	loadRLE4  ( Data * data, Texture * tex, dword * buf, dword palette [] );
	bool	loadRLE8  ( Data * data, Texture * tex, dword * buf, dword palette [] );
};

#undef	BI_RGB
#undef	BI_RLE8
#undef	BI_RLE4

#define BI_RGB  0
#define BI_RLE8 1
#define BI_RLE4 2

#pragma pack (push, 1)					// save current pack, set 1-byte packing

										// structs of BMP file
struct  BmpHeader
{
	uint16	type;                   	// type of file, must be 'BM'
	dword	size;                   	// size of file in bytes
    uint16	reserved1, reserved2;
	dword	offBits;                	// offset from this header to actual data
};

struct  BmpInfoHeader
{
	dword	size;                   	// sizeof of this header
	dword	width;                  	// width of bitmap in pixels
	dword	height;                 	// height of bitmap in pixels
	uint16	planes;                 	// # of planes
	uint16	bitCount;               	// bits per pixel
	dword	compression;            	// type of compression, BI_RGB - no compression
	dword	sizeImage;              	// size of image in bytes
	dword	xPelsPerMeter;          	// hor. resolution of the target device
	dword	yPelsPerMeter;          	// vert. resolution
	dword	clrUsed;
	dword	clrImportant;
};

struct  RGBQuad
{
	byte	blue;
	byte	green;
	byte	red;
	byte	reserved;
};

#pragma	pack (pop)

////////////////////////// here go the methods ///////////////////////////////

Texture * BmpLoader :: load ( Data * data )
{
	BmpHeader     * hdr     = (BmpHeader *)     data -> getPtr ();
	BmpInfoHeader * infoHdr = (BmpInfoHeader *) data -> getPtr ( sizeof ( BmpHeader ) );

    bool	errorCode = hdr -> type == 0x4D42 && infoHdr -> size == 40 && infoHdr -> planes == 1 &&
                        (infoHdr -> bitCount == 1 || infoHdr -> bitCount == 2 ||
                        infoHdr -> bitCount == 4 || infoHdr -> bitCount == 8 || infoHdr -> bitCount == 24 || infoHdr -> bitCount == 32) &&
                        (infoHdr -> compression == BI_RGB || infoHdr -> compression == BI_RLE4 || infoHdr -> compression == BI_RLE8);

	if ( !errorCode )                    // bad header buf
        return NULL;

    int	numColors = 1 << infoHdr -> bitCount;
    int	width     = infoHdr -> width;
    int	height    = infoHdr -> height;

    RGBQuad * pal = (RGBQuad *)(infoHdr + 1);
	dword	  palette [256];
	int		  numComponents = 3;

	if ( infoHdr -> bitCount == 32 )
		numComponents = 4;

	if ( infoHdr -> bitCount <= 8 )
		for ( int i = 0; i < numColors; i++ )
			palette [i] = makeRgba ( pal [i].red, pal [i].green, pal [i].blue );

	Texture * tex = new Texture ( width, height, numComponents );

    if ( tex == NULL )
		return NULL;

	dword * buf = new dword [width];

	data -> seekAbs ( hdr -> offBits );

    if ( infoHdr -> compression == BI_RGB )
    {
		if ( infoHdr -> bitCount == 1 )		// mono
			errorCode = loadMono ( data, tex, buf );
		else
        if ( infoHdr -> bitCount == 4 )		// 16-colors uncompressed
			errorCode = loadRGB4 ( data, tex, buf, palette );
        else
        if ( infoHdr -> bitCount == 8 )		// 256-colors uncompressed
			errorCode = loadRGB8 ( data, tex, buf, palette );
		else
        if ( infoHdr -> bitCount == 24 )	// True-Color bitmap
            errorCode = loadRGB24 ( data, tex, buf );
		else
		if ( infoHdr -> bitCount == 32 )	// true-color bitmap with alpha-channel
			 errorCode = loadRGB32 ( data, tex, buf );
    }
    else
    if ( infoHdr -> compression == BI_RLE4 )// 16-colors RLE compressed
        errorCode = loadRLE4 ( data, tex, buf, palette );
    else
    if ( infoHdr -> compression == BI_RLE8 )// 256-colors RLE compressed
        errorCode = loadRLE8 ( data, tex, buf, palette );
    else
		errorCode = false;

	delete buf;

    if ( !errorCode )
    {
        delete tex;

        return NULL;
    }

    return tex;
}

bool	BmpLoader :: loadMono ( Data * data, Texture * tex, dword * buf )
{
	int	   width  = tex -> getWidth  ();
	int	   height = tex -> getHeight ();
	byte * ptr    = new byte [width];				// sure it's enough

	int		bytesPerLine = (width + 7) / 8;
	int		delta        = bytesPerLine & 3;		// do DWORD alignment

	if ( delta != 0 )
		bytesPerLine += 4 - delta;

    for ( int y = height - 1; y >= 0; y-- )
    {
		if ( data -> getBytes ( ptr, bytesPerLine ) != bytesPerLine )
		{
			delete ptr;

			return false;
		}

		memset ( buf, '\0', sizeof ( dword ) * width );

		for ( int x = 0; x < width; x++ )
			if ( ptr [x >> 3] & (0x80 >> (x & 7) ) )
				buf [x] = makeRgba ( 255, 255, 255 );
			else
				buf [x] = makeRgba ( 0, 0, 0 );
    }

	delete ptr;

    return true;
}


bool	BmpLoader :: loadRGB4 ( Data * data, Texture * tex, dword * buf, dword palette [] )
{
	int	width  = tex -> getWidth  ();
	int	height = tex -> getHeight ();
	int	count, byte;

    for ( int y = height - 1; y >= 0; y-- )
    {
		memset ( buf, '\0', sizeof ( dword ) * width );

		for ( int x = 0; x < width; x++ )
			if ( ( x & 1 ) == 0 )
				if ( ( byte = data -> getByte () ) == -1 )
					return false;
                else
                    buf [x] = palette [byte >> 4];
            else
                buf [x] = palette [byte & 0x0F];

        for ( count = ( width + 1 ) / 2; count % 4; count++ )
			if ( data -> getByte () == -1 )
				return false;

		tex -> putLine ( y, buf );
    }

    return true;
}

bool	BmpLoader :: loadRGB8 ( Data * data, Texture * tex, dword * buf, dword palette [] )
{
	int	width  = tex -> getWidth  ();
	int	height = tex -> getHeight ();
	int	count, byte;

    for ( int y = height - 1; y >= 0; y-- )
    {
		memset ( buf, '\0', sizeof ( dword ) * width );

		for ( int x = count = 0; x < width; x++, count++ )
			if ( ( byte = data -> getByte () ) == -1 )
				return false;
            else
                buf [x] = palette [byte];

        for ( ; count % 4; count++ )    // skip remaining bytes
			if ( data -> getByte () == -1 )
				return false;

		tex -> putLine ( y, buf );
    }

    return true;
}

bool	BmpLoader :: loadRGB24 ( Data * data, Texture * tex, dword * buf )
{
	int	width  = tex -> getWidth  ();
	int	height = tex -> getHeight ();
	int	count;
    int	red, green, blue;

    for ( int y = height - 1; y >= 0; y-- )
    {
		memset ( buf, '\0', sizeof ( dword ) * width );

		for ( int x = count = 0; x < width; x++, count += 3 )
        {
			if ( ( blue = data -> getByte () ) == -1 )
				return false;

            if ( ( green = data -> getByte () ) == -1 )
                return false;

            if ( ( red = data -> getByte () ) == -1 )
                return false;

            buf [x] = makeRgba ( red, green, blue );
        }

        for ( ; count % 4; count++ )    // skip remaining bytes
			if ( data -> getByte () == -1 )
				return false;

		tex -> putLine ( y, buf );
    }

    return true;
}

bool	BmpLoader :: loadRGB32 ( Data * data, Texture * tex, dword * buf )
{
	int	width  = tex -> getWidth  ();
	int	height = tex -> getHeight ();
    int	red, green, blue, alpha;

    for ( int y = height - 1; y >= 0; y-- )
    {
		memset ( buf, '\0', sizeof ( dword ) * width );

		for ( int x = 0; x < width; x++ )
        {
			if ( ( blue = data -> getByte () ) == -1 )
				return false;

            if ( ( green = data -> getByte () ) == -1 )
                return false;

            if ( ( red = data -> getByte () ) == -1 )
                return false;

			if ( ( alpha = data -> getByte () ) == -1 )
				return false;

            buf [x] = makeRgba ( red, green, blue, alpha );
        }

		tex -> putLine ( y, buf );
    }

    return true;
}

bool	BmpLoader :: loadRLE4 ( Data * data, Texture * tex, dword * buf, dword palette [] )
{
	int	width  = tex -> getWidth  ();
	int	height = tex -> getHeight ();
	int	y      = height - 1;
    int x      = 0;
    int count, byte;

	memset ( buf, '\0', sizeof ( dword ) * width );

    for ( ; ; )
    {
		if ( ( count = data -> getByte () ) == -1 )
			return false;
        else
        if ( count == 0 )
        {
			if ( ( count = data -> getByte () ) == -1 )
				return false;
            else
            if ( count == 0 )       // end of line
            {
				tex -> putLine ( y, buf );

				memset ( buf, '\0', sizeof ( dword ) * width );

				y--;
                x = 0;
            }
            else
            if ( count == 1 )       // 0, 1 == end of RLE buf
				break;
            else
            if ( count == 2 )       // 0, 2 == delta record
            {
				tex -> putLine ( y, buf );

				memset ( buf, '\0', sizeof ( dword ) * width );

				y -= data -> getByte ();
                x += data -> getByte ();
            }
            else                    // start of an unencoded block
            {
				for ( int i = 0; i < count; i += 2, x += 2 )
                {
					if ( ( byte = data -> getByte () ) == -1 )
						return false;

                    buf [x]     = palette [byte >> 4];
                    buf [x + 1] = palette [byte & 0x0F];
                }

                if ( ( count / 2 ) & 1 )
					if ( data -> getByte () == -1 )
						return false;
            }
        }
        else                    // RLE decoded record
        {
			if ( ( byte = data -> getByte () ) == -1 )
				return false;

            for ( int i = 0; i < count; i++, x++ )
				if ( i & 1 )
					buf [x] = palette [byte & 0x0F];
                else
					buf [x] = palette [byte >> 4];
        }
    }

    return true;
}

bool	BmpLoader :: loadRLE8 ( Data * data, Texture * tex, dword * buf, dword palette [] )
{
	int	width  = tex -> getWidth  ();
	int	height = tex -> getHeight ();
	int	y      = height - 1;
    int x      = 0;
	int count, byte;

	memset ( buf, '\0', sizeof ( dword ) * width );

    for ( ; ; )
    {
		if ( ( count = data -> getByte () ) == -1 )
			return false;
        else
        if ( count == 0 )
        {
			if ( ( count = data -> getByte () ) == -1 )
				return false;
            else
            if ( count == 0 )       // end of line
            {
				tex -> putLine ( y, buf );

				memset ( buf, '\0', sizeof ( dword ) * width );

				y--;
                x = 0;
            }
            else
            if ( count == 1 )       // 0, 1 == end of RLE buf
				break;
            else
            if ( count == 2 )       // 0, 2 == delta record
            {
				tex -> putLine ( y, buf );

				memset ( buf, '\0', sizeof ( dword ) * width );

				y -= data -> getByte ();
                x += data -> getByte ();
            }
            else                    // start of an unencoded block
            {
				for ( int i = 0; i < count; i++, x ++ )
					if ( ( byte = data -> getByte () ) == -1 )
						return false;
                    else
						buf [x] = palette [byte];

                if ( count & 1 )
					if ( data -> getByte () == -1 )
						return false;
            }
        }
        else                    // RLE decoded record
        {
			if ( ( byte = data -> getByte () ) == -1 )
				return false;

            for ( int i = 0; i < count; i++, x++ )
				buf [x] = palette [byte];
        }
	}

    return true;
}

bool    loadTexture    ( int target, bool mipmap, const char * fileName );

bool    fileExist ( const char * fileName )
{
#ifdef  _WIN32
    struct _stat    buf;
    return _stat ( fileName, &buf ) != -1;
#else
    struct stat buf;
    return stat ( fileName, &buf ) != -1;
#endif
}

Data * getFile ( const char * fileName )
{
    if ( fileExist ( fileName ) )
        return new Data ( fileName );
    return NULL;
}

unsigned    createTexture2D ( bool mipmap, const char * fileName )
{
    unsigned    textureId;
    glGenTextures   ( 1, &textureId );
    glBindTexture   ( GL_TEXTURE_2D, textureId );
    glPixelStorei   ( GL_UNPACK_ALIGNMENT, 1 );                         // set 1-byte alignment
    glTexParameteri ( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );    // set texture to repeat mode
    glTexParameteri ( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
    if ( !loadTexture ( GL_TEXTURE_2D, mipmap, fileName ) )
        return 0;
    glTexParameteri ( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    return textureId;
}

#ifndef _WIN32
    #define stricmp strcasecmp
#endif
Texture * getTexture ( const char * fileName )
{
    const char * ext = strrchr ( fileName, '.' );
    if ( ext == NULL )
        return NULL;
    Data    * data = getFile ( fileName );
    Texture * tex = NULL;
    if ( data == NULL || data -> getLength () < 1 )
    {
        fprintf ( stderr, "Cannot load %s\n", fileName );
        return NULL;
    }
    if ( !stricmp ( ext + 1, "bmp" ) )
    {
        BmpLoader   loader;
        tex = loader.load ( data );
    }
    if ( tex == NULL )
        fprintf ( stderr, "No loader for %s !!!\n", fileName );
    delete data;
    return tex;
}

bool    loadTexture ( int target, bool mipmap, const char * fileName )
{
    Texture * texture = getTexture ( fileName );
    if ( texture == NULL )
        return false;
    texture -> upload ( target, mipmap );
    if ( texture -> isMipmapped () )
        glTexParameteri ( target, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR );
    else
        glTexParameteri ( target, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    delete texture;
    return true;
}

