#ifndef __FPS_COUNTER_H

#define __FPS_COUNTER_H

#include<windows.h>
#include<gl/gl.h>
#include<gl/glut.h>

#include"Timer.h"

class FPSCounter {
protected:
	int frameCount;
	double prevTime;    
	double updateInterval;
	double fps;
	bool isUpdated;
	bool isInited;
public:
	FPSCounter(double updateInterval = 1.0); // Период обновления счетчика (в секундах)
	virtual ~FPSCounter()
		{ }
	virtual void NextFrame();
	double GetFPS() const
		{ return fps; }
	bool IsUpdated() const
		{ return isUpdated; }
};

const int MaxFPSMessageLength = 80;

class FPSDisplay : public FPSCounter {
protected:
	char formatString[MaxFPSMessageLength + 1];
	char text[MaxFPSMessageLength + 1];
public:
	FPSDisplay(const char *fmt = NULL, double updatePeriod = 1.0);
	virtual void NextFrame();
	virtual void Paint()
		{ }
	const char *GetDisplayText() const
		{ return text; }
};

class GlutFPSDisplay : public FPSDisplay {
protected:
	GLdouble r,g,b; // Цвет текста
	int fontSize; // Размер текста в пикселях
public:
	GlutFPSDisplay(const char *formatString = NULL, double updateInterval = 1.0,
		GLdouble r = 0, GLdouble g = 1, GLdouble b = 0, int fontSize = 24) : 
	  FPSDisplay(formatString, updateInterval),r(r),g(g),b(b),fontSize(fontSize)
	    { }
	virtual void Paint();
};

#endif // __FPS_COUNTER_H