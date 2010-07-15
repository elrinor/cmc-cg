#ifndef __TIMER_H
#include"timer.h"
#endif

#include<windows.h>
#include<mmsystem.h>

Timer SystemTimer;

Timer::Timer()
{
    LARGE_INTEGER freq;
    if(QueryPerformanceFrequency(&freq)) { 
        frequency = 4294967296.0*freq.HighPart+freq.LowPart;
        using_mm = 0;
    } else { // Используем MultiMedia таймер (на старых OS)
        frequency = 1000;
        using_mm = 1;
    }
}

double Timer::GetTime() const
{
    if(using_mm) 
        return timeGetTime();
    else {
        LARGE_INTEGER count;
        QueryPerformanceCounter(&count); 
        return 4294967296.0*count.HighPart+count.LowPart;      
    }
}