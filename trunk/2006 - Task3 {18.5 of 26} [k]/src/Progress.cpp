//---------------------------------------------------------------------------


#pragma hdrstop

#include "Progress.h"
#include "mainform.h"
#include <windows.hpp>

//---------------------------------------------------------------------------
int FiltersTotal, FiltersDone;
int Percent;
int FilterStepsTotal, FilterStepsDone;
int LastTickCount=0;

void ProgressInit(int Filters)
{
  Form1->CGauge1->Progress = 0;
  FiltersTotal = Filters;
  FiltersDone = -1;
  Percent = 0;
  Form1->InProgress = true;
  Form1->RePaint();
}

void ProgressNextFilter(int Steps)
{
  FilterStepsTotal = Steps;
  FilterStepsDone = 0;
  FiltersDone++;
}

void ProgressDraw()
{
  Form1->CGauge1->Progress = Percent;
}

void ProgressUpdate(int Steps)
{
  FilterStepsDone += Steps;
  int NewPercent = 100 * (1.0 * FiltersDone / FiltersTotal + 1.0 * FilterStepsDone / (FilterStepsTotal * FiltersTotal));
  if (NewPercent != Percent)
  {
    Percent = NewPercent;
    ProgressDraw();
  }
  int TickCount = GetTickCount();
  if (NewPercent != Percent || TickCount - LastTickCount > 250)
    Application->ProcessMessages();
}

void ProgressUpdate()
{
  ProgressUpdate(1);
}

void ProgressUpdateSet(int Steps)
{
  FilterStepsDone = Steps;
  ProgressUpdate(0);
}

void ProgressClear()
{
  Percent = 0;
  Form1->CGauge1->Progress = 0;
  Form1->InProgress = false;
}

#pragma package(smart_init)
