//---------------------------------------------------------------------------

#ifndef ProgressH
#define ProgressH
//---------------------------------------------------------------------------
void ProgressInit(int Filters);
void ProgressNextFilter(int Steps);
void ProgressDraw();
void ProgressUpdate();
void ProgressUpdate(int Steps);
void ProgressUpdateSet(int Steps);
void ProgressClear();
#endif
