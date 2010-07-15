#ifndef __MODEL
#define __MODEL

typedef struct {
  int VCount;
  int type;
  float* vertex;
  float* normal;
  float* texcoord;
} TModel;

void DrawModel(TModel* model);
void LoadModel(char *FileName, TModel* model);

#endif