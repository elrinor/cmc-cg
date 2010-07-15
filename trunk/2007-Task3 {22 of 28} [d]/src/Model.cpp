#include "Model.h"

using namespace std;

void ModelPart::draw() 
{
	glBindTexture(GL_TEXTURE_2D, this->textureId);
	glEnableClientState(GL_NORMAL_ARRAY);
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
	glEnableClientState(GL_VERTEX_ARRAY);
	glVertexPointer(3, GL_FLOAT, 0, this->points);
	glNormalPointer(GL_FLOAT, 0, this->normals);
	glTexCoordPointer(3, GL_FLOAT, 0, this->texCoords);
	glDrawArrays(GL_TRIANGLES, 0, this->size / 3);
}

ModelPart::ModelPart(GLuint textureId, int maxSize) 
{
	this->textureId = textureId;
	this->maxSize = maxSize;
	this->points = new float[maxSize];
	this->normals = new float[maxSize];
	this->texCoords = new float[maxSize];
	this->size = 0;
}

void ModelPart::reAlloc() 
{
	float *newPoints    = new float[this->maxSize * 2];
	float *newNormals   = new float[this->maxSize * 2];
	float *newTexCoords = new float[this->maxSize * 2];
	memcpy(newPoints,    this->points,    sizeof(float) * this->maxSize);
	memcpy(newNormals,   this->normals,   sizeof(float) * this->maxSize);
	memcpy(newTexCoords, this->texCoords, sizeof(float) * this->maxSize);
	delete[] this->points;
	delete[] this->normals;
	delete[] this->texCoords;
	this->points    = newPoints;
	this->normals   = newNormals;
	this->texCoords = newTexCoords;
	this->maxSize *= 2;
}

void ModelPart::add(Vector3d point, Vector3d normal, Vector3d texCoord) 
{
	if (this->maxSize - this->size < 3)
		this->reAlloc();
	this->points[this->size + 0] = point.x;
	this->points[this->size + 1] = point.y;
	this->points[this->size + 2] = point.z;

	this->normals[this->size + 0] = normal.x;
	this->normals[this->size + 1] = normal.y;
	this->normals[this->size + 2] = normal.z;

	this->texCoords[this->size + 0] = texCoord.x;
	this->texCoords[this->size + 1] = texCoord.y;
	this->texCoords[this->size + 2] = texCoord.z;

	this->size += 3;
}

ModelPart::~ModelPart() 
{
	delete[] this->points;
	delete[] this->normals;
	delete[] this->texCoords;
}

void Model::draw() 
{
	for (unsigned int i = 0; i < this->parts.size(); i++)
		this->parts[i]->draw();
}

void Model::add(ModelPart* part) 
{
	this->parts.push_back(part);
}

Model::~Model() 
{
	for (unsigned int i = 0; i < this->parts.size(); i++)
		delete this->parts[i];
}
