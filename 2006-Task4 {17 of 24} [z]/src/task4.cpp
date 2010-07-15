#define _USE_MATH_DEFINES
#include <math.h>
#include <windows.h>
#include <gl/glut.h>
#include "loadbmp.h"

inline double sqr(double x) {return x*x;}

const int MAX=10000;
const double pi=M_PI;
const double CAM_R = 2.5;

struct Point {
	double x,y,z;
	Point(double xx=0.0,double yy=0.0,double zz=0.0) {x=xx;y=yy;z=zz;}
	Point(int type,double u,double v) {
		switch (type) {
			case 0:
				x=(1+(v/2.0)*cos(u/2.0))*cos(u);
				y=(1+(v/2.0)*cos(u/2.0))*sin(u);
				z=v/2.0*sin(u/2.0);
				break;
			case 1:
				if (u<pi) {
					x=0.08*(6*cos(u)*(1+sin(u))+4*(1-0.5*cos(u))*cos(u)*cos(v));
					y=0.08*(16*sin(u)+4*(1-0.5*cos(u))*sin(u)*cos(v));
					z=0.08*(4*(1-0.5*cos(u))*sin(v));
				} else {
					x=0.08*(6*cos(u)*(1+sin(u))-4*(1-0.5*cos(u))*cos(v));
					y=0.08*(16*sin(u));
					z=0.08*(4*(1-0.5*cos(u))*sin(v));
				}
				break;
			case 2:
				x=0.4*((2.0+cos(u/2.0)*sin(v)-sin(u/2.0)*sin(2.0*v))*cos(u));
				y=0.4*((2.0+cos(u/2.0)*sin(v)-sin(u/2.0)*sin(2.0*v))*sin(u));
				z=0.4*(sin(u/2.0)*sin(v)+cos(u/2.0)*sin(2.0*v));
				break;
			default:x=0.0;y=0.0;z=0.0;
		}
	};
	double len() const {return sqrt(sqr(x)+sqr(y)+sqr(z));}
	void norm() {double l=len();x/=l;y/=l;z/=l;}
	double len2() const {return (sqr(x)+sqr(y)+sqr(z));}
	Point operator+ (Point a) {return Point(x+a.x,y+a.y,z+a.z);}
	Point& operator+= (Point a) {x+=a.x;y+=a.y;z+=a.z;return (*this);}
	Point operator- (Point a) {return Point(x-a.x,y-a.y,z-a.z);}
	Point operator* (double c) {return Point(x*c,y*c,z*c);}
	double operator* (Point a) {return x*a.x+y*a.y+z*a.z;}
	Point operator^ (Point a) {return Point(y*a.z-z*a.y,z*a.x-x*a.z,x*a.y-y*a.x);}
};

int scr_sz_x=1024;
int scr_sz_y=768;
GLuint texture=0;
int wireframe=0;
int figure=1;

//mebius
int m_num=0;
int m_n2;
Point m_p[MAX];
Point m_n[MAX];
int m_f[MAX][4];
Point m_fn[MAX];
Point m_ft[MAX][4];

//klein1
int c1_num=0;
int c1_n1,c1_n2;
Point c1_p[MAX];
Point c1_fn[MAX];
Point c1_ft[MAX][4];
Point c1_n[MAX];
int c1_f[MAX][4];

//klein2
int c2_num=0;
int c2_n1,c2_n2;
Point c2_p[MAX];
Point c2_fn[MAX];
Point c2_ft[MAX][4];
Point c2_n[MAX];
int c2_f[MAX][4];

double dist[MAX];

Point _cam;
typedef double Matrix33[3][3];
Matrix33 cam;

void Mult(Matrix33 &m) {
	Matrix33 a;
	for(int i=0;i<3;i++) for (int j=0;j<3;j++) {
		a[i][j]=0.0;
		for(int k=0;k<3;k++)
			a[i][j]+=cam[i][k]*m[k][j];
	}
	for(int i=0;i<3;i++) for (int j=0;j<3;j++) {
		cam[i][j]=a[i][j];
	}
}

int dist_cmp(const void *a,const void *b) {
	int i=*((int*)a),j=*((int*)b);;
	if (dist[i]>dist[j])
		return -1;
	else
		return 1;
}

void mebius_gen(int N1,int N2) {
	double one=(double)N2/(double)(N2-1);
	m_num=0;
	m_n2=N2;
	for (int i=0;i<N1;i++) {
		double u=2.0*pi*i/N1;
		double u1=2.0*pi*(i+1)/N1;
		for(int j=0;j<2*N2;j++) {
			double v,v1;
			if (j>N2) 
				v=one-(2*one*(j-N2)/N2);
			else
				v=(2*one*j/N2)-one;
			if (j+1>N2) 
				v1=one-(2*one*(j+1-N2)/N2);
			else
				v1=(2*one*(j+1)/N2)-one;
			Point &res=m_p[m_num];
			res=Point(0,u,v);
			m_f[m_num][0]=m_num;
			m_f[m_num][1]=m_num+1;
			if ((m_f[m_num][1])%(N2+N2)==0)
				m_f[m_num][1]-=N2+N2;
			m_f[m_num][3]=m_num+(N2+N2);
			m_f[m_num][2]=m_f[m_num][1]+(N2+N2);
			if (m_f[m_num][2]>=2*N1*N2) {
				m_f[m_num][2]-=2*N1*N2;
				m_f[m_num][2]+=N2;
				m_f[m_num][2]%=(N2+N2);
			}
			if (m_f[m_num][3]>=2*N1*N2) {
				m_f[m_num][3]-=2*N1*N2;
				m_f[m_num][3]+=N2;
				m_f[m_num][3]%=(N2+N2);
			}
			double cx=1.0/pi;
			double cy=0.5;
			double sd=0.5;
			m_ft[m_num][0]=Point(cx*u,sd+cy*v,0);
			m_ft[m_num][1]=Point(cx*u,sd+cy*v1,0);
			m_ft[m_num][2]=Point(cx*u1,sd+cy*v1,0);
			m_ft[m_num][3]=Point(cx*u1,sd+cy*v,0);
			m_num++;
		}
	}
	for(int i=0;i<m_num;i++) {
		for(int j=0;j<4;j++) {
			Point &n=m_fn[i];
			n=(m_p[m_f[i][j]]-m_p[m_f[i][(j+3)%4]])^(m_p[m_f[i][(j+1)%4]]-m_p[m_f[i][j]]);
			n.norm();
			if (m_f[i][j]%(N2+N2)||j==0||j==3)
				m_n[m_f[i][j]]+=n;
		}
	}
	for(int i=0;i<m_num;i++) {
		m_n[i].norm();
	}
}

void mebius() {
	int per[MAX];
	for(int i=0;i<m_num;i++) {
		per[i]=i;
		Point sum;
		for(int k=0;k<4;k++)
			sum+=m_p[m_f[i][k]];
		sum=(sum*0.25)-_cam;
		dist[i]=sum.len2();
	}
	qsort(per,m_num,sizeof(int),dist_cmp);	
	glEnable(GL_CULL_FACE);
	glBegin(GL_QUADS);
	for(int i=0;i<m_num;i++) if (per[i]%m_n2!=0&&per[i]%m_n2!=m_n2-1){
		Point &a=m_p[m_f[per[i]][0]];
		Point &b=m_p[m_f[per[i]][1]];
		Point &c=m_p[m_f[per[i]][2]];
		Point &d=m_p[m_f[per[i]][3]];
		Point &an=m_n[m_f[per[i]][0]];
		Point &bn=m_n[m_f[per[i]][1]];
		Point &cn=m_n[m_f[per[i]][2]];
		Point &dn=m_n[m_f[per[i]][3]];
		Point &at=m_ft[per[i]][0];
		Point &bt=m_ft[per[i]][1];
		Point &ct=m_ft[per[i]][2];
		Point &dt=m_ft[per[i]][3];
		
		glTexCoord2d(at.x,at.y);
		glNormal3d(an.x,an.y,an.z);
		glVertex3d(a.x,a.y,a.z);
		glTexCoord2d(bt.x,bt.y);
		glNormal3d(bn.x,bn.y,bn.z);
		glVertex3d(b.x,b.y,b.z);
		glTexCoord2d(ct.x,ct.y);
		glNormal3d(cn.x,cn.y,cn.z);
		glVertex3d(c.x,c.y,c.z);
		glTexCoord2d(dt.x,dt.y);
		glNormal3d(dn.x,dn.y,dn.z);
		glVertex3d(d.x,d.y,d.z);
	}
	glEnd();
	glDisable(GL_CULL_FACE);
}

void klein1_gen(int N1,int N2) {
	c1_n1=N1;
	c1_n2=N2;
	for(int i=0;i<=N1+1;i++) {
		double u=2.0*pi*i/N1;
		double u1=2.0*pi*(i+1)/N1;
		for(int j=0;j<N2;j++) {
			double v=2.0*pi*j/N2;
			double v1=2.0*pi*(j+1)/N2;
			Point &res=c1_p[c1_num];
			res=Point(1,u,v);
			c1_f[c1_num][0]=c1_num;
			c1_f[c1_num][1]=c1_num+1;
			if ((c1_f[c1_num][1])%N2==0)
				c1_f[c1_num][1]-=N2;
			c1_f[c1_num][3]=c1_num+N2;
			c1_f[c1_num][2]=c1_f[c1_num][1]+N2;
			double cx=1.0/pi;
			double cy=1.0/pi;
			double sd=0.0;
			c1_ft[c1_num][0]=Point(cx*u,sd+cy*v,0);
			c1_ft[c1_num][1]=Point(cx*u,sd+cy*v1,0);
			c1_ft[c1_num][2]=Point(cx*u1,sd+cy*v1,0);
			c1_ft[c1_num][3]=Point(cx*u1,sd+cy*v,0);
			c1_num++;
		}
	}
	for(int i=0;i<c1_num;i++) {
		for(int j=0;j<4;j++) {
			Point &n=c1_fn[i];
			n=(c1_p[c1_f[i][j]]-c1_p[c1_f[i][(j+3)%4]])^(c1_p[c1_f[i][(j+1)%4]]-c1_p[c1_f[i][j]]);
			n.norm();
			c1_n[c1_f[i][j]]+=n;
		}
	}
	for(int i=0;i<c1_num;i++) {
		c1_n[i].norm();
	}
}

void klein1() {
	int per[MAX];
	for(int i=0;i<c1_num;i++) {
		per[i]=i;
		Point sum;
		for(int k=0;k<4;k++)
			sum+=c1_p[c1_f[i][k]];
		sum=(sum*0.25)-_cam;
		dist[i]=sum.len2();
	}
	qsort(per,c1_num,sizeof(int),dist_cmp);	
	glBegin(GL_QUADS);
	for(int i=0;i<c1_num;i++) if (per[i]<c1_n1*c1_n2) {
		Point &a=c1_p[c1_f[per[i]][0]];
		Point &b=c1_p[c1_f[per[i]][1]];
		Point &c=c1_p[c1_f[per[i]][2]];
		Point &d=c1_p[c1_f[per[i]][3]];
		Point &an=c1_n[c1_f[per[i]][0]];
		Point &bn=c1_n[c1_f[per[i]][1]];
		Point &cn=c1_n[c1_f[per[i]][2]];
		Point &dn=c1_n[c1_f[per[i]][3]];
		Point &at=c1_ft[per[i]][0];
		Point &bt=c1_ft[per[i]][1];
		Point &ct=c1_ft[per[i]][2];
		Point &dt=c1_ft[per[i]][3];
		
		glTexCoord2d(at.x,at.y);
		glNormal3d(an.x,an.y,an.z);
		glVertex3d(a.x,a.y,a.z);
		glTexCoord2d(bt.x,bt.y);
		glNormal3d(bn.x,bn.y,bn.z);
		glVertex3d(b.x,b.y,b.z);
		glTexCoord2d(ct.x,ct.y);
		glNormal3d(cn.x,cn.y,cn.z);
		glVertex3d(c.x,c.y,c.z);
		glTexCoord2d(dt.x,dt.y);
		glNormal3d(dn.x,dn.y,dn.z);
		glVertex3d(d.x,d.y,d.z);
	}
	glEnd();
}


void klein2_gen(int N1,int N2) {
	c2_n1=N1;
	c2_n2=N2;
	for(int i=0;i<=N1+1;i++) {
		double u=2.0*pi*i/N1;
		double u1=2.0*pi*(i+1)/N1;
		for(int j=0;j<N2;j++) {
			double v=2.0*pi*j/N2;
			double v1=2.0*pi*(j+1)/N2;
			Point &res=c2_p[c2_num];
			res=Point(2,u,v);
			c2_f[c2_num][0]=c2_num;
			c2_f[c2_num][1]=c2_num+1;
			if ((c2_f[c2_num][1])%N2==0)
				c2_f[c2_num][1]-=N2;
			c2_f[c2_num][3]=c2_num+N2;
			c2_f[c2_num][2]=c2_f[c2_num][1]+N2;
			double cx=1.0/pi;
			double cy=1.0/pi;
			double sd=0.0;
			c2_ft[c2_num][0]=Point(cx*u,sd+cy*v,0);
			c2_ft[c2_num][1]=Point(cx*u,sd+cy*v1,0);
			c2_ft[c2_num][2]=Point(cx*u1,sd+cy*v1,0);
			c2_ft[c2_num][3]=Point(cx*u1,sd+cy*v,0);
			c2_num++;
		}
	}
	for(int i=0;i<c2_num;i++) {
		for(int j=0;j<4;j++) {
			Point &n=c2_fn[i];
			n=(c2_p[c2_f[i][j]]-c2_p[c2_f[i][(j+3)%4]])^(c2_p[c2_f[i][(j+1)%4]]-c2_p[c2_f[i][j]]);
			n.norm();
			c2_n[c2_f[i][j]]+=n;
		}
	}
	for(int i=0;i<c2_num;i++) {
		c2_n[i].norm();
	}
}

void klein2() {
	int per[MAX];
	for(int i=0;i<c2_num;i++) {
		per[i]=i;
		Point sum;
		for(int k=0;k<4;k++)
			sum+=c2_p[c2_f[i][k]];
		sum=(sum*0.25)-_cam;
		dist[i]=sum.len2();
	}
	qsort(per,c2_num,sizeof(int),dist_cmp);	
	glBegin(GL_QUADS);
	for(int i=0;i<c2_num;i++) if (per[i]<c2_n1*c2_n2) {
		Point &a=c2_p[c2_f[per[i]][0]];
		Point &b=c2_p[c2_f[per[i]][1]];
		Point &c=c2_p[c2_f[per[i]][2]];
		Point &d=c2_p[c2_f[per[i]][3]];
		Point &an=c2_n[c2_f[per[i]][0]];
		Point &bn=c2_n[c2_f[per[i]][1]];
		Point &cn=c2_n[c2_f[per[i]][2]];
		Point &dn=c2_n[c2_f[per[i]][3]];
		Point &at=c2_ft[per[i]][0];
		Point &bt=c2_ft[per[i]][1];
		Point &ct=c2_ft[per[i]][2];
		Point &dt=c2_ft[per[i]][3];
		
		glTexCoord2d(at.x,at.y);
		glNormal3d(an.x,an.y,an.z);
		glVertex3d(a.x,a.y,a.z);
		glTexCoord2d(bt.x,bt.y);
		glNormal3d(bn.x,bn.y,bn.z);
		glVertex3d(b.x,b.y,b.z);
		glTexCoord2d(ct.x,ct.y);
		glNormal3d(cn.x,cn.y,cn.z);
		glVertex3d(c.x,c.y,c.z);
		glTexCoord2d(dt.x,dt.y);
		glNormal3d(dn.x,dn.y,dn.z);
		glVertex3d(d.x,d.y,d.z);
	}
	glEnd();
}

void draw_figure() {
	switch (figure) {
		case 1: mebius(); break;
		case 2: klein1(); break;
		case 3: klein2(); break;
	}
}

void display() {
	_cam.x=CAM_R*cam[0][1];
	_cam.y=CAM_R*cam[1][1];
	_cam.z=CAM_R*cam[2][1];
	
	glClear(GL_DEPTH_BUFFER_BIT|GL_COLOR_BUFFER_BIT);
	glEnable(GL_DEPTH_TEST);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	
	glTranslated(0.0,0.0,-CAM_R);
	glRotated(-90.0,1.0,0.0,0.0);
	
	GLfloat l_diffuse[4] = { 1.0f, 1.0f, 1.0f, 1.0f };
	GLfloat l_specular[4] = { 1.0f, 1.0f, 1.0f, 1.0f };
	GLfloat l_ambient[4] = { 1.0f, 1.0f, 1.0f, 1.0f };
	glLightfv(GL_LIGHT0,GL_DIFFUSE,l_diffuse);
	glLightfv(GL_LIGHT0,GL_SPECULAR,l_specular);
	glLightfv(GL_LIGHT0,GL_AMBIENT,l_ambient);
	unsigned int time=GetTickCount();
	double lix=1.0*cos(0.001*time);
	double liy=0.4*sin(0.001*time);
	GLfloat lpos[4]={lix,liy,2.0,1.0};
	glLightfv(GL_LIGHT0,GL_POSITION,lpos);
	glEnable(GL_LIGHT0);

	glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
	glColor3d(1.0,1.0,1.0);
	glEnable(GL_POINT_SMOOTH);
	glPointSize(20.0);

	glBegin(GL_POINTS);
	glVertex3d(lpos[0],lpos[1],lpos[2]);
	glEnd();
	
	glColor3d(0.5,0.0,0.7);
	glBegin(GL_QUADS);
	glVertex3d(-10.0,5.0,-8.0);
	glVertex3d(-10.0,5.0,8.0);
	glVertex3d(10.0,5.0,8.0);
	glVertex3d(10.0,5.0,-8.0);
	glEnd();
	
	glLoadIdentity();
	gluLookAt(_cam.x,_cam.y,_cam.z,0.0,0.0,0.0,cam[0][2],cam[1][2],cam[2][2]);
	glEnable(GL_LIGHTING);
		
	GLfloat m_emissive[4] = { 0.0f, 0.0f, 0.0f, 1.0f };
	GLfloat m_diffuse[4] = { 0.4f, 0.4f, 0.4f, 0.5f };
	GLfloat m_specular[4] = { 0.6f, 0.6f, 0.6f, 1.0f };
	GLfloat m_ambient[4] = { 0.1f, 0.1f, 0.1f, 1.0f };
	glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, m_emissive);
	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, m_diffuse);
	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, m_specular);
	glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, m_ambient);
	glMaterialf(GL_FRONT_AND_BACK,GL_SHININESS,50);
		
	glEnable(GL_POLYGON_OFFSET_FILL);
	glPolygonOffset(3,3);
	if (!wireframe) {
		glEnable(GL_BLEND);
		glEnable(GL_TEXTURE_2D);
	}
	draw_figure();
	if (!wireframe) {
		glDisable(GL_TEXTURE_2D);
		glDisable(GL_BLEND);
	}
	glDisable(GL_POLYGON_OFFSET_FILL);
	glDisable(GL_LIGHTING);
	if (wireframe) {
		glColor3d(0.0,0.0,0.0);
		glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
		draw_figure();
	}
	glFlush();
	glutSwapBuffers();
}

void idle() {
	glutPostRedisplay();
}

void reshape(int w, int h) {
	scr_sz_x=w;
	scr_sz_y=h;
	glutWarpPointer(scr_sz_x/2,scr_sz_y/2);
	glViewport(0,0,w,h);
	if(h > 0) {
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		gluPerspective(90.0,w/(GLdouble)h,0.1,50);
	}
}

void keypress(unsigned char key,int x,int y) {
	switch (key) {
		case 27://Escape button
			glutDestroyWindow(glutGetWindow());
			exit(0);
			break;
		case 'w':
		case 'W':
			wireframe=1-wireframe;
			break;
		case '1':
		case '2':
		case '3':
			figure=key-'0';
			break;
	}
}

void init() {
	glClearColor(0.0,0.0,0.0,0.0);
	glShadeModel(GL_SMOOTH);
	glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
	glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);
	glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
	glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER,GL_TRUE);
	glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,GL_TRUE);
	
	for(int i=0;i<3;i++) for (int j=0;j<3;j++) cam[i][j]=0.0;
	cam[0][0]=cam[1][1]=-(cam[2][2]=1.0);
	
	mebius_gen(150,20);
	klein1_gen(150,50);
	klein2_gen(150,50);
	
	IMAGE img;
	if (LoadBMP("pattern.bmp",&img)) {
		glGenTextures(1,&texture);
		glBindTexture(GL_TEXTURE_2D,texture);
		glPixelStorei(GL_UNPACK_ALIGNMENT,1);
		gluBuild2DMipmaps(GL_TEXTURE_2D,3,img.width,img.height,GL_RGB,GL_UNSIGNED_BYTE,img.data);
		free(img.data);
		
		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT);
		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_REPEAT);
		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_LINEAR);
	}
}

void mousemove(int x,int y) {
	int dx,dy;
	dx=x-scr_sz_x/2;
	dy=y-scr_sz_y/2;
	if (dx||dy) {
		double phi=dx*0.003;
		double ksi=-dy*0.003;
		
		Matrix33 m;
		m[0][0]=m[1][1]=cos(phi);
		m[0][1]=-(m[1][0]=sin(phi));
		m[2][0]=m[0][2]=m[1][2]=m[2][1]=0.0;
		m[2][2]=1.0;
		Mult(m);
		m[2][2]=m[1][1]=cos(ksi);
		m[1][2]=-(m[2][1]=sin(ksi));
		m[0][1]=m[1][0]=m[2][0]=m[0][2]=0.0;
		m[0][0]=1.0;
		Mult(m);
		
		glutWarpPointer(scr_sz_x/2,scr_sz_y/2);
	}
}

int main(int argc,char** argv) {

	glutInit(&argc,argv);
	glutInitDisplayMode(GLUT_DOUBLE|GLUT_DEPTH|GLUT_RGB|GLUT_ALPHA);
	glutInitWindowSize(scr_sz_x,scr_sz_y);
	glutCreateWindow("Task4 - OpenGL");
	
	glutKeyboardFunc(keypress);
	glutPassiveMotionFunc(mousemove);
	glutMotionFunc(mousemove);
	glutDisplayFunc(display);
	glutReshapeFunc(reshape);
	glutIdleFunc(idle);
	
	glutFullScreen();
	init();
	
	glutSetCursor(GLUT_CURSOR_NONE);
	glutWarpPointer(scr_sz_x/2,scr_sz_y/2);
	
	glutMainLoop();
	return 0;
}
