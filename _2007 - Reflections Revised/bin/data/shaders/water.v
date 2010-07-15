uniform vec3 camera;
uniform float time;

varying vec4 position;

void main() {
   position = gl_Vertex;
   
   gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
   
   float t = time * 0.5;
   
   // gl_TexCoord[2] - 1st coord in normal texture
   gl_TexCoord[2].x = gl_Vertex.x / 20.0 + sin(t) * 0.2;
   gl_TexCoord[2].y = gl_Vertex.y / 20.0 - cos(t) * 0.2;
   gl_TexCoord[2].z = 0.0;
   gl_TexCoord[2].w = 1.0;
   
   // gl_TexCoord[3] - 2nd coord in normal texture
   gl_TexCoord[3].x = gl_Vertex.x / 20.0 - sin(t) * 0.2;
   gl_TexCoord[3].y = gl_Vertex.y / 20.0 + cos(t) * 0.2;
   gl_TexCoord[3].z = 0.0;
   gl_TexCoord[3].w = 1.0;
}