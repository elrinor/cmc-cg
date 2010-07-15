varying vec3 normal;
varying vec3 lightDir;
varying vec3 position;

void main() {	
	lightDir = gl_LightSource[0].position.xyz - gl_Vertex.xyz;
	normal = gl_Normal;
	position = gl_Vertex.xyz;
	
	gl_TexCoord[0] = gl_MultiTexCoord0;
	
	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
} 