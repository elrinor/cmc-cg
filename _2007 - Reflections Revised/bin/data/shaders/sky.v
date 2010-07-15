uniform float time;

uniform vec3 camera;

varying vec4 position;

void main() {
	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
	
	position = gl_Vertex -  vec4(camera, 1.0);
}
 