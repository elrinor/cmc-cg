varying vec3 normal;
varying vec3 lightDir;
varying vec3 position;

varying float upTextureK;

varying vec2 texCoord;

void main() {
	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;

	lightDir = gl_LightSource[0].position.xyz - gl_Vertex.xyz;
	normal = gl_Normal;
	position = gl_Vertex.xyz;
	
	texCoord = gl_Vertex.xy / 6;
	upTextureK = clamp(gl_Vertex.z - 0.5, 0.0, 1.0);
}