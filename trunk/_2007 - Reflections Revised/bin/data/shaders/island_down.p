vec4 getIslandColor();

varying vec3 position;

void main() {
	if(position.z > 0.0)
		discard;
	
	gl_FragColor = getIslandColor();
}