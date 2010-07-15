varying vec3 position;

void main() {
   float c = 1 / (1 - min(position.z, 0.0)); // (-inf, 0] -> [0, 1]
   
   gl_FragColor = vec4(c, c, c, 1.0);
}
