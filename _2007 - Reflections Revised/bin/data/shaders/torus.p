varying vec3 normal;
varying vec3 lightDir;
varying vec3 position;

uniform sampler2D torusMap;
uniform vec3 camera;

void main() {
   // get texture color
   vec4 color = texture2D(torusMap, gl_TexCoord[0].st);
   
   // ambient
   vec4 k = vec4(0.5, 0.5, 0.5, 1.0);

   vec3 n = normalize(normal);
   vec3 ld = normalize(lightDir);
   float NdotDir = dot(n, ld);
   if(NdotDir > 0.0) {
      // diffuse
      color += 0.25 * gl_LightSource[0].diffuse * NdotDir;
      
      // specular
      vec3 reflectedLight = reflect(ld, n);
      float EyeDotLight = max(dot(normalize(position - camera), reflectedLight), 0.0);
      color += 0.25 * gl_LightSource[0].specular * pow(EyeDotLight, 4.0);
   }

   gl_FragColor = color;
}
