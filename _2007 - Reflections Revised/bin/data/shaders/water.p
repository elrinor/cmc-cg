uniform sampler2D normalMap;
uniform sampler2D reflectedMap;
uniform sampler2D refractedMap;
uniform sampler2D waterDepthMap;

uniform vec3 camera;

varying vec4 position;

void main() {

   // incoming ray direction
   vec3 dir = normalize(position.xyz - camera);

   // surface normal
   vec3 normal = normalize(vec3(texture2D(normalMap, gl_TexCoord[2].xy).xy - texture2D(normalMap, gl_TexCoord[3].xy).xy, 20.0));

   // reflected ray
   vec3 reflected = reflect(dir, normal);
   
   // reflected texcoord
   reflected.z = -reflected.z;
   vec4 reflectedTexCoord = gl_TextureMatrix[0] * vec4(camera + reflected, 1.0);

   // depth
   float depth = -1.0 + 1.0 / texture2DProj(waterDepthMap, gl_TextureMatrix[0] * position).x; // [0, 1] -> (inf, 0]

   // refraction coefficient
   float refractionK = clamp(1.0 - depth * 0.2, 0.8, 1.0);
   
   // refracted ray
   vec3 refracted = refract(dir, normal, refractionK);
   
   // refracted texcoord
   vec4 refractedTexCoord = gl_TextureMatrix[0] * vec4(camera + refracted, 1.0);
   
   // reflection / refraction k
   float k = -dot(normal, dir);
   k *= k;
   
   // depth coloring k
   float depthK = clamp(depth * 0.33, 0.0, 1.0);
   
   // reflected color
   reflectedTexCoord = vec4(reflectedTexCoord.xyz / reflectedTexCoord.w, 1.0);
   vec4 reflectedColor = 
     (texture2DProj(reflectedMap, reflectedTexCoord) + 
     texture2DProj(reflectedMap, reflectedTexCoord + vec4( 0.0015,  0.0015, 0.0, 0.0)) +
     texture2DProj(reflectedMap, reflectedTexCoord + vec4(-0.0015,  0.0015, 0.0, 0.0)) +
     texture2DProj(reflectedMap, reflectedTexCoord + vec4(-0.0015, -0.0015, 0.0, 0.0)) +
     texture2DProj(reflectedMap, reflectedTexCoord + vec4( 0.0015, -0.0015, 0.0, 0.0))) / 5;
   
   gl_FragColor = reflectedColor * (1 - k) + (texture2DProj(refractedMap, refractedTexCoord) * (1 - depthK) + vec4(0.1, 0.2, 0.4, 1.0) * depthK) * k;
}
