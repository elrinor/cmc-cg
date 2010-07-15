uniform sampler2D horizonMap;
uniform sampler2D cloudMap;

uniform float time;

varying vec4 position;

void main() {
   float upness = dot(vec3(0.0, 0.0, 1.0), normalize(position.xyz));
   vec2 horizonCoord = vec2(0.0, 0.85 + upness * 0.15);
   
   vec2 texPosition = position.xy / position.z;
   
   vec2 cloudCoord1 = vec2(texPosition.x / 10.0 + time / 100.0, texPosition.y / 10.0);
   vec2 cloudCoord2 = vec2(texPosition.x / 5.0 + time / 40.0, texPosition.y / 5.0);
   
   vec4 horizon = texture2D(horizonMap, horizonCoord);
   vec4 clouds0 = texture2D(cloudMap, cloudCoord1);
   vec4 clouds1 = texture2D(cloudMap, cloudCoord2);
   
   vec4 clouds = (clouds0 + clouds1) * upness;
   
   gl_FragColor = horizon * (1.0 - clouds.x) + clouds;
}
                                                                         
