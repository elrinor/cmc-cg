uniform sampler2D underwaterMap;
uniform sampler2D terrainMap;

varying vec3 normal;
varying vec3 lightDir;

varying float upTextureK;

varying vec2 texCoord;

vec4 getIslandColor() {
	// texture color
	vec4 color = (texture2D(underwaterMap, texCoord) * (1 - upTextureK) + texture2D(terrainMap,    texCoord) * upTextureK);
	
	// diffuse lighting
	float NdotDir = max(dot(normalize(normal), normalize(lightDir)), 0.0);
	color *= vec4(0.5, 0.5, 0.5, 0.5) + 0.5 * NdotDir * gl_LightSource[0].diffuse;
	
	return color;
}
