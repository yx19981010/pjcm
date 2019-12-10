package com.samsph.pjcm.config.auth;

public class SecurityConstants {

    /**
     * 登录的地址
     */
    public static final String AUTH_LOGIN_URL = "/auth/login";

    /**
     * 服务器地址和端口号
     */
    public static final String SERVICE_URL = "http://192.168.0.102:8080";

    /**
     * 角色的key
     **/
    public static final String ROLE_CLAIMS = "rol";
    /**
     * rememberMe 为 false 的时候过期时间是1个小时
     */
    public static final long EXPIRATION = 60L * 60L;

    /**
     * JWT签名密钥硬编码到应用程序代码中，应该存放在环境变量或.properties文件中。
     */
    public static final String JWT_SECRET_KEY = "C*F-JaNdRgUkXn2r5u8x/A?D(G+KbPeShVmYq3s6v9y$B&E)H@McQfTjWnZr4u7w";

    // JWT token defaults

    public static final String TOKEN_HEADER = "Authorization";
    public static final String TOKEN_PREFIX = "Bearer";

    private SecurityConstants() {
    }
}
