package com.samsph.pjcm.config.utils;

import com.samsph.pjcm.config.auth.SecurityConstants;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.security.Keys;
import io.jsonwebtoken.SignatureAlgorithm;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Component;

import javax.crypto.SecretKey;
import javax.xml.bind.DatatypeConverter;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author shuang.kou
 */
@Component
public class JwtTokenUtil {


    /**
     * 生成足够的安全随机密钥，以适合符合规范的签名
     */
    private static byte[] apiKeySecretBytes = DatatypeConverter.parseBase64Binary(SecurityConstants.JWT_SECRET_KEY);
    private static SecretKey secretKey = Keys.hmacShaKeyFor(apiKeySecretBytes);

    public static String createToken(String email,String password,List<String> roles) {
        long expiration = SecurityConstants.EXPIRATION;

        String token = Jwts.builder()
                .signWith(secretKey, SignatureAlgorithm.HS256)
                .claim(SecurityConstants.ROLE_CLAIMS, String.join(",", roles))
                .setIssuedAt(new Date())
                .setSubject(email+";"+password)
                .setExpiration(new Date(System.currentTimeMillis() + expiration * 1000))
                .compact();
        return SecurityConstants.TOKEN_PREFIX + token;
    }

    private boolean isTokenExpired(String token) {
        Date expiredDate = getTokenBody(token).getExpiration();
        return expiredDate.before(new Date());
    }

    public Boolean validateTokenExpired(String authorization) {
        String token = authorization.replace(SecurityConstants.TOKEN_PREFIX, "");
        return !isTokenExpired(token);
    }

    public static String getEmailByToken(String token) {
        int i = getTokenBody(token).getSubject().indexOf(";");
        return getTokenBody(token).getSubject().substring(0,i);
    }

    public static String getPasswordByToken(String token) {
        int i = getTokenBody(token).getSubject().indexOf(";");
        return getTokenBody(token).getSubject().substring(i+1);
    }
    /**
     * 获取用户所有角色
     */
    public static List<SimpleGrantedAuthority> getUserRolesByToken(String token) {
        String role = (String) getTokenBody(token)
                .get(SecurityConstants.ROLE_CLAIMS);
        return Arrays.stream(role.split(","))
                .map(SimpleGrantedAuthority::new)
                .collect(Collectors.toList());
    }

    private static Claims getTokenBody(String token) {
        return Jwts.parser()
                .setSigningKey(secretKey)
                .parseClaimsJws(token)
                .getBody();
    }

}
