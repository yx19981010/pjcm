package com.samsph.pjcm.config.auth.filter;


import com.samsph.pjcm.config.auth.SecurityConstants;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.config.utils.JwtTokenUtil;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.MalformedJwtException;
import io.jsonwebtoken.security.SignatureException;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;

import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

import org.springframework.security.web.authentication.www.BasicAuthenticationFilter;


import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.util.List;


public class JWTAuthenticationFilter extends BasicAuthenticationFilter {

    public JWTAuthenticationFilter(AuthenticationManager authenticationManager) {
        super(authenticationManager);
    }

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain chain) throws IOException, ServletException {
        JwtTokenUtil jwtTokenUtil = new JwtTokenUtil();
        String authorization = request.getHeader(SecurityConstants.TOKEN_HEADER);
        try {
            // 如果请求头中没有token信息则直接放行了
            if (authorization == null || !authorization.startsWith(SecurityConstants.TOKEN_PREFIX) || !jwtTokenUtil.validateTokenExpired(authorization)) {
                chain.doFilter(request, response);
                return;
            }
            // 如果请求头中有token，则进行解析，并且设置授权信息
            SecurityContextHolder.getContext().setAuthentication(getAuthentication(authorization));
            super.doFilterInternal(request, response, chain);
        }catch (ExpiredJwtException |SignatureException e){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,e.getMessage());
        }
    }

    /**
     * 获取用户认证信息 Authentication
     */
    private UsernamePasswordAuthenticationToken getAuthentication(String authorization) {
        String token = authorization.replace(SecurityConstants.TOKEN_PREFIX, "");
        try {
            String email = JwtTokenUtil.getEmailByToken(token);
            // 通过 token 获取用户具有的角色
            List<SimpleGrantedAuthority> userRolesByToken = JwtTokenUtil.getUserRolesByToken(token);
            if (!StringUtils.isEmpty(email)) {
                return new UsernamePasswordAuthenticationToken(email, null, userRolesByToken);
            }
        } catch (SignatureException | ExpiredJwtException | MalformedJwtException | IllegalArgumentException exception) {
            exception.printStackTrace();
        }
        return null;
    }

}

