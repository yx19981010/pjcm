package com.samsph.pjcm.config.auth.filter;


import com.samsph.pjcm.config.auth.JSONResult;
import com.samsph.pjcm.config.auth.SecurityConstants;
import com.samsph.pjcm.config.auth.UserLogined;
import com.samsph.pjcm.config.constant.RoleType;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.config.utils.JwtTokenUtil;
import com.samsph.pjcm.config.utils.SpringUtil;
import com.samsph.pjcm.service.UserRoleService;
import com.samsph.pjcm.service.UserService;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.MalformedJwtException;
import io.jsonwebtoken.security.SignatureException;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

import org.springframework.security.web.authentication.www.BasicAuthenticationFilter;


import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;


public class JWTAuthenticationFilter extends BasicAuthenticationFilter {

    public JWTAuthenticationFilter(AuthenticationManager authenticationManager) {
        super(authenticationManager);
    }

    private ApplicationContext applicationContext = SpringUtil.getApplicationContext();
    private UserService userService = applicationContext.getBean(UserService.class);
    private UserRoleService userRoleService = applicationContext.getBean(UserRoleService.class);

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
        }catch (SignatureException | ExpiredJwtException | MalformedJwtException | IllegalArgumentException e){
            response.setStatus(401);
            response.sendError(HttpServletResponse.SC_UNAUTHORIZED, e.getMessage());
        }
    }

    /**
     * 获取用户认证信息 Authentication
     */
    private UsernamePasswordAuthenticationToken getAuthentication(String authorization) {
        String token = authorization.replace(SecurityConstants.TOKEN_PREFIX, "");
        String email = JwtTokenUtil.getEmailByToken(token);
        if(!userService.findUserByEmail(email).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"用户邮箱已修改无效");
        }else{
            int role = 0;
            List<String> roles = JwtTokenUtil.getUserRolesByToken(token).stream().map(GrantedAuthority::getAuthority).collect(Collectors.toList());
            if(roles.size() == 1) {
                for (String role1 : roles) {
                    switch (role1) {
                        case "ROLE_ADMIN":
                            role = RoleType.ADMIN_ROLE;
                            break;
                        case "ROLE_EDITOR":
                            role = RoleType.EDITOR_ROLE;
                            break;
                        case "ROLE_REVIEWER":
                            role = RoleType.REVIEWER_ROLE;
                            break;
                        case "ROLE_CONTRIBUTOR":
                            role = RoleType.CONTRIBUTOR_ROLE;
                            break;
                        default:
                            throw new CustomException(CustomExceptionType.SYSTEM_ERROR,"系统错误");
                    }
                }
            }
            if(!userRoleService.findUserHasRole(userService.findUserByEmail(email).get().getId(),role)){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"用户角色已修改无效");
            }
        }
        // 通过 token 获取用户具有的角色
        List<SimpleGrantedAuthority> userRolesByToken = JwtTokenUtil.getUserRolesByToken(token);
        if (!StringUtils.isEmpty(email)) {
            return new UsernamePasswordAuthenticationToken(email, null, userRolesByToken);
        }
        return null;
    }
}

