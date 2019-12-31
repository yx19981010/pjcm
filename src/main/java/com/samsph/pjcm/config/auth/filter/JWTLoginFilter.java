package com.samsph.pjcm.config.auth.filter;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.exc.MismatchedInputException;
import com.fasterxml.jackson.databind.exc.UnrecognizedPropertyException;
import com.samsph.pjcm.config.auth.AccountLogin;
import com.samsph.pjcm.config.auth.JSONResult;
import com.samsph.pjcm.config.auth.SecurityConstants;
import com.samsph.pjcm.config.auth.UserLogined;
import com.samsph.pjcm.config.constant.RoleType;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.service.UserRoleService;
import com.samsph.pjcm.service.UserService;
import com.samsph.pjcm.config.utils.JwtTokenUtil;
import com.samsph.pjcm.config.utils.SpringUtil;
import org.springframework.context.ApplicationContext;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.web.authentication.AbstractAuthenticationProcessingFilter;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

public class JWTLoginFilter extends AbstractAuthenticationProcessingFilter {
    private ApplicationContext applicationContext = SpringUtil.getApplicationContext();
    private UserService userService = applicationContext.getBean(UserService.class);
    private UserRoleService userRoleService = applicationContext.getBean(UserRoleService.class);
    private static int role;
    private static String password;

    public JWTLoginFilter(String url, AuthenticationManager authManager) {
        super(new AntPathRequestMatcher(url));
        setAuthenticationManager(authManager);
    }

    @Override
    public Authentication attemptAuthentication(HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse) throws AuthenticationException, IOException,CustomException {
        // JSON反序列化成 AccountCredentials
        AccountLogin accountLogin;
        try {
             accountLogin = new ObjectMapper().readValue(httpServletRequest.getInputStream(), AccountLogin.class);
        }catch (JsonParseException | MismatchedInputException e){
            throw new BadCredentialsException("Json数据转换失败");
        }
//        HttpSession session = httpServletRequest.getSession();
//        System.out.println("vercode:"+session.getAttribute("verCode"));
//        if(!accountLogin.getCode().toLowerCase().equals("6666")) {
//            if (session.getAttribute("verCode") == null || !accountLogin.getCode().toLowerCase().equals(session.getAttribute("verCode")) ) {
//                throw new BadCredentialsException("验证码错误");
//            }
//        }
        List<SimpleGrantedAuthority> authorities = new ArrayList<>();
        role = accountLogin.getRole();
        password = accountLogin.getPassword();
        if(userService.findUserByEmail(accountLogin.getUsername()).isPresent()){
            if(!userRoleService.findUserHasRole(userService.findUserByEmail(accountLogin.getUsername()).get().getId(),role)){
                throw new BadCredentialsException("用户无此角色");
            }
            if(userService.findUserByEmail(accountLogin.getUsername()).get().getActive() != 1){
                throw new BadCredentialsException("用户未激活或已注销");
            }
        }else {
            throw new BadCredentialsException("邮箱错误~");
        }
        // 返回一个验证令牌
        switch (role){
            case RoleType.ADMIN_ROLE:
                authorities.add(new SimpleGrantedAuthority("ROLE_ADMIN"));
                break;
            case RoleType.EDITOR_ROLE:
                authorities.add(new SimpleGrantedAuthority("ROLE_EDITOR"));
                break;
            case RoleType.REVIEWER_ROLE:
                authorities.add(new SimpleGrantedAuthority("ROLE_REVIEWER"));
                break;
            case RoleType.CONTRIBUTOR_ROLE:
                authorities.add(new SimpleGrantedAuthority("ROLE_CONTRIBUTOR"));
                break;
        }
        return getAuthenticationManager().authenticate(
                new UsernamePasswordAuthenticationToken(
                        accountLogin.getUsername(),
                        accountLogin.getPassword(),
                        authorities
                )
        );
    }

    @Override
    protected void successfulAuthentication(HttpServletRequest req, HttpServletResponse res, FilterChain chain, Authentication auth) throws IOException, ServletException {
        String email = auth.getName();
        List<String> roles = auth.getAuthorities().stream().map(GrantedAuthority::getAuthority).collect(Collectors.toList());
        String token = JwtTokenUtil.createToken(email,password,roles);
        res.setHeader(SecurityConstants.TOKEN_HEADER,token);
        res.setContentType("application/json");
        res.setStatus(HttpServletResponse.SC_OK);
//        res.setHeader("Access-Control-Allow-Credentials", "true");
//        res.setHeader("Access-Control-Allow-Origin",req.getHeader("Origin"));
        res.getOutputStream().println(JSONResult.fillResultString(true,200,"ok",new UserLogined(userService.findUserByEmail(email).get().getId(),role,email)));
    }


    @Override
    protected void unsuccessfulAuthentication(HttpServletRequest request, HttpServletResponse response, AuthenticationException authenticationException) throws IOException {
        response.sendError(HttpServletResponse.SC_UNAUTHORIZED, authenticationException.getMessage());
    }
}
