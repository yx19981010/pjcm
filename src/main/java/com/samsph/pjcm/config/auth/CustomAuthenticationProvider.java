package com.samsph.pjcm.config.auth;

import com.samsph.pjcm.model.User;
import com.samsph.pjcm.service.UserRoleService;
import com.samsph.pjcm.service.UserService;
import com.samsph.pjcm.config.utils.Sha256Util;
import com.samsph.pjcm.config.utils.SpringUtil;
import org.springframework.context.ApplicationContext;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import java.util.List;


// 自定义身份认证验证组件
public class CustomAuthenticationProvider implements AuthenticationProvider {
    private ApplicationContext applicationContext = SpringUtil.getApplicationContext();
    private UserService userService = applicationContext.getBean(UserService.class);

    @Override
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {
        // 获取认证的用户名 & 密码
        String email = authentication.getName();
        String password = authentication.getCredentials().toString();
        List<SimpleGrantedAuthority> authorities = (List<SimpleGrantedAuthority>) authentication.getAuthorities();
        User user = userService.findUserByEmail(email).get();
        if(user.getPasswordHash().equals(Sha256Util.getSHA256StrJava(password))){
            // 生成令牌
            return new UsernamePasswordAuthenticationToken(email, password, authorities);
        }else{
            throw new BadCredentialsException("密码错误~");
        }
    }

    // 是否可以提供输入类型的认证服务
    @Override
    public boolean supports(Class<?> authentication) {
        return authentication.equals(UsernamePasswordAuthenticationToken.class);
    }
}