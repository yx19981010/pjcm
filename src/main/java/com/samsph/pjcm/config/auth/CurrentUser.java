package com.samsph.pjcm.config.auth;

import com.samsph.pjcm.service.UserService;
import com.samsph.pjcm.config.utils.SpringUtil;
import org.springframework.context.ApplicationContext;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;


public class CurrentUser {
    private ApplicationContext applicationContext = SpringUtil.getApplicationContext();
    private UserService userService = applicationContext.getBean(UserService.class);

    public UserLogined getCurrentUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.getPrincipal() != null) {
            String email = (String) authentication.getPrincipal();
            int userId = userService.findUserByEmail(email).get().getId();
            int role = -1;
            return new UserLogined(userId,role,email);
        }
        return null;
    }
}
