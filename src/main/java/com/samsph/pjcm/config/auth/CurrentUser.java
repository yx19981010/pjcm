package com.samsph.pjcm.config.auth;

import com.samsph.pjcm.config.constant.RoleType;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.service.UserRoleService;
import com.samsph.pjcm.service.UserService;
import com.samsph.pjcm.config.utils.SpringUtil;
import org.springframework.context.ApplicationContext;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.List;
import java.util.stream.Collectors;


public class CurrentUser {
    private ApplicationContext applicationContext = SpringUtil.getApplicationContext();
    private UserService userService = applicationContext.getBean(UserService.class);

    public  UserLogined getCurrentUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication != null && authentication.getPrincipal() != null && !authentication.getPrincipal().equals("anonymousUser")) {
            String email = (String) authentication.getPrincipal();
            if(!userService.findUserByEmail(email).isPresent()){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"邮箱错误");
            }
            int userId = userService.findUserByEmail(email).get().getId();
            int role = 0;
            List<String> roles = authentication.getAuthorities().stream().map(GrantedAuthority::getAuthority).collect(Collectors.toList());
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
                            return null;
                    }
                }
                return new UserLogined(userId,role,email);
            }
        }
        return null;
    }
}
