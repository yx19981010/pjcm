package com.samsph.pjcm.config;


import com.samsph.pjcm.config.auth.CurrentUser;
import org.dozer.DozerBeanMapper;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * 注入一个DozerBeanMapper到上下文中
 *
 * @author hujiahao
 */
@Configuration
public class BeanConfigurer {
    @Bean
    public DozerBeanMapper mapper() {
        return new DozerBeanMapper();
    }

    @Bean
    public CurrentUser getCurrentUser() {
        return new CurrentUser();
    }
}
