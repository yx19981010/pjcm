package com.samsph.pjcm.controller;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.logout.SecurityContextLogoutHandler;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;


@Controller
@Api(tags = "页面管理")
public class HtmlController {
    @ApiOperation(value = "首页")
    @GetMapping(value = "/")
    public String LoginHtml(){
        return "index";
    }

    @ApiOperation(value = "用户注销")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN','ROLE_EDITOR','ROLE_REVIEWER','ROLE_CONTRIBUTOR')")
    @GetMapping(value = "/auth/logout")
    public String Logout(HttpServletRequest request, HttpServletResponse response){
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if(auth != null){
            new SecurityContextLogoutHandler().logout(request,response,auth);
        }
        return "redirect:/";
    }
}
