package com.samsph.pjcm.web.controller;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;


@Controller
@Api(tags = "页面管理")
public class HtmlController {
    @ApiOperation(value = "首页")
    @GetMapping(value = "/")
    public String LoginHtml(){
        return "index";
    }

    @ApiOperation(value = "用户注销")
    @GetMapping(value = "/logout")
    public String Logout(){
        return "redirect:/";
    }
}
