package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.utils.VerifyCodeUtils;
import com.samsph.pjcm.service.UserService;
import com.samsph.pjcm.vo.UserVoPost;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;
import java.io.IOException;
import java.io.OutputStream;
import java.time.LocalDateTime;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

@Api(tags = "认证管理")
@RestController
@RequestMapping(value = "/auth")
public class AuthController {
    @Autowired
    private UserService userService;

    @ApiOperation(value = "用户注册")
    @PostMapping("/register")
    public AjaxResponse registerUser(@Valid @RequestBody UserVoPost userVoPost){
        userService.saveUser(userVoPost);
        return AjaxResponse.success();
    }

    @ApiOperation(value = "生成验证码")
    @GetMapping("/yzm")
    public AjaxResponse yzm(){
        Map<String,String> result = new HashMap<>();
        String[] temp = VerifyCodeUtils.getRandomCodeBase64();
        String code = temp[1];
        String base64String = temp[0];
        result.put("url", "data:image/png;base64," + base64String);
        result.put("code", code);
        return AjaxResponse.success(result);
    }
}
