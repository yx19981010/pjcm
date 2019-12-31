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
//        response.setHeader("Pragma", "No-cache");
//        response.setHeader("Cache-Control", "no-cache");
//        response.setDateHeader("Expires", 0);
//        response.setContentType("image/jpeg");
//        // 生成随机字串
//        String verifyCode = VerifyCodeUtils.generateVerifyCode(4);
//        // 存入会话session
//        HttpSession session = request.getSession(true);
//        System.out.println(session.getAttribute("codeTime"));
//        System.out.println(session.getAttribute("verCode"));
//        // 删除以前的
//        session.removeAttribute("verCode");
//        session.removeAttribute("codeTime");
//        session.setAttribute("verCode", verifyCode.toLowerCase());		//生成session
//        session.setAttribute("codeTime", LocalDateTime.now());
//        // 生成图片
//        int w = 100, h = 30;
//        OutputStream out = response.getOutputStream();
//        VerifyCodeUtils.outputImage(w, h, out, verifyCode);
        Map result = new HashMap();
//        response.setContentType("image/png");
//        response.setHeader("Cache-Control", "no-cache");
//        response.setHeader("Expire", "0");
//        response.setHeader("Pragma", "no-cache");
        VerifyCodeUtils validateCode = new VerifyCodeUtils();
        String[] temp = validateCode.getRandomCodeBase64();
        String code = temp[1];
        String base64String = temp[0];
        result.put("url", "data:image/png;base64," + base64String);
        result.put("code", code);
        return AjaxResponse.success(result);
    }
}
