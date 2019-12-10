package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.service.UserService;
import com.samsph.pjcm.vo.UserVoPost;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.validation.Valid;

@RestController
@RequestMapping(value = "/auth")
public class AuthController {
    @Autowired
    private UserService userService;

    @PostMapping("/register")
    public AjaxResponse registerUser(@Valid @RequestBody UserVoPost userVoPost){
        userService.saveUser(userVoPost);
        return AjaxResponse.success();
    }
}
