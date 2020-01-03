package com.samsph.pjcm.controller;

import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.service.MailService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


@RestController
@RequestMapping(value = "/test")
public class TestController {
    @Autowired
    private MailService mailService;

    @GetMapping("/sendEmail")
    public AjaxResponse sendEmail(){
//            mailService.sendHtmlMailForContributor("12345@test.com","论文1","杨玺");
            mailService.sendHtmlMailForReviewer("554976107@qq.com","论文1","杨玺");
            return AjaxResponse.success();
    }

    @GetMapping("/error")
    public AjaxResponse error(){
        Integer a = null;
        if(a.equals(1)){
            return AjaxResponse.success();
        }
        else{
            throw new CustomException(CustomExceptionType.SYSTEM_ERROR,"出错啦");
        }
    }
}
