package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.utils.DozerUtil;
import com.samsph.pjcm.model.Announcement;
import com.samsph.pjcm.service.MailService;
import com.samsph.pjcm.vo.AnnouncementVoGet;
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
            mailService.sendHtmlMailForContributor("12345@test.com","论文1","杨玺");
//            mailService.sendHtmlMailForReviewer("554976107@qq.com","论文1","杨玺");
            return AjaxResponse.success();
    }
}
