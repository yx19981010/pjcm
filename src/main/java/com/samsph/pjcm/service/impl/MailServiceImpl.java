package com.samsph.pjcm.service.impl;

import com.samsph.pjcm.config.auth.SecurityConstants;
import com.samsph.pjcm.service.MailService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;

import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;

@Service
public class MailServiceImpl implements MailService {
    @Autowired(required = false)
    private JavaMailSender mailSender;
    @Value("${spring.mail.from}")
    private String from;
    @Override
    public void sendHtmlMail(String to, String subject, String content) {
        MimeMessage message = mailSender.createMimeMessage();
        MimeMessageHelper helper = null;
        try {
            helper = new MimeMessageHelper(message, true);
            helper.setFrom(from);
            helper.setSubject(subject);
            helper.setTo(to);
            helper.setText(content, true);
            mailSender.send(message);
        } catch (MessagingException e) {
            e.printStackTrace();
        }
    }

    public void sendHtmlMailForActive(String to,String code) {
        String subject = "来自省医院编辑部的激活邮件";
        String content = "<a href=\""+ SecurityConstants.SERVICE_URL+"/api/v1/users/checkCode?code="+code+"\">省医院编辑部激活请点击"+code+"</a>";
        sendHtmlMail(to,subject,content);
    }
}
