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

    public void sendHtmlMailForContributorActive(String to,String code) {
        String subject = "来自省医院编辑部的激活邮件";
        String content = "<html>" +
                "<body>" +
                "<p>尊敬的用户:</p>" +
                "<p>&nbsp;&nbsp;您好！</p>" +
                "<p>&nbsp;&nbsp;感谢您在我们的系统中注册</p>" +
                "<p>请点击下面的链接，进行激活。</p>" +
                "<a href=\""+ SecurityConstants.SERVICE_URL+"/api/v1/users/checkCode?code="+code+"\">省医院编辑部激活请点击</a>" +
                "<p>&nbsp;&nbsp;祝您生活愉快！万事如意！</p>" +
                "<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" +
                "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;《实用医院临床杂志》</p>" +
                "<body>" +
                "<html>";
        sendHtmlMail(to,subject,content);
    }

    @Override
    public void sendHtmlMailForSystemUserActive(String to, String code, String password) {
        String subject = "来自省医院编辑部的激活邮件";
        String content = "<html>" +
                "<body>" +
                "<p>尊敬的用户:</p>" +
                "<p>&nbsp;&nbsp;您好！</p>" +
                "<p>&nbsp;&nbsp;感谢您在我们的系统中注册</p>" +
                "<p>&nbsp;&nbsp;您的用户名："+to+"</p>" +
                "<p>&nbsp;&nbsp;您的密码："+password+"</p>" +
                "<p>&nbsp;&nbsp;请在登录系统后及时修改</p>" +
                "<p>请点击下面的链接，进行激活。</p>" +
                "<a href=\""+ SecurityConstants.SERVICE_URL+"/api/v1/users/checkCode?code="+code+"\">省医院编辑部激活请点击</a>" +
                "<p>&nbsp;&nbsp;祝您生活愉快！万事如意！</p>" +
                "<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" +
                "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;《实用医院临床杂志》</p>" +
                "<body>" +
                "<html>";
        sendHtmlMail(to,subject,content);
    }

    @Override
    public void sendHtmlMailForNewEmailActive(String to, String code, String password) {
        String subject = "来自省医院编辑部的激活邮件";
        String content = "<html>" +
                "<body>" +
                "<p>尊敬的用户:</p>" +
                "<p>&nbsp;&nbsp;您好！</p>" +
                "<p>&nbsp;&nbsp;您的新用户名："+to+"</p>" +
                "<p>&nbsp;&nbsp;您的新密码："+password+"</p>" +
                "<p>&nbsp;&nbsp;请在登录系统后及时修改</p>" +
                "<p>请点击下面的链接，进行激活。</p>" +
                "<a href=\""+ SecurityConstants.SERVICE_URL+"/api/v1/users/checkCode?code="+code+"\">省医院编辑部激活请点击</a>" +
                "<p>&nbsp;&nbsp;祝您生活愉快！万事如意！</p>" +
                "<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" +
                "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;《实用医院临床杂志》</p>" +
                "<body>" +
                "<html>";
        sendHtmlMail(to,subject,content);
    }

    @Override
    public void sendHtmlMailForChangeEmail(String to, String code, String newEmail) {
        String subject = "来自省医院编辑部的邮箱更改通知";
        String content = "<html>" +
                "<body>" +
                "<p>尊敬的用户:</p>" +
                "<p>&nbsp;&nbsp;您好！</p>" +
                "<p>&nbsp;&nbsp;您希望修改您的邮箱为以下邮箱(如果不是本人行为请忽略)</p>" +
                "<p>&nbsp;&nbsp;"+newEmail+"</p>" +
                "<p>请点击下面的链接，进行确定。并在新邮箱中进行激活</p>" +
                "<a href=\""+ SecurityConstants.SERVICE_URL+"/api/v1/users/checkCodeForNewEmail?code="+code+"&email="+newEmail+"\">邮箱更改确定</a>" +
                "<p>&nbsp;&nbsp;祝您生活愉快！万事如意！</p>" +
                "<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" +
                "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;《实用医院临床杂志》</p>" +
                "<body>" +
                "<html>";
        sendHtmlMail(to,subject,content);

    }

    @Override
    public void sendHtmlMailForChangePassword(String to, String code, String newPassword) {
        String subject = "来自省医院编辑部的邮箱更改通知";
        String content = "<html>" +
                "<body>" +
                "<p>尊敬的用户:</p>" +
                "<p>&nbsp;&nbsp;您好！</p>" +
                "<p>&nbsp;&nbsp;您希望重置您的密码(如果不是本人行为请忽略)</p>" +
                "<p>&nbsp;&nbsp;新的密码："+newPassword+"</p>" +
                "<p>请点击下面的链接，进行确定。</p>" +
                "<a href=\""+ SecurityConstants.SERVICE_URL+"/api/v1/users/checkCodeForNewPassword?code="+code+newPassword+"\">密码更改确定</a>" +
                "<p>&nbsp;&nbsp;祝您生活愉快！万事如意！</p>" +
                "<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" +
                "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;《实用医院临床杂志》</p>" +
                "<body>" +
                "<html>";
        sendHtmlMail(to,subject,content);
    }

    public void sendHtmlMailForContributor(String to,String postName,String contributorName) {
        String subject = "来自省医院编辑部的稿件修改通知";
        String content = "<html>" +
                "<body>" +
                "<p>尊敬的"+contributorName+"作者:</p>" +
                "<p>&nbsp;&nbsp;您好！</p>" +
                "<p>&nbsp;&nbsp;您的稿件《"+postName+"》，缺少一些重要内容，不利于审稿。<br />" +
                "经审核，退回到您的“退改”稿件中，请再登录我刊在线投稿系统，<br />" +
                " 在“待修改稿件”中按要求修改并上传（具体步骤请参照首页公告栏“论文修改步骤”）。<br />" +
                " 请于3日内按要求修回，逾期未修回将视为退稿处理！</p>" +
                "<p>您可以点击下面的链接，登录网站进行修改。</p>" +
                "<a href=\""+ SecurityConstants.SERVICE_URL+"/test/1.jpg\">省医院编辑部</a>" +
                "<p>&nbsp;&nbsp;祝您生活愉快！万事如意！</p>" +
                "<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" +
                "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;《实用医院临床杂志》</p>" +
                "<body>" +
                "<html>";
        sendHtmlMail(to,subject,content);
    }

    public void sendHtmlMailForReviewer(String to,String postName,String reviewerName) {
        String subject = "来自省医院编辑部的稿件审阅通知";
        String content = "<html>" +
                "<body>" +
                "<p>尊敬的"+reviewerName+"专家:</p>" +
                "<p>&nbsp;&nbsp;您好！</p>" +
                "<p>&nbsp;&nbsp;鉴于您的学术成就，现奉上稿件一篇请您评审，如果您同意审理，建议您于10日内审<br />" +
                "回。若您因故不能审阅该稿，亦请推荐合适的审稿人评审或尽快退回原稿。此外，论文意见<br />" +
                " 请尽量具有针对性和可操作性，以便于作者修改。</p>" +
                "<p>稿件题目:"+postName+"</p>" +
                "<p>请您点击下面的链接，登录网站进行审阅。</p>" +
                "<a href=\""+ SecurityConstants.SERVICE_URL+"/test/1.jpg\">省医院编辑部</a>" +
                "<p>&nbsp;&nbsp;衷心感谢您对实用医院临床杂志工作的大力支持，本刊对审稿人姓名保密，并按规定付给审稿费。</p>" +
                "<p>&nbsp;&nbsp;此致</p>" +
                "<p>敬礼</p>" +
                "<p>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" +
                "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;《实用医院临床杂志》</p>" +
                "<body>" +
                "<html>";
        sendHtmlMail(to,subject,content);
    }
}
