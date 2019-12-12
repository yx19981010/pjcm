package com.samsph.pjcm.service;

public interface MailService {
    /**
     * 发送文本邮件
     * @param to
     * @param subject
     * @param content
     */
    //void sendSimpleMail(String to, String subject, String content);

    /**
     * 发送HTML邮件，方便用户点击附带的code用来验证激活账户
     * @param to
     * @param content
     */
    void sendHtmlMail(String to, String subject, String content);

    void sendHtmlMailForContributorActive(String to,String code);

    void sendHtmlMailForSystemUserActive(String to,String code,String password);

    void sendHtmlMailForChangeEmail(String to,String code,String newEmail);

    void sendHtmlMailForChangePassword(String to, String code, String newPassword);

    void sendHtmlMailForNewEmailActive(String to, String code, String password);

    void sendHtmlMailForContributor(String to,String postName,String contributorName);

    void sendHtmlMailForReviewer(String to,String postName,String reviewerName);
}
