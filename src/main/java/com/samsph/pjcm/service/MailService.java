package com.samsph.pjcm.service;

public interface MailService {

    /**
     * 发送HTML邮件
     * @param to 接收方邮箱
     * @param subject 主题
     * @param content 内容
     */
    void sendHtmlMail(String to, String subject, String content);

    /**
     * 给投稿人发送激活邮件
     * @param to 投稿人邮箱
     * @param code 激活码
     */
    void sendHtmlMailForContributorActive(String to,String code);

    /**
     * 给系统用户发送激活邮件
     * @param to 系统用户邮箱
     * @param code 激活码
     * @param password 账户密码
     */
    void sendHtmlMailForSystemUserActive(String to,String code,String password);

    /**
     * 给修改邮箱用户发送修改邮箱确定邮件
     * @param to 修改邮箱的用户的原邮箱
     * @param code 激活码
     * @param newEmail 新邮箱
     */
    void sendHtmlMailForChangeEmail(String to,String code,String newEmail);

    /**
     * 给忘记密码的用户发送修改密码确定邮件
     * @param to 忘记密码的用户邮箱
     * @param code 激活码
     * @param newPassword 新密码
     */
    void sendHtmlMailForChangePassword(String to, String code, String newPassword);

    /**
     * 给修改邮箱确定后的新邮箱发送激活邮件
     * @param to 修改后的新邮箱
     * @param code 激活码
     * @param password 新的密码
     */
    void sendHtmlMailForNewEmailActive(String to, String code, String password);

    /**
     * 给投稿人发送稿件修改通知
     * @param to 投稿人的邮箱
     * @param postName 稿件名称
     * @param contributorName 投稿人姓名
     */
    void sendHtmlMailForContributor(String to,String postName,String contributorName);

    /**
     * 给审稿人发送稿件审阅通知
     * @param to 审稿人的邮箱
     * @param postName 稿件名称
     * @param reviewerName 审稿人姓名
     */
    void sendHtmlMailForReviewer(String to,String postName,String reviewerName);
}
