package com.samsph.pjcm.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UserReviewerVoGet {
    /**
     * 标识
     */
    private Integer id;

    /**
     * 用户名
     */
    private String userName;

    /**
     * 电子邮件
     */
    private String email;

    /**
     * 性别
     */
    private Integer sex;

    /**
     * 电话
     */
    private String phone;

    /**
     * 银行账号
     */
    private String bankAccount;

    /**
     * 银行名
     */
    private String bankName;
}
