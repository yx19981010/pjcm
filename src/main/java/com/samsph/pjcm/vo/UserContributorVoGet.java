package com.samsph.pjcm.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UserContributorVoGet {
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
     * 通讯地址
     */
    private String address;

    /**
     * 邮编
     */
    private String zipCode;

    /**
     * 工作单位
     */
    private String employer;

    /**
     * 专业
     */
    private  Integer major;

    /**
     * 学历
     */
    private Integer education;

    /**
     * 职称
     */
    private Integer title;

    /**
     * 电话
     */
    private String phone;

    /**
     * 状态(-1注销，0未激活，1激活)
     */
    private Integer active;
}
