package com.samsph.pjcm.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.util.Date;

/**
 * @author hujiahao
 */
@Data
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "user")
public class User {
    /**
     * 标识
     */
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    /**
     * 用户名
     */
    @Column(name="user_name")
    private String userName;

    /**
     * 电子邮件
     */
    @Column(name="email")
    private String email;

    /**
     * 密码哈希
     */
    @Column(name="password_hash")
    private String passwordHash;

    /**
     * 性别
     */
    @Column(name="sex")
    private Integer sex;

    /**
     * 通讯地址
     */
    @Column(name="address")
    private String address;

    /**
     * 邮编
     */
    @Column(name="zip_code")
    private String zipCode;

    /**
     * 工作单位
     */
    @Column(name="employer")
    private String employer;

    /**
     * 专业
     */
    @Column(name="major")
    private  Integer major;

    /**
     * 学历
     */
    @Column(name="education")
    private  Integer education;

    /**
     * 职称
     */
    @Column(name="title")
    private Integer title;

    /**
     * （审稿人）银行账号
     */
    @Column(name="bank_account")
    private String bankAccount;

    /**
     * （审稿人）银行名
     */
    @Column(name="bank_name")
    private String bankName;

    /**
     * 电话
     */
    @Column(name="phone")
    private String phone;

    /**
     * 创建时间
     */
    @Column(name="create_time")
    private Date createTime;

    /**
     * 状态(-1注销，0未激活，1激活)
     */
    @Column(name="active")
    private Integer active;

    /**
     *UUID生成一段数字代码
     */
    @Column(name="code")
    private String code;
}