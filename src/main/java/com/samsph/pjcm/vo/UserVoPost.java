package com.samsph.pjcm.vo;

import com.samsph.pjcm.config.constant.RoleType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.*;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class UserVoPost {
    /**
     * 用户名
     */
    @Size(min = 1,max = 13)
    private String userName;

    /**
     * 电子邮件
     */
    @Email
    @NotNull(message = "邮箱不能为空!!!")
    private String email;

    /**
     * 密码
     */
    @NotNull(message = "密码不能为空!!!")
    @Size(min = 8,max = 20)
    private String password;

    /**
     * 角色
     */
    @NotNull(message = "角色不能为空!!")
    @Min(value = RoleType.ADMIN_ROLE)
    @Max(value = RoleType.CONTRIBUTOR_ROLE)
    private Integer roleId;

    /**
     * 性别
     */
    @Min(value = 0)
    @Max(value = 1)
    private Integer sex;

    /**
     * 通讯地址
     */
    @Size(min = 1)
    private String address;

    /**
     * 邮编
     */
    @Size(min = 1)
    private String zipCode;

    /**
     * 工作单位
     */
    @Size(min = 1)
    private String employer;

    /**
     * 专业
     */
    //由前端给出限制
    private  Integer major;

    /**
     * 学历
     */
    //由前端给出限制
    private  Integer education;

    /**
     * 职称
     */
    //由前端给出限制
    private Integer title;

    /**
     * （审稿人）银行账号
     */
    @Size(min = 1,max = 50)
    private String bankAccount;

    /**
     * （审稿人）银行名
     */
    @Size(min = 1,max = 50)
    private String bankName;

    /**
     * 电话
     */
    @Size(min = 1,max = 15)
    private String phone;

}
