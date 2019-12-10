package com.samsph.pjcm.vo;

import com.samsph.pjcm.config.constant.RoleType;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.*;

@ApiModel(description = "用户添加模型")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class UserVoPost {
    /**
     * 用户名
     */
    @ApiModelProperty("用户名")
    @Size(min = 1,max = 20)
    private String userName;

    /**
     * 电子邮件
     */
    @ApiModelProperty("电子邮件")
    @Email
    @NotNull(message = "邮箱不能为空!!!")
    private String email;

    /**
     * 密码
     */
    @ApiModelProperty("密码")
    @NotNull(message = "密码不能为空!!!")
    @Size(min = 8,max = 20)
    private String password;

    /**
     * 角色
     */
    @ApiModelProperty("角色")
    @NotNull(message = "角色不能为空!!")
    @Min(value = RoleType.ADMIN_ROLE)
    @Max(value = RoleType.CONTRIBUTOR_ROLE)
    private Integer roleId;

    /**
     * 性别
     */
    @ApiModelProperty("性别")
    @Min(value = 0)
    @Max(value = 1)
    private Integer sex;

    /**
     * 通讯地址
     */
    @ApiModelProperty("通讯地址")
    @Size(min = 1)
    @Size(max = 100)
    private String address;

    /**
     * 邮编
     */
    @ApiModelProperty("邮编")
    @Size(min = 6)
    @Size(max = 6)
    private String zipCode;

    /**
     * 工作单位
     */
    @ApiModelProperty("工作单位")
    @Size(min = 1)
    @Size(max = 100)
    private String employer;

    /**
     * 专业
     */
    @ApiModelProperty("专业")
    //由前端给出限制
    private  Integer major;

    /**
     * 学历
     */
    @ApiModelProperty("学历")
    //由前端给出限制
    private  Integer education;

    /**
     * 职称
     */
    @ApiModelProperty("职称")
    //由前端给出限制
    private Integer title;

    /**
     * （审稿人）银行账号
     */
    @ApiModelProperty("（审稿人）银行账号")
    @Size(min = 1,max = 50)
    private String bankAccount;

    /**
     * （审稿人）银行名
     */
    @ApiModelProperty("（审稿人）银行名")
    @Size(min = 1,max = 50)
    private String bankName;

    /**
     * 电话
     */
    @ApiModelProperty("电话")
    @Size(min = 1,max = 15)
    private String phone;

}
