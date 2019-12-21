package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ApiModel(description = "投稿人用户返回模型")
public class UserContributorVoGet {
    /**
     * 标识
     */
    @ApiModelProperty("标识")
    private Integer id;

    /**
     * 用户名
     */
    @ApiModelProperty("用户名")
    private String userName;

    /**
     * 电子邮件
     */
    @ApiModelProperty("电子邮件")
    private String email;

    /**
     * 性别
     */
    @ApiModelProperty("性别")
    private Integer sex;

    /**
     * 通讯地址
     */
    @ApiModelProperty("通讯地址")
    private String address;

    /**
     * 邮编
     */
    @ApiModelProperty("邮编")
    private String zipCode;

    /**
     * 工作单位
     */
    @ApiModelProperty("工作单位")
    private String employer;

    /**
     * 专业
     */
    @ApiModelProperty("专业")
    private  String major;

    /**
     * 学历
     */
    @ApiModelProperty("学历")
    private String education;

    /**
     * 职称
     */
    @ApiModelProperty("职称")
    private String title;

    /**
     * 电话
     */
    @ApiModelProperty("电话")
    private String phone;

    /**
     * 状态(-1注销，0未激活，1激活)
     */
    @ApiModelProperty("状态")
    private Integer active;
}
