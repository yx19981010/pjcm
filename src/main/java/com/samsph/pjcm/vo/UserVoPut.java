package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.*;


@Data
@AllArgsConstructor
@NoArgsConstructor
@ApiModel(description = "用户修改模型")
public class UserVoPut {
    /**
     * 用户id
     */
    @ApiModelProperty("用户id")
    @NotNull(message = "id不能为空")
    @Min(value = 1,message = "id必须为正整数")
    private Integer id;
    /**
     * 用户名
     */
    @ApiModelProperty("用户名")
    @Size(min = 1,max = 20)
    private String userName;

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
    private  String major;

    /**
     * 学历
     */
    @ApiModelProperty("学历")
    //由前端给出限制
    private  String education;

    /**
     * 职称
     */
    @ApiModelProperty("职称")
    //由前端给出限制
    private String title;

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
