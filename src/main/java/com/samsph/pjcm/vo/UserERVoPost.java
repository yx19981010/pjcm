package com.samsph.pjcm.vo;

import com.samsph.pjcm.config.constant.RoleType;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.*;

@ApiModel(description = "编辑、审稿人或者管理员用户添加模型")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class UserERVoPost {
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
     * 电话
     */
    @ApiModelProperty("电话")
    @Size(min = 1,max = 15)
    private String phone;
}
