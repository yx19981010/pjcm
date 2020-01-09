package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ApiModel(description = "所有投稿人领域返回模型")
public class ReviewerFieldVoGet{
    /**
     * 审稿人id
     */
    @ApiModelProperty("审稿人id")
    private Integer reviewerUid;

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
     * 领域
     */
    @ApiModelProperty("领域")
    private List<ReviewerFieldVoGetField> field;

    /**
     * 是否激活
     */
    @ApiModelProperty("激活状态")
    private Integer active;
}
