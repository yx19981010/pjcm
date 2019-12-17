package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ApiModel(description = "某个领域下的审稿人返回模型")
public class ReviewerFieldVoGetReviewer {
    /**
     * 标识
     */
    @ApiModelProperty("审稿人领域标识")
    private Integer id;

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
}
