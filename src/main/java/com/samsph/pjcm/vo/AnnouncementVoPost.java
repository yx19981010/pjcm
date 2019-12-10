package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

/**
 * 公告添加模型
 */
@Data
@ApiModel(description = "公告添加模型")
public class AnnouncementVoPost {
    /**
     * 公告标题
     */
    @ApiModelProperty("公告标题")
    @NotNull(message = "公告标题不为空")
    @Size(min = 1,max = 50)
    private String title;
    /**
     * 公告描述
     */
    @ApiModelProperty("公告描述")
    @Size(min = 1,max = 100)
    @NotNull(message = "公告描述不为空")
    private String content;
}
