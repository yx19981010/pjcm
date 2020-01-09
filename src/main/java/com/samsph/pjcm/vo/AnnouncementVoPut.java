package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

/**
 * 公告修改模型
 */
@Data
@ApiModel(description = "公告修改模型")
public class AnnouncementVoPut {
    /**
     * 公告id
     */
    @ApiModelProperty("公告id")
    @NotNull(message = "公告id不为空")
    @Min(value = 1,message = "公告id必须为正整数")
    private Integer id;
    /**
     * 公告标题
     */
    @ApiModelProperty("公告标题")
    @Size(min = 1,max = 50)
    private String title;
    /**
     * 公告描述
     */
    @ApiModelProperty("公告描述")
    @Size(min = 1,max = 300)
    private String content;
}
