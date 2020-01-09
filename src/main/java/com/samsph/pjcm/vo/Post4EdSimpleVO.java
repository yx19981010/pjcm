package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * @author hujiahao
 */

@Data
@ApiModel(description = "稿件VO-编辑-简版")
public class Post4EdSimpleVO {

    @ApiModelProperty("标识号")
    private Integer id;

    @ApiModelProperty("标题")
    private String title;

    @ApiModelProperty("一作")
    private String fAuName;

    @ApiModelProperty("稿件状态")
    private Integer status;

    @ApiModelProperty("投稿时间")
    private Date submitTime;
}
