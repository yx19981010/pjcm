package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @author hujiahao
 */
@Data
@ApiModel(description="稿件VO-投稿人-简版")
public class Post4CtrSimpleVO {
    @ApiModelProperty("标识号")
    private Integer id;

    @ApiModelProperty("标题")
    private String title;

    @ApiModelProperty("稿件状态")
    private Integer status;

    @ApiModelProperty("投稿时间")
    private Integer submitTime;
}
