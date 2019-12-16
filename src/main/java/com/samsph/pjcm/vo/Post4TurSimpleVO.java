package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * @author hujiahao
 */

@Data
@ApiModel(description="稿件VO-游客-简版")
public class Post4TurSimpleVO {
    @ApiModelProperty("标识号")
    private Integer id;

    @ApiModelProperty("标题")
    private String title;
}
