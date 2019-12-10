package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * @author hujiahao
 */

@Data
public class Post4TurSimpleVO {
    @ApiModelProperty("标识号")
    private Integer id;

    @ApiModelProperty("标题")
    private String title;
}
