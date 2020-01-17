package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * @author hujiahao
 */

@Data
@ApiModel(description="稿件VO-审稿人-简版")
public class Post4RevSimpleVO {
    @ApiModelProperty("标识号")
    private Integer id;

    @ApiModelProperty("文章编号")
    private String no;

    @ApiModelProperty("标题")
    private String title;

    @ApiModelProperty("领域")
    private Integer field;

    @ApiModelProperty("状态")
    private Integer status;
}
