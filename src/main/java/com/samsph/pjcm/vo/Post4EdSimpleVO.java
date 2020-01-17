package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

/**
 * @author hujiahao
 */

@Data
@AllArgsConstructor
@NoArgsConstructor
@ApiModel(description = "稿件VO-编辑-简版")
public class Post4EdSimpleVO {

    @ApiModelProperty("标识号")
    private Integer id;

    @ApiModelProperty("文章编号")
    private String no;

    @ApiModelProperty("标题")
    private String title;

    @ApiModelProperty("投稿人姓名")
    private String contributorName;

    @ApiModelProperty("稿件状态")
    private Integer status;

    @ApiModelProperty("投稿时间")
    private Date submitTime;
}
