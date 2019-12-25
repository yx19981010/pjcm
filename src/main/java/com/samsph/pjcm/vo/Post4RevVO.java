package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.persistence.Column;
import java.util.Date;

/**
 * @author hujiahao
 */

@Data
@EqualsAndHashCode(callSuper = true)
@ApiModel(description = "稿件VO-审稿人")
public class Post4RevVO  extends  Post4RevSimpleVO{
    @ApiModelProperty("稿件英文标题")
    private String titleEn;

    @ApiModelProperty("作者信息")
    private String writersInfo;

    @ApiModelProperty("体裁")
    private Integer genre;

    @ApiModelProperty("基金级别")
    private Integer fundLevel;

    @ApiModelProperty("中文关键词")
    private String keywordsZh;

    @ApiModelProperty("英文关键词")
    private String keywordsEn;

    @ApiModelProperty("中文摘要")
    private String abstractZh;

    @ApiModelProperty("英文摘要")
    private String abstractEn;

    @ApiModelProperty("稿件状态")
    private Integer status;

    @ApiModelProperty("稿件审阅总轮数")
    private Integer count;

    @ApiModelProperty("稿件上传时间")
    private Date postUploadTime;

    @ApiModelProperty("提交初审时间")
    private Date submitTime;
}
