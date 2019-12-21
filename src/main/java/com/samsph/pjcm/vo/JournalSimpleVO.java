package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @author hujiahao
 */
@Data
@ApiModel(description = "期刊VO-间")
public class JournalSimpleVO {
    @ApiModelProperty("标识号")
    private Integer id;

    @ApiModelProperty("年号")
    private Integer year;

    @ApiModelProperty("月号")
    private Integer month;

    @ApiModelProperty("卷号")
    private Integer volume;

    @ApiModelProperty("期号")
    private Integer number;
}
