package com.samsph.pjcm.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.List;

/**
 * @author hujiahao
 */

@Data
@ApiModel(description = "期刊VO")
public class JournalVO{
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

    @ApiModelProperty("包含论文数")
    private Integer total;
}
