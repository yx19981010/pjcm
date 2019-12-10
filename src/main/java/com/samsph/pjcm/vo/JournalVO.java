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
@EqualsAndHashCode(callSuper = true)
public class JournalVO extends JournalSimpleVO {
    @ApiModelProperty("期刊包含论文列表")
    private List<Post4TurSimpleVO> posts;

    @ApiModelProperty("包含论文数")
    private Integer total;
}
